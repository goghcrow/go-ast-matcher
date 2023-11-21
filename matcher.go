package astmatcher

import (
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"reflect"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

type (
	PackageID  = string
	PatternVar = string
	Binds      map[PatternVar]ast.Node
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ MatchOption ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type MatchFlags struct {
	loadAll           bool
	unparenExpr       bool
	matchCallEllipsis bool
	pkgFilter         func(pkg *packages.Package) bool
	fileFilter        func(filename string) bool
}

type MatchOption func(*MatchFlags)

// WithLoadAll 加载所有外部依赖包比较耗时, 如果需要引用外部类型进行判断需要加载
func WithLoadAll() MatchOption     { return func(opts *MatchFlags) { opts.loadAll = true } }
func WithUnparenExpr() MatchOption { return func(opts *MatchFlags) { opts.unparenExpr = true } }
func WithMatchCallEllipsis() MatchOption {
	return func(opts *MatchFlags) { opts.matchCallEllipsis = true }
}
func WithFilterPkg(f func(pkg *packages.Package) bool) MatchOption {
	return func(opts *MatchFlags) { opts.pkgFilter = f }
}
func WithFilterFile(f func(filename string) bool) MatchOption {
	return func(opts *MatchFlags) { opts.fileFilter = f }
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Matcher ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type Matcher struct {
	*MatchFlags

	// global
	FSet *token.FileSet
	Init []*packages.Package
	All  map[PackageID]*packages.Package

	// current package
	Pkg *packages.Package
	*types.Package
	*types.Info

	// current file
	Filename string

	// MatchFun Index, ref MkXXXPattern
	// index: (BadExpr|BadStmt|BadDecl).FromPos
	// ImportSpec.EndPos
	// Ident.NamePos
	// Field.Doc.List[0].Slash
	// BasicLit.ValuePos
	// FieldList.Opening
	// CallExpr.Lparen
	// FuncType.Func
	// BlockStmt.Lbrace
	funs []MatchFun
}

func NewMatcher(
	dir string,
	patterns []string,
	opts ...MatchOption,
) *Matcher {
	flags := &MatchFlags{}
	for _, opt := range opts {
		opt(flags)
	}

	fset, init, all := LoadDir(dir, patterns, flags.loadAll)
	return &Matcher{
		FSet:       fset,
		Init:       init,
		All:        all,
		MatchFlags: flags,
	}
}

func (m *Matcher) setPkg(pkg *packages.Package) {
	m.Pkg = pkg
	m.Info = pkg.TypesInfo
	m.Package = pkg.Types
}

func (m *Matcher) Walk(f func(m *Matcher, file *ast.File)) {
	for _, pkg := range m.Init {
		if m.pkgFilter == nil || m.pkgFilter(pkg) {
			m.setPkg(pkg)
			for i, filename := range pkg.CompiledGoFiles {
				m.Filename = filename
				if m.fileFilter == nil || m.fileFilter(filename) {
					f(m, pkg.Syntax[i])
				}
			}
		}
	}
}

func (m *Matcher) Match(pattern ast.Node, f Callback) {
	m.Walk(func(m *Matcher, file *ast.File) {
		m.MatchNode(pattern, file, f)
	})
}

// Callback
// 如果是 preorder, callback 可以返回 bool, 控制是否继续遍历子树
// 但是如果是 preorder 可能会错过 修改过的子树满足 pattern 的情况
// postorder 同样也有类型问题, 可能需要 working-list 之类方式处理
type Callback func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds)

type stackBuilder func(node ast.Node) []ast.Node

func (m *Matcher) MatchNode(pattern, node ast.Node, f Callback) {
	buildStack := m.mkStackBuilder(node)
	postOrder(node, func(c *astutil.Cursor) bool {
		n := c.Node()
		stack := buildStack(n)
		binds := map[PatternVar]ast.Node{}
		if m.match(pattern, n, stack, binds) {
			f(m, c, stack, binds)
		}
		return true
	})
}

func (m *Matcher) mkStackBuilder(root ast.Node) stackBuilder {
	parents := map[ast.Node]ast.Node{}
	postOrder(root, func(c *astutil.Cursor) bool {
		parents[c.Node()] = c.Parent()
		return true
	})

	return func(node ast.Node) []ast.Node {
		var stack []ast.Node
		for node != nil {
			stack = append(stack, node)
			node = parents[node]
		}
		return stack
	}
}

// x pattern, y node
// 整体 nil 判断的逻辑, 如果 pattern (x) 为 nil, 相当于 wildcard, 无条件返回 true
// 当 y 为 nil, 首先要先进行 matchFunc 的判断, 可能 matchFunc 需要匹配 nil 场景
// 最后, pattern 不为 nil, 但是 y 为 nil, 返回 false
func (m *Matcher) match(x, y ast.Node, stack []ast.Node, binds Binds) bool {
	if IsNilNode(x) {
		return true
	}

	if matchFun := m.TryGetNodeMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}

	if x, ok := x.(ast.Stmt); ok {
		if y, ok := y.(ast.Stmt); ok {
			return m.matchStmt(x, y, stack, binds)
		}
	}
	if x, ok := x.(ast.Expr); ok {
		if y, ok := y.(ast.Expr); ok {
			return m.matchExpr(x, y, stack, binds)
		}
	}
	if x, ok := x.(ast.Spec); ok {
		if y, ok := y.(ast.Spec); ok {
			return m.matchSpec(x, y, stack, binds)
		}
	}
	if x, ok := x.(ast.Decl); ok {
		if y, ok := y.(ast.Decl); ok {
			return m.matchDecl(x, y, stack, binds)
		}
	}

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}
	switch x := x.(type) {

	default:
		panic("unexpect Node: " + m.ShowNodeWithPos(x))

	// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Fields ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

	case *ast.Field:
		y := y.(*ast.Field)
		if matchFun := m.TryGetFieldMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if y == nil {
			return false
		}
		return m.matchIdents(x.Names, y.Names, stack, binds) &&
			m.matchExpr(x.Type, y.Type, stack, binds) &&
			m.matchExpr(x.Tag, y.Tag, stack, binds)

	case *ast.FieldList:
		y := y.(*ast.FieldList)
		if matchFun := m.TryGetFieldListMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if matchFun := m.TryGetFieldsMatchFun(x.List); matchFun != nil {
			if y == nil {
				return matchFun(m, nil, stack, binds)
			} else {
				return matchFun(m, FieldsNode(y.List), stack, binds)
			}
		}
		if y == nil {
			return false
		}
		if len(x.List) != len(y.List) {
			return false
		}
		for i := range x.List {
			if !m.match(x.List[i], y.List[i], stack, binds) {
				return false
			}
		}
		return true

	// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Comments ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
	case *ast.Comment:
		return true

	case *ast.CommentGroup:
		return true

	// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Files and packages ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
	case *ast.File:
		return true

	case *ast.Package:
		return true
	}

	return false
}

func (m *Matcher) matchSpec(x, y ast.Spec, stack []ast.Node, binds Binds) bool {
	if IsNilNode(x) {
		return true
	}

	if matchFun := m.TryGetSpecMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}

	switch x := x.(type) {

	default:
		panic("unexpect Spec: " + m.ShowNodeWithPos(x))

	case *ast.ImportSpec:
		y := y.(*ast.ImportSpec)
		if y == nil {
			return false
		}
		if !m.matchIdent(x.Name, y.Name, stack, binds) {
			return false
		}
		// import path 一定是 string literal
		// xp, _ := strconv.Unquote(x.Path.Value)
		// yp, _ := strconv.Unquote(y.Path.Value)
		// return xp == yp
		return m.matchBasicLit(x.Path, y.Path, stack, binds)

	case *ast.ValueSpec:
		y := y.(*ast.ValueSpec)
		if y == nil {
			return false
		}
		return m.matchIdents(x.Names, y.Names, stack, binds) &&
			m.matchExpr(x.Type, y.Type, stack, binds) &&
			m.matchExprs(x.Values, y.Values, stack, binds)

	case *ast.TypeSpec:
		y := y.(*ast.TypeSpec)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Name, y.Name, stack, binds) &&
			m.match(x.TypeParams, y.TypeParams, stack, binds) &&
			m.matchExpr(x.Type, y.Type, stack, binds)
	}
}

func (m *Matcher) matchDecl(x, y ast.Decl, stack []ast.Node, binds Binds) bool {
	if IsNilNode(x) {
		return true
	}

	if matchFun := m.TryGetDeclMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}

	switch x := x.(type) {
	default:
		panic("unexpect Decl: " + m.ShowNodeWithPos(x))

	case *ast.BadDecl:
		panic("unexpect BadDecl: " + m.ShowNodeWithPos(x))

	case *ast.GenDecl:
		y := y.(*ast.GenDecl)
		if y == nil {
			return false
		}
		return m.matchToken(x.Tok, y.Tok, stack, binds) &&
			m.matchSpecs(x.Specs, y.Specs, stack, binds)

	case *ast.FuncDecl:
		y := y.(*ast.FuncDecl)
		if y == nil {
			return false
		}
		return m.match(x.Recv, y.Recv, stack, binds) &&
			m.matchExpr(x.Name, y.Name, stack, binds) &&
			m.matchExpr(x.Type, y.Type, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds)
	}
}

func (m *Matcher) matchStmt(x, y ast.Stmt, stack []ast.Node, binds Binds) bool {
	if IsNilNode(x) {
		return true
	}

	if matchFun := m.TryGetStmtMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}

	switch x := x.(type) {

	default:
		panic("unexpect Stmt: " + m.ShowNodeWithPos(x))

	case *ast.BadStmt:
		panic("unexpect BadStmt: " + m.ShowNodeWithPos(x))

	case *ast.EmptyStmt:
		// 其实不需要, reflect 已经判断
		// y := y.(*ast.EmptyStmt)
		return true

	case *ast.DeclStmt:
		y := y.(*ast.DeclStmt)
		if y == nil {
			return false
		}
		return m.matchDecl(x.Decl, y.Decl, stack, binds)

	case *ast.LabeledStmt:
		y := y.(*ast.LabeledStmt)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Label, y.Label, stack, binds) &&
			m.matchStmt(x.Stmt, y.Stmt, stack, binds)

	case *ast.ExprStmt:
		y := y.(*ast.ExprStmt)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds)

	case *ast.SendStmt:
		y := y.(*ast.SendStmt)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Chan, y.Chan, stack, binds) &&
			m.matchExpr(x.Value, y.Value, stack, binds)

	case *ast.IncDecStmt:
		y := y.(*ast.IncDecStmt)
		if y == nil {
			return false
		}
		return m.matchToken(x.Tok, y.Tok, stack, binds) &&
			m.matchExpr(x.X, y.X, stack, binds)

	case *ast.AssignStmt:
		y := y.(*ast.AssignStmt)
		if y == nil {
			return false
		}
		return m.matchToken(x.Tok, y.Tok, stack, binds) &&
			m.matchExprs(x.Lhs, y.Lhs, stack, binds) &&
			m.matchExprs(x.Rhs, y.Rhs, stack, binds)

	case *ast.GoStmt:
		y := y.(*ast.GoStmt)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Call, y.Call, stack, binds)

	case *ast.DeferStmt:
		y := y.(*ast.DeferStmt)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Call, y.Call, stack, binds)

	case *ast.ReturnStmt:
		y := y.(*ast.ReturnStmt)
		if y == nil {
			return false
		}
		return m.matchExprs(x.Results, y.Results, stack, binds)

	case *ast.BranchStmt:
		y := y.(*ast.BranchStmt)
		if y == nil {
			return false
		}
		return m.matchToken(x.Tok, y.Tok, stack, binds) &&
			m.matchExpr(x.Label, y.Label, stack, binds)

	case *ast.BlockStmt:
		y := y.(*ast.BlockStmt)
		if matchFun := m.TryGetBlockStmtMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if y == nil {
			return false
		}
		return m.matchStmts(x.List, y.List, stack, binds)

	case *ast.IfStmt:
		y := y.(*ast.IfStmt)
		if y == nil {
			return false
		}
		return m.matchStmt(x.Init, y.Init, stack, binds) &&
			m.matchExpr(x.Cond, y.Cond, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds) &&
			m.matchStmt(x.Else, y.Else, stack, binds)

	case *ast.CaseClause:
		y := y.(*ast.CaseClause)
		if y == nil {
			return false
		}
		return m.matchExprs(x.List, y.List, stack, binds) &&
			m.matchStmts(x.Body, y.Body, stack, binds)

	case *ast.SwitchStmt:
		y := y.(*ast.SwitchStmt)
		if y == nil {
			return false
		}
		return m.matchStmt(x.Init, y.Init, stack, binds) &&
			m.matchExpr(x.Tag, y.Tag, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds)

	case *ast.TypeSwitchStmt:
		y := y.(*ast.TypeSwitchStmt)
		if y == nil {
			return false
		}
		return m.matchStmt(x.Init, y.Init, stack, binds) &&
			m.matchStmt(x.Assign, y.Assign, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds)

	case *ast.CommClause:
		y := y.(*ast.CommClause)
		if y == nil {
			return false
		}
		return m.matchStmt(x.Comm, y.Comm, stack, binds) &&
			m.matchStmts(x.Body, y.Body, stack, binds)

	case *ast.SelectStmt:
		y := y.(*ast.SelectStmt)
		if y == nil {
			return false
		}
		return m.matchStmt(x.Body, y.Body, stack, binds)

	case *ast.ForStmt:
		y := y.(*ast.ForStmt)
		if y == nil {
			return false
		}
		return m.matchStmt(x.Init, y.Init, stack, binds) &&
			m.matchExpr(x.Cond, y.Cond, stack, binds) &&
			m.matchStmt(x.Post, y.Post, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds)

	case *ast.RangeStmt:
		y := y.(*ast.RangeStmt)
		if y == nil {
			return false
		}
		return m.matchToken(x.Tok, y.Tok, stack, binds) &&
			m.matchExpr(x.Key, y.Key, stack, binds) &&
			m.matchExpr(x.Value, y.Value, stack, binds) &&
			m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds)
		return true
	}
}

func (m *Matcher) matchExpr(x, y ast.Expr, stack []ast.Node, binds Binds) bool {
	if IsNilNode(x) {
		return true
	}

	if m.unparenExpr {
		x = astutil.Unparen(x)
		y = astutil.Unparen(y)
	}

	if matchFun := m.TryGetExprMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}

	if reflect.TypeOf(x) != reflect.TypeOf(y) {
		return false
	}

	switch x := x.(type) {

	default:
		panic("unexpect Expr: " + m.ShowNodeWithPos(x))

	case *ast.BadExpr:
		panic("unexpect BadExpr: " + m.ShowNodeWithPos(x))

	case *ast.Ident:
		y := y.(*ast.Ident)
		if matchFun := m.TryGetIdentMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if y == nil {
			return false
		}
		return x.Name == y.Name

	case *ast.Ellipsis:
		y := y.(*ast.Ellipsis)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Elt, y.Elt, stack, binds)

	case *ast.BasicLit:
		y := y.(*ast.BasicLit)
		return m.matchBasicLit(x, y, stack, binds)
	case *ast.FuncLit:
		y := y.(*ast.FuncLit)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Type, y.Type, stack, binds) &&
			m.matchStmt(x.Body, y.Body, stack, binds)

	case *ast.CompositeLit:
		y := y.(*ast.CompositeLit)
		if y == nil {
			return false
		}
		return (x.Type == nil) == (y.Type == nil) &&
			m.matchExpr(x.Type, y.Type, stack, binds) &&
			m.matchExprs(x.Elts, y.Elts, stack, binds)

	case *ast.ParenExpr:
		y := y.(*ast.ParenExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds)

	case *ast.SelectorExpr:
		y := y.(*ast.SelectorExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchExpr(x.Sel, y.Sel, stack, binds)

	case *ast.IndexExpr:
		y := y.(*ast.IndexExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchExpr(x.Index, y.Index, stack, binds)

	case *ast.IndexListExpr:
		y := y.(*ast.IndexListExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchExprs(x.Indices, y.Indices, stack, binds)

	case *ast.SliceExpr:
		y := y.(*ast.SliceExpr)
		if y == nil {
			return false
		}
		return x.Slice3 == y.Slice3 &&
			m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchExpr(x.Low, y.Low, stack, binds) &&
			m.matchExpr(x.High, y.High, stack, binds) &&
			m.matchExpr(x.Max, y.Max, stack, binds)

	case *ast.TypeAssertExpr:
		y := y.(*ast.TypeAssertExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchExpr(x.Type, y.Type, stack, binds)

	case *ast.CallExpr:
		y := y.(*ast.CallExpr)
		if matchFun := m.TryGetCallExprMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if y == nil {
			return false
		}
		if m.matchCallEllipsis &&
			x.Ellipsis.IsValid() != y.Ellipsis.IsValid() {
			return false
		}
		return m.matchExpr(x.Fun, y.Fun, stack, binds) &&
			m.matchExprs(x.Args, y.Args, stack, binds)

	case *ast.StarExpr:
		y := y.(*ast.StarExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.X, y.X, stack, binds)

	case *ast.UnaryExpr:
		y := y.(*ast.UnaryExpr)
		if y == nil {
			return false
		}
		return m.matchToken(x.Op, y.Op, stack, binds) &&
			m.matchExpr(x.X, y.X, stack, binds)

	case *ast.BinaryExpr:
		y := y.(*ast.BinaryExpr)
		if y == nil {
			return false
		}
		return m.matchToken(x.Op, y.Op, stack, binds) &&
			m.matchExpr(x.X, y.X, stack, binds) &&
			m.matchExpr(x.Y, y.Y, stack, binds)

	case *ast.KeyValueExpr:
		y := y.(*ast.KeyValueExpr)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Key, y.Key, stack, binds) &&
			m.matchExpr(x.Value, y.Value, stack, binds)

	// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Types Exprs ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

	case *ast.ArrayType:
		y := y.(*ast.ArrayType)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Len, y.Len, stack, binds) &&
			m.matchExpr(x.Elt, y.Elt, stack, binds)

	case *ast.StructType:
		y := y.(*ast.StructType)
		if y == nil {
			return false
		}
		return m.match(x.Fields, y.Fields, stack, binds)

	case *ast.FuncType:
		y := y.(*ast.FuncType)
		if matchFun := m.TryGetFuncTypeMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if y == nil {
			return false
		}
		return m.match(x.TypeParams, y.TypeParams, stack, binds) &&
			m.match(x.Params, y.Params, stack, binds) &&
			m.match(x.Results, y.Results, stack, binds)

	case *ast.InterfaceType:
		y := y.(*ast.InterfaceType)
		if y == nil {
			return false
		}
		return m.match(x.Methods, y.Methods, stack, binds)

	case *ast.MapType:
		y := y.(*ast.MapType)
		if y == nil {
			return false
		}
		return m.matchExpr(x.Key, y.Key, stack, binds) &&
			m.matchExpr(x.Value, y.Value, stack, binds)

	case *ast.ChanType:
		y := y.(*ast.ChanType)
		if y == nil {
			return false
		}
		return x.Dir == y.Dir &&
			m.matchExpr(x.Value, y.Value, stack, binds)
	}
}

func (m *Matcher) matchIdent(x, y *ast.Ident, stack []ast.Node, binds Binds) bool {
	if x == nil {
		return true
	}
	if matchFun := m.TryGetIdentMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}
	if y == nil {
		return false
	}
	return x.Name == y.Name
}

func (m *Matcher) matchBasicLit(x, y *ast.BasicLit, stack []ast.Node, binds Binds) bool {
	if x == nil {
		return true
	}
	if matchFun := m.TryGetBasicLitMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}
	if y == nil {
		return false
	}
	// 注意: BasicLit 是原子 Pattern, 不支持展开匹配, 比如展开匹配 Kind
	// 且字面量需要用 constant.Compare 判断, 不能只看 Kind
	xVal := constant.MakeFromLiteral(x.Value, x.Kind, 0)
	yVal := constant.MakeFromLiteral(y.Value, y.Kind, 0)
	return constant.Compare(xVal, token.EQL, yVal)
}

func (m *Matcher) matchToken(x, y token.Token, stack []ast.Node, binds Binds) bool {
	if matchFun := m.TryGetTokenMatchFun(x); matchFun != nil {
		return matchFun(m, TokenNode(y), stack, binds)
	}
	return x == y
}

func (m *Matcher) matchStmts(xs, ys []ast.Stmt, stack []ast.Node, binds Binds) bool {
	if xs == nil {
		return true
	}
	if matchFun := m.TryGetStmtsMatchFun(xs); matchFun != nil {
		return matchFun(m, StmtsNode(ys), stack, binds)
	}
	if len(xs) != len(ys) {
		return false
	}
	for i := range xs {
		if !m.matchStmt(xs[i], ys[i], stack, binds) {
			return false
		}
	}
	return true
}

func (m *Matcher) matchExprs(xs, ys []ast.Expr, stack []ast.Node, binds Binds) bool {
	if xs == nil {
		return true
	}
	if matchFun := m.TryGetExprsMatchFun(xs); matchFun != nil {
		return matchFun(m, ExprsNode(ys), stack, binds)
	}
	if len(xs) != len(ys) {
		return false
	}
	for i := range xs {
		if !m.matchExpr(xs[i], ys[i], stack, binds) {
			return false
		}
	}
	return true
}

func (m *Matcher) matchIdents(xs, ys []*ast.Ident, stack []ast.Node, binds Binds) bool {
	if xs == nil {
		return true
	}
	if matchFun := m.TryGetIdentsMatchFun(xs); matchFun != nil {
		return matchFun(m, IdentsNode(ys), stack, binds)
	}
	if len(xs) != len(ys) {
		return false
	}
	for i := range xs {
		if !m.matchIdent(xs[i], ys[i], stack, binds) {
			return false
		}
	}
	return true
}

func (m *Matcher) matchSpecs(xs, ys []ast.Spec, stack []ast.Node, binds Binds) bool {
	if xs == nil {
		return true
	}
	if matchFun := m.TryGetSpecsMatchFun(xs); matchFun != nil {
		return matchFun(m, SpecsNode(ys), stack, binds)
	}
	if len(xs) != len(ys) {
		return false
	}
	for i := range xs {
		if !m.matchSpec(xs[i], ys[i], stack, binds) {
			return false
		}
	}
	return true
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ etc ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func (m *Matcher) ShowPos(n ast.Node) string {
	return ShowPos(m.FSet, n).String()
}

func (m *Matcher) ShowNode(n ast.Node) string {
	return ShowNode(m.FSet, n)
}

func (m *Matcher) ShowNodeWithPos(n ast.Node) string {
	return m.ShowNode(n) + "\nin\n" + m.ShowPos(n)
}

func (m *Matcher) WriteFile(filename string, f *ast.File) {
	WriteFile(m.FSet, filename, f)
}

func (m *Matcher) FormatFile(f *ast.File) string {
	return string(FormatFile(m.FSet, f))
}

func (m *Matcher) MustLookupType(qualified string) types.Type {
	obj := m.Lookup(qualified)
	assert(obj != nil, "type not found: "+qualified)
	return obj.Type()
}

// Lookup builtin | qualified ident
// e.g. "error", "string", "encoding/json.Marshal"
// 缓存下?
func (m *Matcher) Lookup(qualifiedName string) types.Object {
	idx := strings.LastIndex(qualifiedName, ".")
	if idx == -1 {
		return types.Universe.Lookup(qualifiedName)
	}
	pkg := qualifiedName[:idx]
	id := qualifiedName[idx+1:]
	p := m.All[pkg]
	if p == nil {
		return nil
	}
	return p.Types.Scope().Lookup(id)
}
