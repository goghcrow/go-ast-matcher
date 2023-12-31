package matcher

import (
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"reflect"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
	"golang.org/x/tools/go/types/typeutil"
)

type (
	PatternVar = string
	Binds      map[PatternVar]ast.Node
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ MatchOption ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type MatchFlags struct {
	LoadFlags

	unparenExpr       bool
	matchCallEllipsis bool
	pkgFilter         func(pkg *packages.Package) bool
	fileFilter        func(filename string, file *ast.File) bool
	genFilter         func(filename string, gen GeneratedBy, file *ast.File) bool
}

type MatchOption func(*MatchFlags)

// WithLoadDepts is slowly, but you can match external types
func WithLoadDepts() MatchOption { return func(opts *MatchFlags) { opts.LoadDepts = true } }

// WithBuildTag comma-separated list of extra build tags (see: go help buildconstraint)
func WithBuildTag(tag string) MatchOption  { return func(opts *MatchFlags) { opts.BuildTag = tag } }
func WithGopath(gopath string) MatchOption { return func(opts *MatchFlags) { opts.Gopath = gopath } }
func WithLoadTest() MatchOption            { return func(opts *MatchFlags) { opts.Test = true } }
func WithSuppressErrors() MatchOption      { return func(opts *MatchFlags) { opts.PrintErrors = false } }

func WithUnparenExpr() MatchOption { return func(opts *MatchFlags) { opts.unparenExpr = true } }
func WithCallEllipsisMatch() MatchOption {
	return func(opts *MatchFlags) { opts.matchCallEllipsis = true }
}
func WithPkgFilter(f func(pkg *packages.Package) bool) MatchOption {
	return func(opts *MatchFlags) { opts.pkgFilter = f }
}
func WithFileFilter(f func(filename string, file *ast.File) bool) MatchOption {
	return func(opts *MatchFlags) { opts.fileFilter = f }
}
func WithGenFilter(f func(filename string, gen GeneratedBy, file *ast.File) bool) MatchOption {
	return func(opts *MatchFlags) { opts.genFilter = f }
}
func WithSkipGenerated() MatchOption {
	return func(opts *MatchFlags) {
		opts.genFilter = func(filename string, gen GeneratedBy, file *ast.File) bool {
			return false
		}
	}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Matcher ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type Matcher struct {
	*MatchFlags

	*Loader

	// current package
	Pkg *packages.Package
	*types.Package
	*types.Info

	// current file
	File     *ast.File
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
	flags.PrintErrors = true
	for _, opt := range opts {
		opt(flags)
	}
	m := &Matcher{Loader: NewLoader(), MatchFlags: flags}
	m.Load(dir, patterns, flags.LoadFlags)
	return m
}

func (m *Matcher) setPkg(pkg *packages.Package) {
	m.Pkg = pkg
	m.Info = pkg.TypesInfo
	m.Package = pkg.Types
}

func (m *Matcher) setFile(filename string, file *ast.File) {
	m.Filename = filename
	m.File = file
}

func (m *Matcher) visitPkgFiles(pkg *packages.Package, f func(m *Matcher, file *ast.File)) {
	for i, filename := range pkg.CompiledGoFiles {
		file := pkg.Syntax[i]
		m.setFile(filename, file)

		genBy := m.Generated[filename]
		isGen := genBy != ""
		hasGenFilter := m.genFilter != nil
		if isGen && hasGenFilter {
			if !m.genFilter(filename, genBy, file) {
				continue
			}
		}

		if m.fileFilter == nil || m.fileFilter(filename, m.File) {
			f(m, m.File)
		}
	}
}

func (m *Matcher) visitPkgImports(
	pkg *packages.Package,
	f func(name *ast.Ident, path string),
) {
	m.visitPkgFiles(pkg, func(m *Matcher, file *ast.File) {
		for _, decl := range file.Decls {
			d, ok := decl.(*ast.GenDecl)
			if !ok || d.Tok != token.IMPORT {
				continue
			}
			for _, spec := range d.Specs {
				iSpec := spec.(*ast.ImportSpec)
				name := iSpec.Name
				path := trimPkgPath(iSpec.Path.Value)
				f(name, path)
			}
		}
	})
	return
}

// VisitAllFiles Walk all files in all init (defined by load pattern) packages
func (m *Matcher) VisitAllFiles(f func(m *Matcher, file *ast.File)) {
	for _, pkg := range m.Init {
		if m.pkgFilter == nil || m.pkgFilter(pkg) {
			m.setPkg(pkg)
			m.visitPkgFiles(pkg, f)
		}
	}
}

// VisitAllPackages in topological order
// walk files order by compiledGoFiles in same package
// walk imports order by declaration in one file
// deep first
func (m *Matcher) VisitAllPackages(
	pre func(*packages.Package) bool,
	post func(*packages.Package),
) {
	seen := map[*packages.Package]bool{}
	var visit func(*packages.Package)
	visit = func(pkg *packages.Package) {
		if seen[pkg] {
			return
		}
		seen[pkg] = true
		if pre == nil || pre(pkg) {
			m.visitPkgImports(pkg, func(name *ast.Ident, path string) {
				impt := m.All[path]
				if impt != nil {
					visit(impt)
				}
			})
		}
		if post != nil {
			post(pkg)
		}
	}
	for _, pkg := range m.Init {
		if m.pkgFilter == nil || m.pkgFilter(pkg) {
			m.setPkg(pkg)
			visit(pkg)
		}
	}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ node matcher ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// Match pattern in all packages
func (m *Matcher) Match(pattern ast.Node, f Callback) {
	m.VisitAllFiles(func(m *Matcher, file *ast.File) {
		m.MatchNode(pattern, file, f)
	})
}

// Callback
// If it is preorder, the callback can return bool to control whether to continue traversing the subtree
// But if it is preorder, it may miss the case where the modified subtree satisfies the pattern
// Postorder also has type problems, and may need to be handled with a working-list
type Callback func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds)

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

type stackBuilder func(node ast.Node) []ast.Node

// Matched when any subtree of rootNode matched pattern, return immediately
func (m *Matcher) Matched(pattern, rootNode ast.Node) (matched bool) {
	var abort = new(int)
	defer func() {
		if r := recover(); r != nil && r != abort {
			panic(r)
		}
	}()
	m.MatchNode(pattern, rootNode, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		matched = true
		panic(abort)
	})
	return matched
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

// x is pattern, y is node
// If pattern x is nil, it is equivalent to wildcard, and true is returned
// When y is nil, first call matchFunc, because nil case may need
// Finally, pattern is not nil, but y is nil, return false
func (m *Matcher) match(x, y ast.Node, stack []ast.Node, binds Binds) bool {
	isWildcard := IsNilNode(x)
	if isWildcard {
		return true
	}

	if matchFun := m.tryGetNodeMatchFun(x); matchFun != nil {
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
		if matchFun := m.tryGetFieldMatchFun(x); matchFun != nil {
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
		if matchFun := m.tryGetFieldListMatchFun(x); matchFun != nil {
			return matchFun(m, y, stack, binds)
		}
		if matchFun := m.tryGetFieldsMatchFun(x.List); matchFun != nil {
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
	isWildcard := IsNilNode(x)
	if isWildcard {
		return true
	}

	if matchFun := m.tryGetSpecMatchFun(x); matchFun != nil {
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
		// import path must be string literal
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
	isWildcard := IsNilNode(x)
	if isWildcard {
		return true
	}

	if matchFun := m.tryGetDeclMatchFun(x); matchFun != nil {
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
	isWildcard := IsNilNode(x)
	if isWildcard {
		return true
	}

	if matchFun := m.tryGetStmtMatchFun(x); matchFun != nil {
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
		// no need, checked in reflect.TypeOf
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
		if matchFun := m.tryGetBlockStmtMatchFun(x); matchFun != nil {
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
	isWildcard := IsNilNode(x)
	if isWildcard {
		return true
	}

	if m.unparenExpr {
		x = astutil.Unparen(x)
		y = astutil.Unparen(y)
	}

	if matchFun := m.tryGetExprMatchFun(x); matchFun != nil {
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
		if matchFun := m.tryGetIdentMatchFun(x); matchFun != nil {
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
		return m.matchExpr(x.Type, y.Type, stack, binds) &&
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
		if matchFun := m.tryGetCallExprMatchFun(x); matchFun != nil {
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
		if matchFun := m.tryGetFuncTypeMatchFun(x); matchFun != nil {
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
	isWildcard := x == nil
	if isWildcard {
		return true
	}
	if matchFun := m.tryGetIdentMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}
	if y == nil {
		return false
	}
	return x.Name == y.Name
}

func (m *Matcher) matchBasicLit(x, y *ast.BasicLit, stack []ast.Node, binds Binds) bool {
	isWildcard := x == nil
	if isWildcard {
		return true
	}
	if matchFun := m.tryGetBasicLitMatchFun(x); matchFun != nil {
		return matchFun(m, y, stack, binds)
	}
	if y == nil {
		return false
	}
	// Notice: BasicLit is atomic Pattern,
	// &ast.BasicLit{ Kind: token.INT } can be used for matching INT literal
	// because zero Value is ambiguous, wildcard or zero value ?
	xVal := constant.MakeFromLiteral(x.Value, x.Kind, 0)
	yVal := constant.MakeFromLiteral(y.Value, y.Kind, 0)
	return constant.Compare(xVal, token.EQL, yVal)
}

func (m *Matcher) matchToken(x, y token.Token, stack []ast.Node, binds Binds) bool {
	if matchFun := m.tryGetTokenMatchFun(x); matchFun != nil {
		return matchFun(m, TokenNode(y), stack, binds)
	}
	return x == y
}

func (m *Matcher) matchStmts(xs, ys []ast.Stmt, stack []ast.Node, binds Binds) bool {
	isWildcard := xs == nil
	if isWildcard {
		return true
	}
	if matchFun := m.tryGetStmtsMatchFun(xs); matchFun != nil {
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
	// notice: nil is wildcard pattern, but []ast.Expr{} exactly Matched empty ys
	isWildcard := xs == nil
	if isWildcard {
		return true
	}
	if matchFun := m.tryGetExprsMatchFun(xs); matchFun != nil {
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
	isWildcard := xs == nil
	if isWildcard {
		return true
	}
	if matchFun := m.tryGetIdentsMatchFun(xs); matchFun != nil {
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
	isWildcard := xs == nil
	if isWildcard {
		return true
	}
	if matchFun := m.tryGetSpecsMatchFun(xs); matchFun != nil {
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

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ lookup ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func (m *Matcher) LookupPackage(pkg string) *packages.Package {
	return m.All[pkg]
}

func (m *Matcher) MustLookupType(qualified string) types.Type {
	obj := m.Lookup(qualified)
	assert(obj != nil, "type not found: "+qualified)
	return obj.Type()
}

// Lookup builtin | qualified ident
// e.g. "error", "string", "encoding/json.Marshal"
// cached?
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

func (m *Matcher) MustLookup(qualifiedName string) types.Object {
	obj := m.Lookup(qualifiedName)
	assert(obj != nil, "object not found: "+qualifiedName)
	return obj
}

// Lookups name in current package and all imported packages
func (m *Matcher) Lookups(name string) []types.Object {
	pkg := m
	if o := pkg.Scope().Lookup(name); o != nil {
		return []types.Object{o}
	}

	var ret []types.Object
	for _, imp := range pkg.Imports() {
		if obj := imp.Scope().Lookup(name); obj != nil {
			ret = append(ret, obj)
		}
	}
	return ret
}

func (m *Matcher) LookupFieldOrMethod(pkg, typ, fieldOrMethod string) types.Object {
	qualified := pkg + "." + typ
	tyObj := m.Lookup(qualified)
	assert(tyObj != nil, qualified+" not found")

	p := m.All[pkg]
	assert(p != nil, pkg+" not found")
	obj, _, indirect := types.LookupFieldOrMethod(tyObj.Type(), false, p.Types, fieldOrMethod)
	if obj == nil && indirect {
		obj, _, _ = types.LookupFieldOrMethod(tyObj.Type(), true, p.Types, fieldOrMethod)
	}
	return obj
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ etc ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func (m *Matcher) ObjectOfCall(call *ast.CallExpr) types.Object {
	return typeutil.Callee(m.Info, call)
}

func (m *Matcher) Position(pos token.Pos) token.Position {
	return m.FSet.Position(pos)
}

func (m *Matcher) ShowPos(n ast.Node) string {
	return PosOfNode(m.FSet, n).String()
}

func (m *Matcher) ShowNode(n ast.Node) string {
	return ShowNode(m.FSet, n)
}

func (m *Matcher) ShowNodeWithPos(n ast.Node) string {
	return m.ShowNode(n) + "\nat " + m.ShowPos(n)
}

func (m *Matcher) WriteFile(filename string, f *ast.File) {
	WriteFile(m.FSet, filename, f)
}

func (m *Matcher) FormatFile(f *ast.File) string {
	return string(FmtFile(m.FSet, f))
}

func (m *Matcher) SortImports(
	f *ast.File,
	projectPkgPrefix string,
	companyPkgPrefix []string,
) {
	SortImports(f, projectPkgPrefix, companyPkgPrefix)
}
