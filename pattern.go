package matcher

import (
	"go/ast"
	"go/token"
	"strings"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Pattern ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// Notice myself: How to add new Pattern
// 0. Declare XXXPattern type, add to Pattern interface
// 1. Add MkXXXPattern && MkPattern
// 2. Add TryGetXXXMatchFun & TryGetMatchFun
// 3. Add MkXXXPatternVar & MkVar
// 4. Hook Matcher, call TryGetXXXMatchFun

// Notice: BasicLit is atomic Pattern, does not support expanding match
// e.g. matching BasicLit.Kind
// And to judge equivalence of  literal by constant.Compare, not just Kind

type (
	Pattern interface {
		NodePattern | StmtPattern | ExprPattern | DeclPattern | SpecPattern |
			IdentPattern | FieldPattern | FieldListPattern |
			CallExprPattern | FuncTypePattern | BlockStmtPattern | TokenPattern | BasicLitPattern |
			SlicePattern
	}
	TypingPattern interface {
		IdentPattern | ExprPattern
	}
	SlicePattern interface {
		StmtsPattern | ExprsPattern | SpecsPattern | IdentsPattern | FieldsPattern
	}
	ElemPattern interface {
		StmtPattern | ExprPattern | SpecPattern | IdentPattern | FieldPattern
	}

	NodePattern      = MatchFun
	StmtPattern      = *ast.BadStmt
	ExprPattern      = *ast.BadExpr
	DeclPattern      = *ast.BadDecl
	SpecPattern      = *ast.ImportSpec
	IdentPattern     = *ast.Ident
	FieldPattern     = *ast.Field
	FieldListPattern = *ast.FieldList
	CallExprPattern  = *ast.CallExpr
	FuncTypePattern  = *ast.FuncType
	BlockStmtPattern = *ast.BlockStmt
	BasicLitPattern  = *ast.BasicLit // for matching Field.Tag, Import.Path
	TokenPattern     = token.Token   // for matching token type
	StmtsPattern     = []ast.Stmt
	ExprsPattern     = []ast.Expr
	SpecsPattern     = []ast.Spec
	IdentsPattern    = []*ast.Ident // for matching Field.Name, etc
	FieldsPattern    = []*ast.Field
	// ChanDirPattern = ast.ChanDir // no need, just two values, use Or to match
	// StringPattern = string // maybe for expanding match Ident, Import.Path
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Factory ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func MkVar[T Pattern](m *Matcher, name string) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func PatternOf[T Pattern](m *Matcher, ptn ast.Node) T {
	assert(!IsPseudoNode(ptn), "invalid pattern")
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		return m.match(ptn, n, stack, binds)
	})
}

func MkPattern[T Pattern](m *Matcher, f MatchFun) T {
	var zero T
	switch any(zero).(type) {
	case NodePattern:
		return any(m.mkNodePattern(f)).(T)
	case StmtPattern:
		return any(m.mkStmtPattern(f)).(T)
	case ExprPattern:
		return any(m.mkExprPattern(f)).(T)
	case DeclPattern:
		return any(m.mkDeclPattern(f)).(T)
	case IdentPattern:
		return any(m.mkIdentPattern(f)).(T)
	case FieldPattern:
		return any(m.mkFieldPattern(f)).(T)
	case FieldListPattern:
		return any(m.mkFieldListPattern(f)).(T)
	case CallExprPattern:
		return any(m.mkCallExprPattern(f)).(T)
	case FuncTypePattern:
		return any(m.mkFuncTypePattern(f)).(T)
	case BlockStmtPattern:
		return any(m.mkBlockStmtPattern(f)).(T)
	case TokenPattern:
		return any(m.mkTokenPattern(f)).(T)
	case BasicLitPattern:
		return any(m.mkBasicLitPattern(f)).(T)
	case StmtsPattern:
		return any(m.mkStmtsPattern(f)).(T)
	case ExprsPattern:
		return any(m.mkExprsPattern(f)).(T)
	case SpecsPattern:
		return any(m.mkSpecsPattern(f)).(T)
	case IdentsPattern:
		return any(m.mkIdentsPattern(f)).(T)
	case FieldsPattern:
		return any(m.mkFieldsPattern(f)).(T)
	default:
		panic("unreachable")
	}
}

func IsPattern[T Pattern](m *Matcher, n any) bool {
	return TryGetMatchFun[T](m, n) != nil
}

// TryGetMatchFun
// if T is NodePattern, n must be ast.Node
// if T is StmtPattern, n must be ast.Stmt
// if T is ExprPattern, n must be ast.Expr
// if T is DeclPattern, n must be ast.Decl
// if T is SpecPattern, n must be ast.Spec
// if T is IdentPattern, n must be *ast.Ident
// if T is FieldPattern, n must be *ast.Field
// if T is FieldListPattern, n must be *ast.FieldList
// if T is CallExprPattern, n must be *ast.CallExpr
// if T is FuncTypePattern, n must be *ast.FuncType
// if T is BlockStmtPattern, n must be *ast.BlockStmt
// if T is TokenPattern, n must be token.Token
// if T is BasicLitPattern, n must be *ast.BasicLit
// if T is StmtsPattern, n must be []ast.Stmt
// if T is ExprsPattern, n must be []ast.Expr
// if T is SpecsPattern, n must be []ast.Spec
// if T is IdentsPattern, n must be []*ast.Ident
// if T is FieldsPattern, n must be []*ast.Field
func TryGetMatchFun[T Pattern](m *Matcher, n any) MatchFun {
	var zero T
	switch any(zero).(type) {
	case NodePattern:
		return m.tryGetNodeMatchFun(n.(ast.Node))
	case StmtPattern:
		return m.tryGetStmtMatchFun(n.(ast.Stmt))
	case ExprPattern:
		return m.tryGetExprMatchFun(n.(ast.Expr))
	case DeclPattern:
		return m.tryGetDeclMatchFun(n.(ast.Decl))
	case SpecPattern:
		return m.tryGetSpecMatchFun(n.(ast.Spec))
	case IdentPattern:
		return m.tryGetIdentMatchFun(n.(*ast.Ident))
	case FieldPattern:
		return m.tryGetFieldMatchFun(n.(*ast.Field))
	case FieldListPattern:
		return m.tryGetFieldListMatchFun(n.(*ast.FieldList))
	case CallExprPattern:
		return m.tryGetCallExprMatchFun(n.(*ast.CallExpr))
	case FuncTypePattern:
		return m.tryGetFuncTypeMatchFun(n.(*ast.FuncType))
	case BlockStmtPattern:
		return m.tryGetBlockStmtMatchFun(n.(*ast.BlockStmt))
	case TokenPattern:
		return m.tryGetTokenMatchFun(n.(token.Token))
	case BasicLitPattern:
		return m.tryGetBasicLitMatchFun(n.(*ast.BasicLit))
	case StmtsPattern:
		return m.tryGetStmtsMatchFun(n.([]ast.Stmt))
	case ExprsPattern:
		return m.tryGetExprsMatchFun(n.([]ast.Expr))
	case SpecsPattern:
		return m.tryGetSpecsMatchFun(n.([]ast.Spec))
	case IdentsPattern:
		return m.tryGetIdentsMatchFun(n.([]*ast.Ident))
	case FieldsPattern:
		return m.tryGetFieldsMatchFun(n.([]*ast.Field))
	default:
		panic("unreachable")
	}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ mkXXXPattern ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// MkNodePattern type of callback param node is ast.Node
func (m *Matcher) mkNodePattern(f MatchFun) NodePattern {
	return f
}

// MkStmtPattern type of callback param node is ast.Stmt
func (m *Matcher) mkStmtPattern(f MatchFun) StmtPattern {
	m.funs = append(m.funs, f)
	return &ast.BadStmt{From: token.Pos(-len(m.funs))}
}

// MkExprPattern type of callback param node is ast.Expr
func (m *Matcher) mkExprPattern(f MatchFun) ExprPattern {
	m.funs = append(m.funs, f)
	return &ast.BadExpr{From: token.Pos(-len(m.funs))}
}

// MkDeclPattern type of callback param node is ast.Decl
func (m *Matcher) mkDeclPattern(f MatchFun) DeclPattern {
	m.funs = append(m.funs, f)
	return &ast.BadDecl{From: token.Pos(-len(m.funs))}
}

// MkSpecPattern type of callback param node is ast.Spec
func (m *Matcher) mkSpecPattern(f MatchFun) SpecPattern {
	m.funs = append(m.funs, f)
	return &ast.ImportSpec{EndPos: token.Pos(-len(m.funs))}
}

// MkIdentPattern type of callback param node is *ast.Ident
func (m *Matcher) mkIdentPattern(f MatchFun) IdentPattern {
	m.funs = append(m.funs, f)
	return &ast.Ident{NamePos: token.Pos(-len(m.funs))}
}

// MkFieldPattern type of callback param node is *ast.Field
func (m *Matcher) mkFieldPattern(f MatchFun) FieldPattern {
	m.funs = append(m.funs, f)
	// Putting pos it in Type/Tag/Name will cause ambiguity
	// e.g. Field{ Type: MkExprPattern() }
	// e.g. Field{ Tag: MkBasicLitPattern() }
	// e.g. Field{ Name: MkIdentsPattern() }
	return &ast.Field{
		Doc: &ast.CommentGroup{
			List: []*ast.Comment{
				{Slash: token.Pos(-len(m.funs))},
				nil,
			},
		},
	}
}

// MkFieldListPattern type of callback param node is *ast.FieldList
func (m *Matcher) mkFieldListPattern(f MatchFun) FieldListPattern {
	m.funs = append(m.funs, f)
	return &ast.FieldList{Opening: token.Pos(-len(m.funs))}
}

// MkCallExprPattern type of callback param node is *ast.CallExpr
func (m *Matcher) mkCallExprPattern(f MatchFun) CallExprPattern {
	m.funs = append(m.funs, f)
	return &ast.CallExpr{Lparen: token.Pos(-len(m.funs))}
}

// MkFuncTypePattern type of callback param node is *ast.FuncType
func (m *Matcher) mkFuncTypePattern(f MatchFun) FuncTypePattern {
	m.funs = append(m.funs, f)
	return &ast.FuncType{Func: token.Pos(-len(m.funs))}
}

// MkBlockStmtPattern type of callback param node is *ast.BlockStmt
func (m *Matcher) mkBlockStmtPattern(f MatchFun) BlockStmtPattern {
	m.funs = append(m.funs, f)
	return &ast.BlockStmt{Lbrace: token.Pos(-len(m.funs))}
}

// MkTokenPattern type of callback param node is TokenNode
func (m *Matcher) mkTokenPattern(f MatchFun) TokenPattern {
	m.funs = append(m.funs, f)
	return token.Token(-len(m.funs))
}

// MkBasicLitPattern type of callback param node is *ast.BasicLit
func (m *Matcher) mkBasicLitPattern(f MatchFun) BasicLitPattern {
	m.funs = append(m.funs, f)
	return &ast.BasicLit{ValuePos: token.Pos(-len(m.funs))}
}

// []Pattern
// one more nil is for avoiding ambiguity
// e.g. []Expr{ XXXExprPattern }
// will be recognized as ExprsPattern, not ExprsPattern with only one element
// Normal grammar should not be []Node { , nil }

// MkStmtsPattern type of callback param node is StmtsNode
func (m *Matcher) mkStmtsPattern(f MatchFun) StmtsPattern {
	return []ast.Stmt{m.mkStmtPattern(f), nil}
}

// MkExprsPattern type of callback param node is ExprsNode
func (m *Matcher) mkExprsPattern(f MatchFun) ExprsPattern {
	return []ast.Expr{m.mkExprPattern(f), nil}
}

// MkSpecsPattern type of callback param node is SpecsNode
func (m *Matcher) mkSpecsPattern(f MatchFun) SpecsPattern {
	return []ast.Spec{m.mkSpecPattern(f), nil}
}

// MkIdentsPattern type of callback param node is IdentsNode
func (m *Matcher) mkIdentsPattern(f MatchFun) IdentsPattern {
	return []*ast.Ident{m.mkIdentPattern(f), nil}
}

// MkFieldsPattern type of callback param node is FieldsNode
func (m *Matcher) mkFieldsPattern(f MatchFun) FieldsPattern {
	return []*ast.Field{m.mkFieldPattern(f), nil}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ tryGetMatchFun ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func (m *Matcher) tryGetNodeMatchFun(n ast.Node) MatchFun {
	if x, ok := n.(NodePattern); ok {
		return x
	}
	return nil
}

func (m *Matcher) tryGetStmtMatchFun(n ast.Stmt) MatchFun {
	if x, _ := n.(StmtPattern); x != nil && x.From < 0 {
		return m.funs[-x.From-1]
	}
	return nil
}

func (m *Matcher) tryGetExprMatchFun(n ast.Expr) MatchFun {
	if x, _ := n.(ExprPattern); x != nil && x.From < 0 {
		return m.funs[-x.From-1]
	}
	return nil
}

func (m *Matcher) tryGetDeclMatchFun(n ast.Decl) MatchFun {
	if x, _ := n.(DeclPattern); x != nil && x.From < 0 {
		return m.funs[-x.From-1]
	}
	return nil
}

func (m *Matcher) tryGetSpecMatchFun(n ast.Spec) MatchFun {
	if x, _ := n.(*ast.ImportSpec); x != nil && x.EndPos < 0 {
		return m.funs[-x.EndPos-1]
	}
	return nil
}

func (m *Matcher) tryGetIdentMatchFun(x *ast.Ident) MatchFun {
	if x != nil && x.NamePos < 0 {
		return m.funs[-x.NamePos-1]
	}
	return nil
}

func (m *Matcher) tryGetFieldMatchFun(x *ast.Field) MatchFun {
	if x != nil && x.Doc != nil &&
		len(x.Doc.List) == 2 &&
		x.Doc.List[0].Slash < 0 &&
		x.Doc.List[1] == nil {
		return m.funs[-x.Doc.List[0].Slash-1]
	}
	return nil
}

func (m *Matcher) tryGetFieldListMatchFun(x *ast.FieldList) MatchFun {
	if x != nil && x.Opening < 0 {
		return m.funs[-x.Opening-1]
	}
	return nil
}

func (m *Matcher) tryGetCallExprMatchFun(x *ast.CallExpr) MatchFun {
	if x != nil && x.Lparen < 0 {
		return m.funs[-x.Lparen-1]
	}
	return nil
}

func (m *Matcher) tryGetFuncTypeMatchFun(x *ast.FuncType) MatchFun {
	if x != nil && x.Func < 0 {
		return m.funs[-x.Func-1]
	}
	return nil
}

func (m *Matcher) tryGetBlockStmtMatchFun(x *ast.BlockStmt) MatchFun {
	if x != nil && x.Lbrace < 0 {
		return m.funs[-x.Lbrace-1]
	}
	return nil
}

func (m *Matcher) tryGetTokenMatchFun(x token.Token) MatchFun {
	if x < 0 {
		return m.funs[-x-1]
	}
	return nil
}

func (m *Matcher) tryGetBasicLitMatchFun(x *ast.BasicLit) MatchFun {
	if x != nil && x.ValuePos < 0 {
		return m.funs[-x.ValuePos-1]
	}
	return nil
}

func (m *Matcher) tryGetStmtsMatchFun(xs []ast.Stmt) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.tryGetStmtMatchFun(xs[0])
}

func (m *Matcher) tryGetExprsMatchFun(xs []ast.Expr) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.tryGetExprMatchFun(xs[0])
}

func (m *Matcher) tryGetSpecsMatchFun(xs []ast.Spec) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.tryGetSpecMatchFun(xs[0])
}

func (m *Matcher) tryGetIdentsMatchFun(xs []*ast.Ident) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.tryGetIdentMatchFun(xs[0])
}

func (m *Matcher) tryGetFieldsMatchFun(xs []*ast.Field) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.tryGetFieldMatchFun(xs[0])
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Pseudo Node ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type PseudoNode interface {
	MatchFun | StmtsNode | ExprsNode | SpecsNode | IdentsNode | FieldsNode | TokenNode
}

type (
	MatchFun   func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool
	StmtsNode  []ast.Stmt   // for the callback param of StmtsPattern
	ExprsNode  []ast.Expr   // for the callback param of ExprsPattern
	SpecsNode  []ast.Spec   // for the callback param of SpecsPattern
	IdentsNode []*ast.Ident // for the callback param of IdentsPattern
	FieldsNode []*ast.Field // for the callback param of FieldsPattern
	TokenNode  token.Token  // for the callback param of TokenPattern
)

func IsPseudoNode(n ast.Node) bool {
	switch n.(type) {
	case MatchFun:
		return true
	case StmtsNode:
		return true
	case ExprsNode:
		return true
	case SpecsNode:
		return true
	case IdentsNode:
		return true
	case FieldsNode:
		return true
	case TokenNode:
		return true
	}
	return false
}

func showPseudoNode(fset *token.FileSet, n ast.Node) string {
	switch n := n.(type) {
	case MatchFun:
		return "match-fun"
	case StmtsNode:
		xs := make([]string, len(n))
		for i, it := range n {
			xs[i] = ShowNode(fset, it)
		}
		return strings.Join(xs, "\n")
	case ExprsNode:
		xs := make([]string, len(n))
		for i, it := range n {
			xs[i] = ShowNode(fset, it)
		}
		return strings.Join(xs, "\n")
	case SpecsNode:
		xs := make([]string, len(n))
		for i, it := range n {
			xs[i] = ShowNode(fset, it)
		}
		return strings.Join(xs, "\n")
	case IdentsNode:
		xs := make([]string, len(n))
		for i, it := range n {
			xs[i] = ShowNode(fset, it)
		}
		return strings.Join(xs, "\n")
	case FieldsNode:
		xs := make([]string, len(n))
		for i, it := range n {
			xs[i] = ShowNode(fset, it)
		}
		return strings.Join(xs, "\n")
	case TokenNode:
		return token.Token(n).String()
	default:
		panic("unknown pseudo node")
	}
}

func (MatchFun) Pos() token.Pos   { return token.NoPos }
func (MatchFun) End() token.Pos   { return token.NoPos }
func (StmtsNode) Pos() token.Pos  { return token.NoPos }
func (StmtsNode) End() token.Pos  { return token.NoPos }
func (ExprsNode) Pos() token.Pos  { return token.NoPos }
func (ExprsNode) End() token.Pos  { return token.NoPos }
func (SpecsNode) Pos() token.Pos  { return token.NoPos }
func (SpecsNode) End() token.Pos  { return token.NoPos }
func (IdentsNode) Pos() token.Pos { return token.NoPos }
func (IdentsNode) End() token.Pos { return token.NoPos }
func (FieldsNode) Pos() token.Pos { return token.NoPos }
func (FieldsNode) End() token.Pos { return token.NoPos }
func (TokenNode) Pos() token.Pos  { return token.NoPos }
func (TokenNode) End() token.Pos  { return token.NoPos }
