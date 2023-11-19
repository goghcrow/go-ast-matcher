package astmatcher

import (
	"go/ast"
	"go/token"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Pattern ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// 如何加入新的 Pattern (but, 应该加的差不多了)
// 0. 声明 XXXPattern 类型, 加入 Pattern
// 1. 加 MkXXXPattern && MkPattern
// 2. 加 TryGetXXXMatchFun & TryGetMatchFun
// 3. 加 MkXXXPatternVar & MkPatternVar
// 4. Hook Matcher, 调用 TryGetXXXMatchFun

// 注意: BasicLit 是原子 Pattern, 不支持展开匹配, 比如展开匹配 Kind
// 且字面量需要用 constant.Compare 判断, 不能只看 Kind

type (
	Pattern interface {
		NodePattern | StmtPattern | ExprPattern | DeclPattern |
			IdentPattern | FieldPattern | FieldListPattern |
			CallExprPattern | FuncTypePattern | BlockStmtPattern | TokenPattern | BasicLitPattern |
			StmtsPattern | ExprsPattern | IdentsPattern | FieldsPattern
	}
	TypingPattern interface {
		IdentPattern | ExprPattern
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
	BasicLitPattern  = *ast.BasicLit // 用来匹配 Field.Tag, Import.Path
	TokenPattern     = token.Token   // 用来匹配 token 类型
	StmtsPattern     = []ast.Stmt
	ExprsPattern     = []ast.Expr
	IdentsPattern    = []*ast.Ident // 用来匹配 Field.Name 等
	FieldsPattern    = []*ast.Field
	// ChanDirPattern = ast.ChanDir // 不需要, 就俩值, 外层用 Or 匹配就行了
	// StringPattern = string // 可以用来展开匹配 Ident, Import.Path 之类
)

// MkNodePattern 回调 node 参数类型 ast.Node
func (m *Matcher) MkNodePattern(f MatchFun) NodePattern {
	return f
}

// MkStmtPattern 回调 node 参数类型 ast.Stmt
func (m *Matcher) MkStmtPattern(f MatchFun) StmtPattern {
	m.funs = append(m.funs, f)
	return &ast.BadStmt{From: token.Pos(-len(m.funs))}
}

// MkExprPattern 回调 node 参数类型 ast.Expr
func (m *Matcher) MkExprPattern(f MatchFun) ExprPattern {
	m.funs = append(m.funs, f)
	return &ast.BadExpr{From: token.Pos(-len(m.funs))}
}

// MkDeclPattern 回调 node 参数类型 ast.Decl
func (m *Matcher) MkDeclPattern(f MatchFun) DeclPattern {
	m.funs = append(m.funs, f)
	return &ast.BadDecl{From: token.Pos(-len(m.funs))}
}

// MkSpecPattern 回调 node 参数类型 ast.Spec
func (m *Matcher) MkSpecPattern(f MatchFun) SpecPattern {
	m.funs = append(m.funs, f)
	return &ast.ImportSpec{EndPos: token.Pos(-len(m.funs))}
}

// MkIdentPattern 回调 node 参数类型 *ast.Ident
func (m *Matcher) MkIdentPattern(f MatchFun) IdentPattern {
	m.funs = append(m.funs, f)
	return &ast.Ident{NamePos: token.Pos(-len(m.funs))}
}

// MkFieldPattern 回调 node 参数类型 *ast.Field
func (m *Matcher) MkFieldPattern(f MatchFun) FieldPattern {
	m.funs = append(m.funs, f)
	// 放到 tag 上防止歧义, 如果放到 Type 上, 会与 Field{ Type: MkExprPattern() } 歧义
	return &ast.Field{Tag: &ast.BasicLit{ValuePos: token.Pos(-len(m.funs))}}
}

// MkFieldListPattern 回调 node 参数类型 *ast.FieldList
func (m *Matcher) MkFieldListPattern(f MatchFun) FieldListPattern {
	m.funs = append(m.funs, f)
	return &ast.FieldList{Opening: token.Pos(-len(m.funs))}
}

// MkCallExprPattern 回调 node 参数类型 *ast.CallExpr
func (m *Matcher) MkCallExprPattern(f MatchFun) CallExprPattern {
	m.funs = append(m.funs, f)
	return &ast.CallExpr{Lparen: token.Pos(-len(m.funs))}
}

// MkFuncTypePattern 回调 node 参数类型 *ast.FuncType
func (m *Matcher) MkFuncTypePattern(f MatchFun) FuncTypePattern {
	m.funs = append(m.funs, f)
	return &ast.FuncType{Func: token.Pos(-len(m.funs))}
}

// MkBlockStmtPattern 回调 node 参数类型 *ast.BlockStmt
func (m *Matcher) MkBlockStmtPattern(f MatchFun) BlockStmtPattern {
	m.funs = append(m.funs, f)
	return &ast.BlockStmt{Lbrace: token.Pos(-len(m.funs))}
}

// MkTokenPattern 回调 node 参数类型 TokenNode
func (m *Matcher) MkTokenPattern(f MatchFun) TokenPattern {
	m.funs = append(m.funs, f)
	return token.Token(-len(m.funs))
}

// MkBasicLitPattern 回调 node 参数类型 *ast.BasicLit
func (m *Matcher) MkBasicLitPattern(f MatchFun) BasicLitPattern {
	m.funs = append(m.funs, f)
	return &ast.BasicLit{ValuePos: token.Pos(-len(m.funs))}
}

// []Pattern 结果跟一个 nil 是为了避免歧义
// 比如 []Expr{ XXXExprPattern }
// 会被识别成 ExprsPattern, 而不是 只有一个元素的 ExprsPattern
// 正常的语法不应该 []Node { , nil }

// MkStmtsPattern 回调 node 参数类型 StmtsNode
func (m *Matcher) MkStmtsPattern(f MatchFun) StmtsPattern {
	return []ast.Stmt{m.MkStmtPattern(f), nil}
}

// MkExprsPattern 回调 node 参数类型 ExprsNode
func (m *Matcher) MkExprsPattern(f MatchFun) ExprsPattern {
	return []ast.Expr{m.MkExprPattern(f), nil}
}

// MkIdentsPattern 回调 node 参数类型 IdentsNode
func (m *Matcher) MkIdentsPattern(f MatchFun) IdentsPattern {
	return []*ast.Ident{m.MkIdentPattern(f), nil}
}

// MkFieldsPattern 回调 node 参数类型 FieldsNode
func (m *Matcher) MkFieldsPattern(f MatchFun) FieldsPattern {
	return []*ast.Field{m.MkFieldPattern(f), nil}
}

func (m *Matcher) TryGetNodeMatchFun(n ast.Node) MatchFun {
	if x, ok := n.(NodePattern); ok {
		return x
	}
	return nil
}

func (m *Matcher) TryGetStmtMatchFun(n ast.Stmt) MatchFun {
	if x, _ := n.(StmtPattern); x != nil && x.From < 0 {
		return m.funs[-x.From-1]
	}
	return nil
}

func (m *Matcher) TryGetExprMatchFun(n ast.Expr) MatchFun {
	if x, _ := n.(ExprPattern); x != nil && x.From < 0 {
		return m.funs[-x.From-1]
	}
	return nil
}

func (m *Matcher) TryGetDeclMatchFun(n ast.Decl) MatchFun {
	if x, _ := n.(DeclPattern); x != nil && x.From < 0 {
		return m.funs[-x.From-1]
	}
	return nil
}

func (m *Matcher) TryGetSpecMatchFun(n ast.Spec) MatchFun {
	if x, _ := n.(*ast.ImportSpec); x != nil && x.EndPos < 0 {
		return m.funs[-x.EndPos-1]
	}
	return nil
}

func (m *Matcher) TryGetIdentMatchFun(x *ast.Ident) MatchFun {
	if x != nil && x.NamePos < 0 {
		return m.funs[-x.NamePos-1]
	}
	return nil
}

func (m *Matcher) TryGetFieldMatchFun(x *ast.Field) MatchFun {
	if x != nil && x.Tag != nil && x.Tag.ValuePos < 0 {
		return m.funs[-x.Tag.ValuePos-1]
	}
	return nil
}

func (m *Matcher) TryGetFieldListMatchFun(x *ast.FieldList) MatchFun {
	if x != nil && x.Opening < 0 {
		return m.funs[-x.Opening-1]
	}
	return nil
}

func (m *Matcher) TryGetCallExprMatchFun(x *ast.CallExpr) MatchFun {
	if x != nil && x.Lparen < 0 {
		return m.funs[-x.Lparen-1]
	}
	return nil
}

func (m *Matcher) TryGetFuncTypeMatchFun(x *ast.FuncType) MatchFun {
	if x != nil && x.Func < 0 {
		return m.funs[-x.Func-1]
	}
	return nil
}

func (m *Matcher) TryGetBlockStmtMatchFun(x *ast.BlockStmt) MatchFun {
	if x != nil && x.Lbrace < 0 {
		return m.funs[-x.Lbrace-1]
	}
	return nil
}

func (m *Matcher) TryGetTokenMatchFun(x token.Token) MatchFun {
	if x < 0 {
		return m.funs[-x-1]
	}
	return nil
}

func (m *Matcher) TryGetBasicLitMatchFun(x *ast.BasicLit) MatchFun {
	if x != nil && x.ValuePos < 0 {
		return m.funs[-x.ValuePos-1]
	}
	return nil
}

func (m *Matcher) TryGetStmtsMatchFun(xs []ast.Stmt) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.TryGetStmtMatchFun(xs[0])
}

func (m *Matcher) TryGetExprsMatchFun(xs []ast.Expr) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.TryGetExprMatchFun(xs[0])
}

func (m *Matcher) TryGetIdentsMatchFun(xs []*ast.Ident) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.TryGetIdentMatchFun(xs[0])
}

func (m *Matcher) TryGetFieldsMatchFun(xs []*ast.Field) MatchFun {
	if len(xs) != 2 || xs[1] != nil {
		return nil
	}
	return m.TryGetFieldMatchFun(xs[0])
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Pattern Variable ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func (m *Matcher) MkNodePatternVar(name string) NodePattern {
	return m.MkNodePattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkStmtPatternVar(name string) StmtPattern {
	return m.MkStmtPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkExprPatternVar(name string) ExprPattern {
	return m.MkExprPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkDeclPatternVar(name string) DeclPattern {
	return m.MkDeclPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkSpecPatternVar(name string) SpecPattern {
	return m.MkSpecPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkIdentPatternVar(name string) IdentPattern {
	return m.MkIdentPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkFieldPatternVar(name string) FieldPattern {
	return m.MkFieldPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkFieldListPatternVar(name string) FieldListPattern {
	return m.MkFieldListPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkCallExprPatternVar(name string) CallExprPattern {
	return m.MkCallExprPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkFuncTypePatternVar(name string) FuncTypePattern {
	return m.MkFuncTypePattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkBlockStmtPatternVar(name string) BlockStmtPattern {
	return m.MkBlockStmtPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkTokenPatternVar(name string) TokenPattern {
	return m.MkTokenPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkBasicLitPatternVar(name string) BasicLitPattern {
	return m.MkBasicLitPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkStmtsPatternVar(name string) StmtsPattern {
	return m.MkStmtsPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkExprsPatternVar(name string) ExprsPattern {
	return m.MkExprsPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkIdentsPatternVar(name string) IdentsPattern {
	return m.MkIdentsPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

func (m *Matcher) MkFieldsPatternVar(name string) FieldsPattern {
	return m.MkFieldsPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		binds[name] = n
		return true
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Primitives ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type (
	MatchFun   func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool
	StmtsNode  []ast.Stmt   // 用于 StmtsPattern 回调参数
	ExprsNode  []ast.Expr   // 用于 ExprsPattern 回调参数
	IdentsNode []*ast.Ident // 用于 IdentsPattern 回调参数
	FieldsNode []*ast.Field // 用于 FieldsPattern 回调参数
	TokenNode  token.Token  // 用于 TokenPattern 回调参数
)

func (MatchFun) Pos() token.Pos   { return token.NoPos }
func (MatchFun) End() token.Pos   { return token.NoPos }
func (StmtsNode) Pos() token.Pos  { return token.NoPos }
func (StmtsNode) End() token.Pos  { return token.NoPos }
func (ExprsNode) Pos() token.Pos  { return token.NoPos }
func (ExprsNode) End() token.Pos  { return token.NoPos }
func (IdentsNode) Pos() token.Pos { return token.NoPos }
func (IdentsNode) End() token.Pos { return token.NoPos }
func (FieldsNode) Pos() token.Pos { return token.NoPos }
func (FieldsNode) End() token.Pos { return token.NoPos }
func (TokenNode) Pos() token.Pos  { return token.NoPos }
func (TokenNode) End() token.Pos  { return token.NoPos }
