package astmatcher

import (
	"go/ast"
	"go/token"
	"go/types"
	"regexp"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Factory ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func MkPattern[T Pattern](m *Matcher, f MatchFun) T {
	var zero T
	switch any(zero).(type) {
	case NodePattern:
		return any(m.MkNodePattern(f)).(T)
	case StmtPattern:
		return any(m.MkStmtPattern(f)).(T)
	case ExprPattern:
		return any(m.MkExprPattern(f)).(T)
	case DeclPattern:
		return any(m.MkDeclPattern(f)).(T)
	case IdentPattern:
		return any(m.MkIdentPattern(f)).(T)
	case FieldPattern:
		return any(m.MkFieldPattern(f)).(T)
	case FieldListPattern:
		return any(m.MkFieldListPattern(f)).(T)
	case CallExprPattern:
		return any(m.MkCallExprPattern(f)).(T)
	case FuncTypePattern:
		return any(m.MkFuncTypePattern(f)).(T)
	case BlockStmtPattern:
		return any(m.MkBlockStmtPattern(f)).(T)
	case TokenPattern:
		return any(m.MkTokenPattern(f)).(T)
	case BasicLitPattern:
		return any(m.MkBasicLitPattern(f)).(T)
	case StmtsPattern:
		return any(m.MkStmtsPattern(f)).(T)
	case ExprsPattern:
		return any(m.MkExprsPattern(f)).(T)
	case IdentsPattern:
		return any(m.MkIdentsPattern(f)).(T)
	case FieldsPattern:
		return any(m.MkFieldsPattern(f)).(T)
	default:
		panic("unreachable")
	}
}

func IsPattern[T Pattern](m *Matcher, n any) bool {
	return TryGetMatchFun[T](m, n) != nil
}

// TryGetMatchFun
// T 为 NodePattern, n 需要 ast.Node
// T 为 StmtPattern, n 需要 ast.Stmt
// T 为 ExprPattern, n 需要 ast.Expr
// T 为 DeclPattern, n 需要 ast.Decl
// T 为 SpecPattern, n 需要 ast.Spec
// T 为 IdentPattern, n 需要 *ast.Ident
// T 为 FieldPattern, n 需要 *ast.Field
// T 为 FieldListPattern, n 需要 *ast.FieldList
// T 为 CallExprPattern, n 需要 *ast.CallExpr
// T 为 FuncTypePattern, n 需要 *ast.FuncType
// T 为 BlockStmtPattern, n 需要 *ast.BlockStmt
// T 为 TokenPattern, n 需要 token.Token
// T 为 BasicLitPattern, n 需要 *ast.BasicLit
// T 为 StmtsPattern, n 需要 []ast.Stmt
// T 为 ExprsPattern, n 需要 []ast.Expr
// T 为 IdentsPattern, n 需要 []*ast.Ident
// T 为 FieldsPattern, n 需要 []*ast.Field
func TryGetMatchFun[T Pattern](m *Matcher, n any) MatchFun {
	var zero T
	switch any(zero).(type) {
	case NodePattern:
		return m.TryGetNodeMatchFun(n.(ast.Node))
	case StmtPattern:
		return m.TryGetStmtMatchFun(n.(ast.Stmt))
	case ExprPattern:
		return m.TryGetExprMatchFun(n.(ast.Expr))
	case DeclPattern:
		return m.TryGetDeclMatchFun(n.(ast.Decl))
	case SpecPattern:
		return m.TryGetSpecMatchFun(n.(ast.Spec))
	case IdentPattern:
		return m.TryGetIdentMatchFun(n.(*ast.Ident))
	case FieldPattern:
		return m.TryGetFieldMatchFun(n.(*ast.Field))
	case FieldListPattern:
		return m.TryGetFieldListMatchFun(n.(*ast.FieldList))
	case CallExprPattern:
		return m.TryGetCallExprMatchFun(n.(*ast.CallExpr))
	case FuncTypePattern:
		return m.TryGetFuncTypeMatchFun(n.(*ast.FuncType))
	case BlockStmtPattern:
		return m.TryGetBlockStmtMatchFun(n.(*ast.BlockStmt))
	case TokenPattern:
		return m.TryGetTokenMatchFun(n.(token.Token))
	case BasicLitPattern:
		return m.TryGetBasicLitMatchFun(n.(*ast.BasicLit))
	case StmtsPattern:
		return m.TryGetStmtsMatchFun(n.([]ast.Stmt))
	case ExprsPattern:
		return m.TryGetExprsMatchFun(n.([]ast.Expr))
	case IdentsPattern:
		return m.TryGetIdentsMatchFun(n.([]*ast.Ident))
	case FieldsPattern:
		return m.TryGetFieldsMatchFun(n.([]*ast.Field))
	default:
		panic("unreachable")
	}
}

func MkPatternVar[T Pattern](m *Matcher, name string) T {
	var zero T
	switch any(zero).(type) {
	case NodePattern:
		return any(m.MkNodePatternVar(name)).(T)
	case StmtPattern:
		return any(m.MkStmtPatternVar(name)).(T)
	case ExprPattern:
		return any(m.MkExprPatternVar(name)).(T)
	case DeclPattern:
		return any(m.MkDeclPatternVar(name)).(T)
	case SpecPattern:
		return any(m.MkSpecPatternVar(name)).(T)
	case IdentPattern:
		return any(m.MkIdentPatternVar(name)).(T)
	case FieldPattern:
		return any(m.MkFieldPatternVar(name)).(T)
	case FieldListPattern:
		return any(m.MkFieldListPatternVar(name)).(T)
	case CallExprPattern:
		return any(m.MkCallExprPatternVar(name)).(T)
	case FuncTypePattern:
		return any(m.MkFuncTypePatternVar(name)).(T)
	case BlockStmtPattern:
		return any(m.MkBlockStmtPatternVar(name)).(T)
	case TokenPattern:
		return any(m.MkTokenPatternVar(name)).(T)
	case BasicLitPattern:
		return any(m.MkBasicLitPatternVar(name)).(T)
	case StmtsPattern:
		return any(m.MkStmtsPatternVar(name)).(T)
	case ExprsPattern:
		return any(m.MkExprsPatternVar(name)).(T)
	case IdentsPattern:
		return any(m.MkIdentsPatternVar(name)).(T)
	case FieldsPattern:
		return any(m.MkFieldsPatternVar(name)).(T)
	default:
		panic("unreachable")
	}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type (
	Comparator[T /*comparable*/ any] func(T, T) bool
	Predicate[T any]                 func(T) bool
	Unary[T any]                     func(T) T
	Binary[T any]                    func(T, T) T
)

func BindWith[T Pattern](m *Matcher, name string, ptn T) T {
	return And(m, ptn, MkPatternVar[T](m, name))
}

func Combine1[T Pattern](m *Matcher, a T, un Unary[MatchFun]) T {
	return MkPattern[T](m, un(
		TryGetMatchFun[T](m, a),
	))
}

func Combine[T Pattern](m *Matcher, a, b T, bin Binary[MatchFun]) T {
	return MkPattern[T](m, bin(
		TryGetMatchFun[T](m, a),
		TryGetMatchFun[T](m, b),
	))
}

func Not[T Pattern](m *Matcher, a T) T {
	return Combine1[T](m, a, func(a MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return !a(m, n, stack, binds)
		}
	})
}
func And[T Pattern](m *Matcher, a, b T) T {
	return Combine[T](m, a, b, func(a, b MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return a(m, n, stack, binds) && b(m, n, stack, binds)
		}
	})
}
func Or[T Pattern](m *Matcher, a, b T) T {
	return Combine[T](m, a, b, func(a, b MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return a(m, n, stack, binds) || b(m, n, stack, binds)
		}
	})
}

// Wildcard 同一个 m 的 wildcard 可以缓存起来使用
func Wildcard[T Pattern](m *Matcher) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool { return true })
}

// ↓↓↓↓↓↓↓↓↓↓↓ Ident ↓↓↓↓↓↓↓↓↓↓↓↓

func IdentPredicate(m *Matcher, p Predicate[string]) IdentPattern {
	return m.MkIdentPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		// 这里不能用 id,ok 判断, typed nil!
		id, _ := n.(*ast.Ident)
		if id == nil {
			return false
		}
		return p(id.Name)
	})
}
func IdentEqual(m *Matcher, name string) IdentPattern {
	return IdentPredicate(m, func(s string) bool { return name == s })
}
func IdentRegex(m *Matcher, reg *regexp.Regexp) IdentPattern {
	return IdentPredicate(m, func(s string) bool { return reg.Match([]byte(s)) })
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Type ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func TypeCompare[T TypingPattern](m *Matcher, ty types.Type, cmp Comparator[types.Type]) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		expr := n.(ast.Expr) // ast.Expr | *ast.Ident
		if expr == nil {     // 注意 cast 之后再判空
			return false
		}
		exprTy := m.TypeOf(expr)
		assert(ty != nil, "type not found: "+m.ShowNode(expr))
		// if ty == nil { return false }
		return cmp(exprTy, ty)
	})
}

func ConvertibleTo[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeCompare[T](m, ty, types.ConvertibleTo)
}
func AssignableTo[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeCompare[T](m, ty, types.AssignableTo)
}
func Identical[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeCompare[T](m, ty, types.Identical)
}
func IdenticalIgnoreTags[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeCompare[T](m, ty, types.IdenticalIgnoreTags)
}
func Implements[T TypingPattern](m *Matcher, iface *types.Interface) T {
	return TypeCompare[T](m, iface, func(v, t types.Type) bool {
		return TypeImplements(v, iface)
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Method ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func MethodRecv(m *Matcher, f func(ident *ast.Ident, ty ast.Expr) bool) FieldListPattern {
	return MkPattern[FieldListPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		lst := n.(*ast.FieldList)
		if lst == nil {
			return false
		}
		if lst.NumFields() != 1 {
			return false
		}
		field := lst.List[0]
		if len(field.Names) == 0 {
			return f(nil, field.Type)
		}
		return f(field.Names[0], field.Type)
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ BasicLit ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func BasicLitKind(m *Matcher, kind token.Token) ExprPattern {
	return MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		lit, _ := n.(*ast.BasicLit)
		if lit == nil {
			return false
		}
		return lit.Kind == kind
	})
}
