package astmatcher

import (
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
	"regexp"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type (
	Comparator[T /*comparable*/ any] func(T, T) bool
	Predicate[T any]                 func(T) bool
	Unary[T any]                     func(T) T
	Binary[T any]                    func(T, T) T
)

// Wildcard of the same m can be cached
func Wildcard[T Pattern](m *Matcher) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool { return true })
}

func Bind[T Pattern](m *Matcher, name string, ptn T) T {
	return And(m, ptn, MkVar[T](m, name))
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

// ↓↓↓↓↓↓↓↓↓↓↓ Slice ↓↓↓↓↓↓↓↓↓↓↓↓

func Any[E ElemPattern, S SlicePattern](m *Matcher, p E) S {
	return MkPattern[S](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		xs := reflect.ValueOf(n)
		for i := 0; i < xs.Len(); i++ {
			node := xs.Index(i).Interface().(ast.Node)
			if TryGetMatchFun[E](m, p)(m, node, stack, binds) {
				return true
			}
		}
		return false
	})
}

func Len[T SlicePattern](m *Matcher, p Predicate[int]) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		return p(reflect.ValueOf(n).Len())
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓ Ident ↓↓↓↓↓↓↓↓↓↓↓↓

func IdentPredicate(m *Matcher, p Predicate[string]) IdentPattern {
	return m.mkIdentPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		ident := n.(*ast.Ident)
		if ident == nil {
			return false
		}
		return p(ident.Name)
	})
}
func IdentEqual(m *Matcher, name string) IdentPattern {
	return IdentPredicate(m, func(s string) bool { return name == s })
}
func IdentRegex(m *Matcher, reg *regexp.Regexp) IdentPattern {
	return IdentPredicate(m, func(s string) bool { return reg.Match([]byte(s)) })
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Type ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func typeCompare[T TypingPattern](m *Matcher, ty types.Type, cmp Comparator[types.Type]) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		// typeof(n) = ast.Expr | *ast.Ident
		// n maybe nil, e.g. const x = 1
		expr := n.(ast.Expr)
		if expr == nil {
			return false
		}
		exprTy := m.TypeOf(expr)
		assert(ty != nil, "type not found: "+m.ShowNode(expr))
		// if ty == nil { return false }
		return cmp(exprTy, ty)
	})
}

func TypeConvertibleTo[T TypingPattern](m *Matcher, ty types.Type) T {
	return typeCompare[T](m, ty, types.ConvertibleTo)
}
func TypeAssignableTo[T TypingPattern](m *Matcher, ty types.Type) T {
	return typeCompare[T](m, ty, types.AssignableTo)
}
func TypeIdentical[T TypingPattern](m *Matcher, ty types.Type) T {
	return typeCompare[T](m, ty, types.Identical)
}
func TypeIdenticalIgnoreTags[T TypingPattern](m *Matcher, ty types.Type) T {
	return typeCompare[T](m, ty, types.IdenticalIgnoreTags)
}
func TypeImplements[T TypingPattern](m *Matcher, iface *types.Interface) T {
	return typeCompare[T](m, iface, func(v, t types.Type) bool {
		return implements(v, iface)
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Method ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// IsFunction For ast.FuncDecl Recv
func IsFunction(m *Matcher) FieldListPattern {
	return MkPattern[FieldListPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		return IsNilNode(n)
	})
}

func IsMethod(m *Matcher) FieldListPattern {
	return Not[FieldListPattern](m, IsFunction(m))
}

func MethodRecv(m *Matcher, f func(ident *ast.Ident, ty ast.Expr) bool) FieldListPattern {
	return MkPattern[FieldListPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
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
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		lit := n.(*ast.BasicLit)
		if lit == nil {
			return false
		}
		return lit.Kind == kind
	})
}
