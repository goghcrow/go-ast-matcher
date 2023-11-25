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

func SliceLen[T SlicePattern](m *Matcher, p Predicate[int]) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		return p(reflect.ValueOf(n).Len())
	})
}

func SliceLenEQ[T SlicePattern](m *Matcher, n int) T {
	return SliceLen[T](m, func(len int) bool { return len == n })
}
func SliceLenGT[T SlicePattern](m *Matcher, n int) T {
	return SliceLen[T](m, func(len int) bool { return len > n })
}
func SliceLenGE[T SlicePattern](m *Matcher, n int) T {
	return SliceLen[T](m, func(len int) bool { return len >= n })
}
func SliceLenLT[T SlicePattern](m *Matcher, n int) T {
	return SliceLen[T](m, func(len int) bool { return len < n })
}
func SliceLenLE[T SlicePattern](m *Matcher, n int) T {
	return SliceLen[T](m, func(len int) bool { return len >= n })
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

func ObjectOf(m *Matcher, pred Predicate[types.Object]) IdentPattern {
	return MkPattern[IdentPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		id := n.(*ast.Ident)
		if id == nil {
			return false
		}
		return pred(m.ObjectOf(id))
	})
}

func IsBuiltin(m *Matcher) IdentPattern {
	return ObjectOf(m, func(obj types.Object) bool {
		_, ok := obj.(*types.Builtin)
		return ok
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Type ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func TypeOf[T TypingPattern](m *Matcher, pred Predicate[types.Type]) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		// typeof(n) = ast.Expr | *ast.Ident
		// n maybe nil, e.g. const x = 1
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		expr := n.(ast.Expr)
		if expr == nil { // ast.Expr(nil)
			return false
		}
		exprTy := m.TypeOf(expr)
		// assert(ty != nil, "type not found: "+m.ShowNode(expr))
		// if ty == nil { return false }
		return pred(exprTy)
	})
}

func TypeConvertibleTo[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeOf[T](m, func(t types.Type) bool {
		return types.ConvertibleTo(t, ty)
	})
}
func TypeAssignableTo[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeOf[T](m, func(t types.Type) bool {
		return types.AssignableTo(t, ty)
	})
}
func TypeIdentical[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeOf[T](m, func(t types.Type) bool {
		return types.Identical(t, ty)
	})
}
func TypeIdenticalIgnoreTags[T TypingPattern](m *Matcher, ty types.Type) T {
	return TypeOf[T](m, func(t types.Type) bool {
		return types.IdenticalIgnoreTags(t, ty)
	})
}
func TypeImplements[T TypingPattern](m *Matcher, iface *types.Interface) T {
	return TypeOf[T](m, func(t types.Type) bool {
		return types.Implements(t, iface)
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

func SignatureOf(m *Matcher, p Predicate[*types.Signature]) IdentPattern {
	return TypeOf[IdentPattern](m, func(t types.Type) bool {
		if sig, ok := t.(*types.Signature); ok {
			return p(sig)
		}
		return false
	})
}

// RecvTypeOf for ast.FuncDecl { Name }
func RecvTypeOf(m *Matcher, p Predicate[types.Type]) IdentPattern {
	return SignatureOf(m, func(sig *types.Signature) bool {
		if sig.Recv() == nil {
			return false
		}
		return p(sig.Recv().Type())
	})
}

// RecvOf for ast.FuncDecl { Recv }
func RecvOf(m *Matcher, f func(recv *ast.Field) bool) FieldListPattern {
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
		return f(lst.List[0])
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ BasicLit ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func BasicLitKind(m *Matcher, kind token.Token) ExprPattern {
	return MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		// Notice: ExprPattern returns, so param n of callback is ast.Expr
		// n is BasicLit expr and not nil
		lit, _ := n.(*ast.BasicLit)
		if lit == nil {
			return false
		}
		return lit.Kind == kind
	})
}
