package matcher

import (
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
	"regexp"
	"strconv"

	"golang.org/x/tools/go/types/typeutil"
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

// Nil literal represents wildcard[T] for convenient, so a special Nil pattern needed
func Nil[T Pattern](m *Matcher) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		return IsNilNode(n)
	})
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

// Any when any subtree of rootNode matched pattern, return immediately
func Any[T Pattern](m *Matcher, pattern ast.Node) T {
	return MkPattern[T](m, func(m *Matcher, rootNode ast.Node, stack []ast.Node, binds Binds) bool {
		return m.Matched(pattern, rootNode)
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓ Slice ↓↓↓↓↓↓↓↓↓↓↓↓

// func Contains[E ElemPattern, S SlicePattern](m *Matcher, elPtn E) S {
// 	return MkPattern[S](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
// 		if n == nil /*ast.Node(nil)*/ {
// 			return false
// 		}
// 		xs := reflect.ValueOf(n)
// 		fun := TryGetMatchFun[E](m, elPtn)
// 		assert(fun != nil, "bad pattern")
// 		for i := 0; i < xs.Len(); i++ {
// 			node := xs.Index(i).Interface().(ast.Node)
// 			if fun(m, node, stack, binds) {
// 				return true
// 			}
// 		}
// 		return false
// 	})
// }

func Contains[S SlicePattern](m *Matcher, p ast.Node) S {
	return MkPattern[S](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		xs := reflect.ValueOf(n)
		for i := 0; i < xs.Len(); i++ {
			node := xs.Index(i).Interface().(ast.Node)
			if m.Matched(p, node) {
				return true
			}
		}
		return false
	})
}

func SliceLenOf[T SlicePattern](m *Matcher, p Predicate[int]) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		return p(reflect.ValueOf(n).Len())
	})
}

func SliceLenEQ[T SlicePattern](m *Matcher, n int) T {
	return SliceLenOf[T](m, func(len int) bool { return len == n })
}
func SliceLenGT[T SlicePattern](m *Matcher, n int) T {
	return SliceLenOf[T](m, func(len int) bool { return len > n })
}
func SliceLenGE[T SlicePattern](m *Matcher, n int) T {
	return SliceLenOf[T](m, func(len int) bool { return len >= n })
}
func SliceLenLT[T SlicePattern](m *Matcher, n int) T {
	return SliceLenOf[T](m, func(len int) bool { return len < n })
}
func SliceLenLE[T SlicePattern](m *Matcher, n int) T {
	return SliceLenOf[T](m, func(len int) bool { return len >= n })
}

// ↓↓↓↓↓↓↓↓↓↓↓ Ident ↓↓↓↓↓↓↓↓↓↓↓↓

func IdentOf(m *Matcher, p Predicate[*ast.Ident]) IdentPattern {
	return m.mkIdentPattern(func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		ident := n.(*ast.Ident)
		if ident == nil {
			return false
		}
		return p(ident)
	})
}

func IdentNameIs(m *Matcher, name string) IdentPattern {
	return IdentOf(m, func(id *ast.Ident) bool { return name == id.Name })
}

func IdentNameMatch(m *Matcher, reg *regexp.Regexp) IdentPattern {
	return IdentOf(m, func(id *ast.Ident) bool { return reg.Match([]byte(id.Name)) })
}

func ObjectOf(m *Matcher, pred Predicate[types.Object]) IdentPattern {
	return MkPattern[IdentPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		switch n := n.(type) {
		case *ast.Ident:
			obj := m.ObjectOf(n)
			if obj == nil {
				return false
			}
			return pred(obj)
		case *ast.SelectorExpr:
			sel := m.Selections[n]
			if sel == nil {
				return false
			}
			obj := sel.Obj()
			if obj == nil {
				return false
			}
			return pred(obj)
		default:
			return false
		}
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

// ↓↓↓↓↓↓↓↓↓↓↓↓ BasicLit ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func BasicLitOfKind(m *Matcher, kind token.Token) ExprPattern {
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

func StringLitOf(m *Matcher, p Predicate[string]) ExprPattern {
	return MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		// Notice: ExprPattern returns, so param n of callback is ast.Expr
		// n is BasicLit expr and not nil
		lit, _ := n.(*ast.BasicLit)
		if lit == nil {
			return false
		}
		if lit.Kind != token.STRING {
			return false
		}
		val, _ := strconv.Unquote(lit.Value)
		return p(val)
	})
}

func TagOf(m *Matcher, p Predicate[*reflect.StructTag]) BasicLitPattern {
	return MkPattern[BasicLitPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		tagLit, _ := n.(*ast.BasicLit)
		if tagLit == nil {
			// return false
			return p(nil)
		}

		assert(tagLit.Kind == token.STRING, "")
		tag, _ := strconv.Unquote(tagLit.Value)
		structTag := reflect.StructTag(tag)
		return p(&structTag)
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Callee ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
// ref testdata/match/callee.txt

// CalleeOf a builtin / function / method / var call
func CalleeOf(m *Matcher, p Predicate[types.Object]) CallExprPattern {
	return MkPattern[CallExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		call := n.(*ast.CallExpr)
		if call == nil {
			return false
		}
		return p(typeutil.Callee(m.Info, call))
	})
}

// BuiltinCalleeOf a builtin function call
func BuiltinCalleeOf(m *Matcher, p Predicate[*types.Builtin]) CallExprPattern {
	return CalleeOf(m, func(callee types.Object) bool {
		if f, ok := callee.(*types.Builtin); ok {
			return p(f)
		}
		return false
	})
}

// VarCalleeOf a var call
func VarCalleeOf(m *Matcher, p Predicate[*types.Var]) CallExprPattern {
	return CalleeOf(m, func(callee types.Object) bool {
		if f, ok := callee.(*types.Var); ok {
			return p(f)
		}
		return false
	})
}

// FuncOrMethodCalleeOf a function or method call, exclude builtin and var call
func FuncOrMethodCalleeOf(m *Matcher, p Predicate[*types.Func]) CallExprPattern {
	return CalleeOf(m, func(callee types.Object) bool {
		if f, ok := callee.(*types.Func); ok {
			return p(f)
		}
		return false
	})
}

func FuncCalleeOf(m *Matcher, p Predicate[*types.Func]) CallExprPattern {
	return CalleeOf(m, func(callee types.Object) bool {
		if f, ok := callee.(*types.Func); ok {
			recv := f.Type().(*types.Signature).Recv()
			return recv == nil && p(f)
		}
		return false
	})
}

func MethodCalleeOf(m *Matcher, p Predicate[*types.Func]) CallExprPattern {
	return CalleeOf(m, func(callee types.Object) bool {
		if f, ok := callee.(*types.Func); ok {
			recv := f.Type().(*types.Signature).Recv()
			return recv != nil && p(f)
		}
		return false
	})
}

// StaticCalleeOf a static function (or method) call, exclude var / builtin call
func StaticCalleeOf(m *Matcher, p Predicate[*types.Func]) CallExprPattern {
	return FuncOrMethodCalleeOf(m, func(f *types.Func) bool {
		recv := f.Type().(*types.Signature).Recv()
		isIfaceRecv := recv != nil && types.IsInterface(recv.Type())
		return !isIfaceRecv && p(f)
	})
}

func IfaceCalleeOf(m *Matcher, p Predicate[*types.Func]) CallExprPattern {
	return MethodCalleeOf(m, func(f *types.Func) bool {
		recv := f.Type().(*types.Signature).Recv()
		return types.IsInterface(recv.Type()) && p(f)
	})
}

// ↓↓↓↓↓↓ exactly callee match ↓↓↓↓↓↓

func BuiltinCallee(m *Matcher, fun string) CallExprPattern {
	builtIn := types.Universe.Lookup(fun)
	assert(builtIn != nil, fun+" not found")

	return BuiltinCalleeOf(m, func(callee *types.Builtin) bool {
		return callee.Name() == fun &&
			callee.Type() == builtIn.Type()
	})
}

func FuncCallee(m *Matcher, pkg, fun string) CallExprPattern {
	qualified := pkg + "." + fun
	funObj := m.Lookup(qualified)
	assert(funObj != nil, qualified+" not found")

	_, isFunc := funObj.Type().(*types.Signature)
	assert(isFunc, qualified+" not func")

	return FuncCalleeOf(m, func(f *types.Func) bool {
		return f.Name() == fun &&
			funObj.Type() == f.Type()
	})
}

func MethodCallee(m *Matcher, pkg, typ, method string, addressable bool) CallExprPattern {
	qualified := pkg + "." + typ
	tyObj := m.Lookup(qualified)
	assert(tyObj != nil, qualified+" not found")

	methodObj, _, _ := types.LookupFieldOrMethod(tyObj.Type(), addressable, tyObj.Pkg(), method)
	assert(methodObj != nil, method+" not found")

	_, isFunc := methodObj.Type().(*types.Signature)
	assert(isFunc, method+" not func")

	return MethodCalleeOf(m, func(f *types.Func) bool {
		return f.Name() == method &&
			f.Type() == methodObj.Type()
	})
}

func IfaceCallee(m *Matcher, pkg, iface, method string) CallExprPattern {
	qualified := pkg + "." + iface
	ifaceObj := m.Lookup(qualified)
	assert(ifaceObj != nil, qualified+" not found")

	// types.Named -> types.Interface
	_, isIface := ifaceObj.Type().Underlying().(*types.Interface)
	assert(isIface, qualified+" not interface")

	methodObj, _, _ := types.LookupFieldOrMethod(ifaceObj.Type(), false, ifaceObj.Pkg(), method)
	assert(methodObj != nil, method+" not found")

	_, isFunc := methodObj.Type().(*types.Signature)
	assert(isFunc, method+" not func")

	return IfaceCalleeOf(m, func(f *types.Func) bool {
		return f.Name() == method &&
			f.Type() == methodObj.Type()
	})
}

// ↑↑↑↑↑ exactly callee match ↑↑↑↑↑

// ↓↓↓↓↓↓↓↓↓↓↓↓ Recv ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// IsFuncRecv For ast.FuncDecl Recv
func IsFuncRecv(m *Matcher) FieldListPattern {
	return MkPattern[FieldListPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		return IsNilNode(n)
	})
}

func IsMethodRecv(m *Matcher) FieldListPattern {
	return Not[FieldListPattern](m, IsFuncRecv(m))
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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Builtin & Init  ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func IsBuiltin(m *Matcher) IdentPattern {
	return ObjectOf(m, func(obj types.Object) bool {
		_, ok := obj.(*types.Builtin)
		return ok
	})
}

func InitFunc(m *Matcher) *ast.FuncDecl {
	// return &ast.FuncDecl{
	// 	// types.Identical(*types.Signature, *types.Signature) does not compare recv
	// 	Recv: IsFuncRecv(m),
	// 	Name: And(m,
	// 		IdentNameIs(m, "init"),
	// 		TypeIdentical[IdentPattern](m, types.NewSignatureType(
	// 			nil, nil, nil, nil, nil, false,
	// 		)),
	// 	),
	// }
	return &ast.FuncDecl{
		Name: And(m,
			IdentNameIs(m, "init"),
			SignatureOf(m, func(sig *types.Signature) bool {
				return sig.Recv() == nil && sig.Params() == nil && sig.Results() == nil
			}),
		),
	}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Selector ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func SelectorOfPkgPath(m *Matcher, path string, reg *regexp.Regexp) *ast.SelectorExpr {
	return SelectorOfPkg(m, func(pkg *types.Package) bool {
		return pkg.Path() == path
	}, func(ident *ast.Ident) bool {
		return reg.MatchString(ident.Name)
	})
}

func SelectorOfPkg(m *Matcher, pPkg Predicate[*types.Package], pId Predicate[*ast.Ident]) *ast.SelectorExpr {
	return &ast.SelectorExpr{
		X: IdentOf(m, func(id *ast.Ident) bool {
			pkg, ok := m.Uses[id].(*types.PkgName)
			return ok && pPkg(pkg.Imported())
		}),
		Sel: IdentOf(m, pId),
	}
}

func SelectorOfStructField(m *Matcher, pStruct Predicate[*types.Struct], pField Predicate[*types.Var]) ExprPattern {
	// must be struct field selector
	return And(m,
		PatternOf[ExprPattern](m, &ast.SelectorExpr{
			X: TypeOf[ExprPattern](m, func(ty types.Type) bool {
				assert(ty != nil, "invalid")
				ts, ok := ty.Underlying().(*types.Struct)
				return ok && pStruct(ts)
			}),
		}),
		MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			sel, _ := n.(*ast.SelectorExpr) // has confirmed
			assert(sel != nil && m.Selections[sel] != nil, "invalid")
			tv, ok := m.Selections[sel].Obj().(*types.Var)
			return ok && tv.IsField() && pField(tv)
		}),
	)
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Unary ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func PtrOf(expr ast.Expr) *ast.UnaryExpr {
	return &ast.UnaryExpr{
		Op: token.AND,
		X:  expr,
	}
}
