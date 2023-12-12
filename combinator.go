package matcher

import (
	"go/ast"
	"go/constant"
	"go/token"
	"go/types"
	"reflect"
	"regexp"
	"strconv"

	"golang.org/x/tools/go/types/typeutil"
)

type (
	Comparator[T /*comparable*/ any] func(T, T) bool
	Predicate[T any]                 func(T) bool
	Unary[T any]                     func(T) T
	Binary[T any]                    func(T, T) T
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Primitive Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// Wildcard is a pattern that matches any node
func Wildcard[T Pattern](m *Matcher) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool { return true })
}

// Nil literal represents wildcard[T] for convenient, so a special Nil pattern needed
func Nil[T Pattern](m *Matcher) T {
	return MkPattern[T](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		return IsNilNode(n)
	})
}

// Bind matched node to variable, so can be retrieved from env in callback's arg
func Bind[T Pattern](m *Matcher, variable string, ptn T) T {
	return And(m, ptn, MkVar[T](m, variable))
}

// Any subtree node matched pattern
func Any[T Pattern](m *Matcher, nodeOrPtn ast.Node) T {
	return MkPattern[T](m, func(m *Matcher, root ast.Node, stack []ast.Node, binds Binds) bool {
		return m.Matched(nodeOrPtn, root)
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Relational Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type NodeOrPtn = ast.Node

// Not a must be Pattern, can not be node literal, means TryGetMatchFun(m, a) != nil
func Not[Ptn Pattern](m *Matcher, a Ptn) Ptn {
	return combine1[Ptn](m, a, func(a MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return !a(m, n, stack, binds)
		}
	})
}

func NotEx[T Pattern](m *Matcher, a NodeOrPtn) T {
	return combineEx1[T](m, a, func(a MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return !a(m, n, stack, binds)
		}
	})
}

// And lhs, rhs must be Pattern, can not be node literal, means TryGetMatchFun(m, l or r) != nil
func And[Ptn Pattern](m *Matcher, lhs, rhs Ptn) Ptn {
	return combine[Ptn](m, lhs, rhs, func(lhs, rhs MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return lhs(m, n, stack, binds) && rhs(m, n, stack, binds)
		}
	})
}

func AndEx[Ptn Pattern](m *Matcher, lhs, rhs NodeOrPtn) Ptn {
	return combineEx[Ptn](m, lhs, rhs, func(lhs, rhs MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return lhs(m, n, stack, binds) && rhs(m, n, stack, binds)
		}
	})
}

// Or lhs, rhs must be Pattern, can not be node literal, means TryGetMatchFun(m, l or r) != nil
func Or[Ptn Pattern](m *Matcher, lhs, rhs Ptn) Ptn {
	return combine[Ptn](m, lhs, rhs, func(lhs, rhs MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return lhs(m, n, stack, binds) || rhs(m, n, stack, binds)
		}
	})
}

func OrEx[Ptn Pattern](m *Matcher, lhs, rhs NodeOrPtn) Ptn {
	return combineEx[Ptn](m, lhs, rhs, func(lhs, rhs MatchFun) MatchFun {
		return func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			return lhs(m, n, stack, binds) || rhs(m, n, stack, binds)
		}
	})
}

func combine1[T Pattern](m *Matcher, a T, un Unary[MatchFun]) T {
	return MkPattern[T](m, un(
		MustGetMatchFun[T](m, a),
	))
}

func combine[T Pattern](m *Matcher, a, b T, bin Binary[MatchFun]) T {
	return MkPattern[T](m, bin(
		MustGetMatchFun[T](m, a),
		MustGetMatchFun[T](m, b),
	))
}

func combineEx1[T Pattern](m *Matcher, a NodeOrPtn, un Unary[MatchFun]) T {
	return MkPattern[T](m, un(
		TryGetOrMkMatchFun[T](m, a),
	))
}

func combineEx[T Pattern](m *Matcher, a, b ast.Node, bin Binary[MatchFun]) T {
	return MkPattern[T](m, bin(
		TryGetOrMkMatchFun[T](m, a),
		TryGetOrMkMatchFun[T](m, b),
	))
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Typing Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Literal Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// Notice: BasicLit is atomic Pattern,
// &ast.BasicLit{ Kind: token.INT } can be used for matching INT literal
// because zero Value is ambiguous, wildcard or zero value ?

// Notice: LitXXXOf returns ExprPattern, so type of callback param is ast.Expr

func LitKindOf(m *Matcher, kind token.Token) ExprPattern {
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

func LitOf(m *Matcher, kind token.Token, p Predicate[constant.Value]) ExprPattern {
	return MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		lit, _ := n.(*ast.BasicLit)
		if lit == nil {
			return false
		}
		if lit.Kind != kind {
			return false
		}
		val := constant.MakeFromLiteral(lit.Value, kind, 0)
		return p(val)
	})
}

func LitIntOf(m *Matcher, p Predicate[constant.Value]) ExprPattern   { return LitOf(m, token.INT, p) }
func LitFloatOf(m *Matcher, p Predicate[constant.Value]) ExprPattern { return LitOf(m, token.FLOAT, p) }
func LitCharOf(m *Matcher, p Predicate[constant.Value]) ExprPattern  { return LitOf(m, token.CHAR, p) }
func LitStringOf(m *Matcher, p Predicate[constant.Value]) ExprPattern {
	return LitOf(m, token.STRING, p)
}
func LitStringValOf(m *Matcher, p Predicate[string]) ExprPattern {
	return MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
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

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Identifier Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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
	return IdentOf(m, func(id *ast.Ident) bool {
		return name == id.Name
	})
}

func IdentNameMatch(m *Matcher, reg *regexp.Regexp) IdentPattern {
	return IdentOf(m, func(id *ast.Ident) bool {
		return reg.Match([]byte(id.Name))
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Object Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func ObjectOf(m *Matcher, pred Predicate[types.Object]) ExprPattern {
	return OrEx[ExprPattern](m,
		IdentObjectOf(m, pred),
		SelectorObjectOf(m, pred),
	)
}

func IdentObjectOf(m *Matcher, pred Predicate[types.Object]) IdentPattern {
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
		default:
			return false
		}
	})
}

func SelectorObjectOf(m *Matcher, pred Predicate[types.Object]) ExprPattern {
	return MkPattern[ExprPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
		if n == nil /*ast.Node(nil)*/ {
			return false
		}
		switch n := n.(type) {
		case *ast.SelectorExpr:
			// XXX m.Selections[n]
			obj := m.ObjectOf(n.Sel)
			if obj == nil {
				return false
			}
			return pred(obj)
		default:
			return false
		}
	})
}

func IsBuiltin(m *Matcher) IdentPattern {
	return IdentObjectOf(m, func(obj types.Object) bool {
		_, ok := obj.(*types.Builtin)
		return ok
	})
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Slice Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func SliceContains[S SlicePattern](m *Matcher, p NodeOrPtn) S {
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

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Callee Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓
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

// BuiltinCallee match fun exactly
func BuiltinCallee(m *Matcher, fun string) CallExprPattern {
	builtIn := types.Universe.Lookup(fun)
	assert(builtIn != nil, fun+" not found")

	return BuiltinCalleeOf(m, func(callee *types.Builtin) bool {
		return callee.Name() == fun &&
			callee.Type() == builtIn.Type()
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

// FuncCallee match pkg.fun exactly
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

func MethodCalleeOf(m *Matcher, p Predicate[*types.Func]) CallExprPattern {
	return CalleeOf(m, func(callee types.Object) bool {
		if f, ok := callee.(*types.Func); ok {
			recv := f.Type().(*types.Signature).Recv()
			return recv != nil && p(f)
		}
		return false
	})
}

// MethodCallee match pkg.typ.method exactly
// addressable means whether the receiver is addressable
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

// IfaceCallee match pkg.iface.method exactly
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

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Recv Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Selector Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// TODO replace by objectOf
func SelectorOfPkgPath(m *Matcher, path string, reg *regexp.Regexp) *ast.SelectorExpr {
	return SelectorOfPkg(m, func(pkg *types.Package) bool {
		return pkg.Path() == path
	}, func(ident *ast.Ident) bool {
		return reg.MatchString(ident.Name)
	})
}

// TODO replace by objectOf
func SelectorOfPkg(m *Matcher, pPkg Predicate[*types.Package], pId Predicate[*ast.Ident]) *ast.SelectorExpr {
	return &ast.SelectorExpr{
		X: IdentOf(m, func(id *ast.Ident) bool {
			pkg, ok := m.Uses[id].(*types.PkgName)
			return ok && pPkg(pkg.Imported())
		}),
		Sel: IdentOf(m, pId),
	}
}

// TODO replace by objectOf
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
			// TODO m.ObjectOf(sel.Sel)
			sel, _ := n.(*ast.SelectorExpr) // has confirmed
			assert(sel != nil && m.Selections[sel] != nil, "invalid")
			tv, ok := m.Selections[sel].Obj().(*types.Var)
			return ok && tv.IsField() && pField(tv)
		}),
	)
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Other Combinators ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

func PtrOf(expr ast.Expr) *ast.UnaryExpr {
	return &ast.UnaryExpr{
		Op: token.AND,
		X:  expr,
	}
}
