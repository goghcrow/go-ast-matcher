package matcher

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"go/types"
	"os"
	"reflect"
	"strconv"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓ Type ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func typeId(obj types.Object) string {
	pkg := obj.Pkg()
	name := obj.Name()
	if pkg != nil && pkg.Path() != "" {
		return pkg.Path() + "." + name
	} else {
		return name
	}
}

func deref(ty types.Type) types.Type {
	for {
		if ptr, ok := ty.(*types.Pointer); ok {
			ty = ptr.Elem()
		} else {
			break
		}
	}
	return ty
}

func derefUnder(ty types.Type) types.Type {
	return deref(ty).Underlying()
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Show ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func ShowNode(fset *token.FileSet, n ast.Node) string {
	if IsPseudoNode(n) {
		return showPseudoNode(fset, n)
	}
	var buf bytes.Buffer
	_ = printer.Fprint(&buf, fset, n)
	return buf.String()
}

func PosOfNode(fset *token.FileSet, n ast.Node) token.Position {
	return fset.Position(n.Pos())
}

func LocOfPos(fset *token.FileSet, pos token.Pos) string {
	if pos == token.NoPos {
		return ""
	}
	return " at " + fset.Position(pos).String()
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Is ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// IsNilNode for typed-nil problem
func IsNilNode(n ast.Node) bool {
	// if type of n is identity to ast.Node
	// e.g. var n ast.Node
	if n == nil {
		return true
	}
	// if type of n is subtype of ast.Node
	// e.g. var n *ast.Ident
	if v := reflect.ValueOf(n); v.Kind() == reflect.Ptr && v.IsNil() {
		// n = nil // conv typed nil to untyped nil
		return true
	}
	return false
}

func IsNode[T ast.Node](n ast.Node) bool {
	_, ok := n.(T)
	return ok
}

func IsNilType(ty types.Type) bool {
	if ty == nil {
		return true
	}
	if v := reflect.ValueOf(ty); v.Kind() == reflect.Ptr && v.IsNil() {
		return true
	}
	return false
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Package ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// func trimPkgPath(pkg string) string {
// 	xs := strings.Split(pkg, " ")
// 	if len(xs) > 1 {
// 		return strings.Trim(xs[1], `"`)
// 	}
// 	return strings.Trim(pkg, `"`)
// }

func trimPkgPath(pkg string) string {
	pkg, _ = strconv.Unquote(pkg)
	xs := strings.Split(pkg, " ")
	if len(xs) > 1 {
		return xs[1]
	}
	return pkg
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ BuildTag ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func parseBuildTag(f *ast.File) string {
	startWith := strings.HasPrefix
	trimLeft := strings.TrimPrefix
	for _, g := range f.Comments {
		for _, c := range g.List {
			if startWith(c.Text, "//go:build") ||
				startWith(c.Text, "//+build") {
				return strings.TrimSpace(
					trimLeft(trimLeft(c.Text, "//go:build"), "//+build"),
				)
			}
		}
	}
	return ""
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Other ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func preOrder(root ast.Node, f astutil.ApplyFunc) {
	astutil.Apply(root, f, nil)
}

func postOrder(root ast.Node, f astutil.ApplyFunc) {
	astutil.Apply(root, nil, f)
}

func errLog(a ...any) {
	_, _ = fmt.Fprintln(os.Stderr, a...)
}

func exitOf(msg string) {
	os.Exit(1)
}

func panicIfErr(err error) {
	if err != nil {
		panic(err)
	}
}

func assert(b bool, msg string) {
	if !b {
		panic(msg)
	}
}
