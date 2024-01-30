package matcher

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/printer"
	"go/token"
	"go/types"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

func preOrder(root ast.Node, f astutil.ApplyFunc) {
	astutil.Apply(root, f, nil)
}

func postOrder(root ast.Node, f astutil.ApplyFunc) {
	astutil.Apply(root, nil, f)
}

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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Normalize ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func PkgPath(pkg *types.Package) string {
	if pkg == nil {
		return ""
	}
	if pkg.Path() == "command-line-arguments" {
		return ""
	}
	return pkg.Path()
}

func FunName(fun *types.Func) string {
	return strings.ReplaceAll(fun.FullName(), "command-line-arguments.", "")
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

func Diff(oldName string, old []byte, newName string, new []byte) []byte {
	f1, err := writeTempFile(old)
	panicIf(err)
	//goland:noinspection GoUnhandledErrorResult
	defer os.Remove(f1)

	f2, err := writeTempFile(new)
	panicIf(err)
	//goland:noinspection GoUnhandledErrorResult
	defer os.Remove(f2)

	data, err := exec.Command("diff", "-u", f1, f2).CombinedOutput()
	if err != nil && len(data) == 0 {
		panicIf(err)
	}

	if len(data) == 0 {
		return nil
	}

	i := bytes.IndexByte(data, '\n')
	if i < 0 {
		return data
	}
	j := bytes.IndexByte(data[i+1:], '\n')
	if j < 0 {
		return data
	}
	start := i + 1 + j + 1
	if start >= len(data) || data[start] != '@' {
		return data
	}

	return append([]byte(fmt.Sprintf("diff %s %s\n--- %s\n+++ %s\n", oldName, newName, oldName, newName)), data[start:]...)
}

func writeTempFile(data []byte) (string, error) {
	file, err := os.CreateTemp("", "diff")
	if err != nil {
		return "", err
	}
	_, err = file.Write(data)
	if err1 := file.Close(); err == nil {
		err = err1
	}
	if err != nil {
		_ = os.Remove(file.Name())
		return "", err
	}
	return file.Name(), nil
}

func mustMkDir(dir string) string {
	dir, err := filepath.Abs(dir)
	panicIf(err)
	err = os.MkdirAll(dir, os.ModePerm)
	panicIf(err)
	return dir
}

func errLog(a ...any) {
	_, _ = fmt.Fprintln(os.Stderr, a...)
}

func exitOf(msg string) {
	os.Exit(1)
}

func panicIf(err error) {
	if err != nil {
		panic(err)
	}
}

func assert(b bool, msg string) {
	if !b {
		panic(msg)
	}
}
