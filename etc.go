package astmatcher

import (
	"bytes"
	"go/ast"
	"go/format"
	"go/printer"
	"go/token"
	"go/types"
	"os"
	"path/filepath"
	"reflect"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

// IsNilNode 解决参数是接口类型判空的问题
func IsNilNode(n ast.Node) bool {
	// 如果 n 本身就是 ast.Node, e.g var n ast.Node
	if n == nil {
		return true
	}
	// 如果 n 是 ast.Node 的子类型
	if v := reflect.ValueOf(n); v.Kind() == reflect.Ptr && v.IsNil() {
		// n = nil // 这样可以把 typed nil 转换成 untyped nil
		return true
	}
	return false
}

func TypeImplements(v types.Type, iface *types.Interface) bool {
	return types.Implements(v, iface) ||
		types.Implements(types.NewPointer(v), iface)
}

func TypeId(obj types.Object) string {
	pkg := obj.Pkg()
	name := obj.Name()
	if pkg != nil && pkg.Path() != "" {
		return pkg.Path() + "." + name
	} else {
		return name
	}
}

func DerefUnder(ty types.Type) types.Type {
	for {
		if ptr, ok := ty.(*types.Pointer); ok {
			ty = ptr.Elem()
		} else {
			break
		}
	}
	return ty.Underlying()
}

func ShowNode(fset *token.FileSet, n ast.Node) string {
	var buf bytes.Buffer
	_ = printer.Fprint(&buf, fset, n)
	return buf.String()
}

func ShowPos(fset *token.FileSet, n ast.Node) string {
	return fset.Position(n.Pos()).String()
}

func LoadDir(dir string) (*token.FileSet, []*packages.Package) {
	dir, err := filepath.Abs(dir)
	panicIfErr(err)

	fset := token.NewFileSet()
	pkgs, err := packages.Load(&packages.Config{
		Fset: fset,
		Mode: packages.NeedTypesInfo |
			packages.NeedName |
			packages.NeedTypes |
			packages.NeedSyntax |
			packages.NeedImports |
			packages.NeedDeps |
			packages.NeedCompiledGoFiles,
		Tests: true, // 包括测试
		Dir:   dir,
	}, "./..." /*all*/) // e.g. pattern std
	panicIfErr(err)
	return fset, pkgs
}

func WriteFile(fset *token.FileSet, filename string, f *ast.File) {
	fh, err := os.Create(filename)
	panicIfErr(err)
	defer func() {
		err = fh.Close()
		panicIfErr(err)
	}()
	err = format.Node(fh, fset, f)
	panicIfErr(err)
}

func FormatFile(fset *token.FileSet, f *ast.File) []byte {
	buf := new(bytes.Buffer)
	err := format.Node(buf, fset, f)
	panicIfErr(err)
	return buf.Bytes()
}

func preOrder(root ast.Node, f astutil.ApplyFunc) {
	astutil.Apply(root, f, nil)
}

func postOrder(root ast.Node, f astutil.ApplyFunc) {
	astutil.Apply(root, nil, f)
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
