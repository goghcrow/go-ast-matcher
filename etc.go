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
	"strings"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓ Type ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type typeX string

var TypeX typeX

func (typeX) Implements(v types.Type, iface *types.Interface) bool {
	return types.Implements(v, iface) ||
		types.Implements(types.NewPointer(v), iface)
}
func (typeX) TypeId(obj types.Object) string {
	pkg := obj.Pkg()
	name := obj.Name()
	if pkg != nil && pkg.Path() != "" {
		return pkg.Path() + "." + name
	} else {
		return name
	}
}
func (typeX) DerefUnder(ty types.Type) types.Type {
	for {
		if ptr, ok := ty.(*types.Pointer); ok {
			ty = ptr.Elem()
		} else {
			break
		}
	}
	return ty.Underlying()
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Show ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func ShowNode(fset *token.FileSet, n ast.Node) string {
	// 处理自定义的 Node 类型
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
	}

	var buf bytes.Buffer
	_ = printer.Fprint(&buf, fset, n)
	return buf.String()
}
func ShowPos(fset *token.FileSet, n ast.Node) token.Position {
	return fset.Position(n.Pos())
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ IsNilNode ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

// ↓↓↓↓↓↓↓↓↓↓↓↓ IO ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Others ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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
