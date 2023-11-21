package astmatcher

import (
	"bytes"
	"fmt"
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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Node ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Loader ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

const (
	PatternAll = "./..."
	PatternStd = "std"

	IncludeTests = true

	// NeedImports & NeedDeps 为了加载所有依赖包
	loadMode packages.LoadMode = packages.NeedTypesInfo |
		packages.NeedName |
		packages.NeedFiles |
		packages.NeedExportFile |
		packages.NeedCompiledGoFiles |
		packages.NeedTypes |
		packages.NeedSyntax |
		packages.NeedTypesInfo |
		packages.NeedModule
)

func LoadDir(dir string, patterns []string, loadAll bool) (
	*token.FileSet,
	[]*packages.Package,
	map[string]*packages.Package,
) {
	mode := loadMode
	if loadAll {
		mode |= packages.NeedImports | packages.NeedDeps
	}

	dir, err := filepath.Abs(dir)
	panicIfErr(err)

	fset := token.NewFileSet()
	init, err := packages.Load(&packages.Config{
		Fset:  fset,
		Mode:  loadMode,
		Tests: IncludeTests,
		Dir:   dir,
	}, patterns...)
	panicIfErr(err)

	if len(init) == 0 {
		errLog("no packages found")
	}

	all := map[PackageID]*packages.Package{}
	packages.Visit(init, nil, func(pkg *packages.Package) {
		all[pkg.ID] = pkg
		for _, err := range pkg.Errors {
			errLog(err)
		}
	})

	return fset, init, all
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ Format ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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
