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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Node ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Loader ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

const (
	PatternAll = "./..."
	PatternStd = "std"

	IncludeTests = true

	// LoadDepts load all dependencies
	LoadDepts = packages.NeedImports | packages.NeedDeps
	LoadMode  = packages.NeedTypesInfo |
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
	mode := LoadMode
	if loadAll {
		mode |= LoadDepts
	}

	dir, err := filepath.Abs(dir)
	panicIfErr(err)

	fset := token.NewFileSet()
	init, err := packages.Load(&packages.Config{
		Fset:  fset,
		Mode:  mode,
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

// ↓↓↓↓↓↓↓↓↓↓↓↓ Packages ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func trimPkgPath(pkg string) string {
	xs := strings.Split(pkg, " ")
	if len(xs) > 1 {
		return strings.Trim(xs[1], `"`)
	}
	return strings.Trim(pkg, `"`)
}

func fmtImport(spec *ast.ImportSpec) string {
	if spec.Name == nil {
		return spec.Path.Value
	}
	return spec.Name.Name + " " + spec.Path.Value
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
