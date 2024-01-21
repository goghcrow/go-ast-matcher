package imports

import (
	"go/ast"
	"go/token"
	"go/types"
	"strconv"
	"strings"

	"github.com/goghcrow/go-ast-matcher"
	"golang.org/x/tools/go/ast/astutil"
)

type (
	Matcher     = matcher.Matcher
	PackagePath = matcher.PackagePath
)

func Optimize(
	m *Matcher,
	file *ast.File,
	projectPkgPrefix string,
	companyPkgPrefix []string,
) {
	Clean(m, file)
	Sort(file, projectPkgPrefix, companyPkgPrefix)
}

func Uses(m *Matcher, f *ast.File, pkg *types.Package) (use bool) {
	path := pkg.Path()
	s := ImportSpec(f, path)
	importedName := importName(s, pkg.Name())
	if staticUsesImport(f, s, importedName) == used {
		return true
	}

	usePkg := func(id *ast.Ident) bool {
		switch obj := m.ObjectOf(id).(type) {
		case nil:
			return false
		case *types.PkgName:
			return obj.Imported() == pkg
		default:
			return obj.Pkg() == pkg
		}
	}

	astutil.Apply(f, nil, func(c *astutil.Cursor) bool {
		if _, is := c.Parent().(*ast.ImportSpec); is {
			return true // skip import spec
		}

		n := c.Node()
		switch n := n.(type) {
		case *ast.Ident:
			// skip selector.Sel
			if sel, ok := c.Parent().(*ast.SelectorExpr); ok {
				if sel.Sel == n {
					return true
				}
			}
			// only to mark top ident
			if n.Obj == nil {
				use = usePkg(n)
			}
		case *ast.Field:
			for _, name := range n.Names {
				use = usePkg(name)
				if use {
					break
				}
			}
		}
		return !use // stop walking
	})
	return
}

func staticUsesImport(f *ast.File, spec *ast.ImportSpec, pkgName string) useImport {
	if spec == nil {
		return unused
	}

	importedName := spec.Name.String()
	switch pkgName {
	case "<nil>":
		// no alias, use pkg name
		importedName = pkgName
	case "_":
		return used
	case ".":
		return unknown
	}

	found := false
	ast.Inspect(f, func(n ast.Node) bool {
		sel, ok := n.(*ast.SelectorExpr)
		if ok && isTopName(sel.X, importedName) {
			found = true
		}
		return true
	})
	if found {
		return used
	}
	return unused
}

func isTopName(n ast.Expr, name string) bool {
	id, ok := n.(*ast.Ident)
	return ok && id.Name == name && id.Obj == nil
}

func ImportsAs(f *ast.File, name, path string) bool {
	for _, s := range f.Imports {
		if SpecPath(s) == path {
			return s.Name.String() == name
		}
	}
	return false
}

func Imports(f *ast.File, path string) bool {
	return ImportSpec(f, path) != nil
}

func ImportSpec(f *ast.File, path string) *ast.ImportSpec {
	for _, s := range f.Imports {
		if SpecPath(s) == path {
			return s
		}
	}
	return nil
}

func SpecPath(s *ast.ImportSpec) string {
	// for unparsed: ast.File.Decls.Specs(ImportSpec).Path.Value
	// for parsed: ast.File.Imports
	path := s.Path.Value
	xs := strings.Split(path, " ")
	if len(xs) > 1 {
		path = xs[1]
	}
	t, err := strconv.Unquote(path)
	panicIfErr(err)
	return t
}

func SpecName(s *ast.ImportSpec) string {
	name := s.Name.String()
	xs := strings.Split(s.Path.Value, " ")
	if len(xs) > 1 {
		assert(name == "<nil>", "illegal state")
		name = xs[0]
	}
	if name == "<nil>" {
		return ""
	}
	return name
}

func ImportName(f *ast.File, path string, pkgName string) (name string) {
	s := ImportSpec(f, path)
	return importName(s, pkgName)
}

func importName(s *ast.ImportSpec, pkgName string) (name string) {
	if s == nil {
		return
	}
	name = s.Name.String()
	if name == "<nil>" || name == "" {
		return pkgName
	}
	return
}

func Fmt(s *ast.ImportSpec) string {
	if s.Name == nil {
		return s.Path.Value
	}
	return s.Name.Name + " " + s.Path.Value
}

func walkImportGroup(file *ast.File, f func(*ast.GenDecl)) {
	for _, decl := range file.Decls {
		d, ok := decl.(*ast.GenDecl)
		if !ok || d.Tok != token.IMPORT {
			continue
		}
		f(d)
	}
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
