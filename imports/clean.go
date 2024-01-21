package imports

import (
	"go/ast"
	"go/token"
	"go/types"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓ clean unused import ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// Clean dedup and remove unused
func Clean(m *Matcher, file *ast.File) {
	i := &importCleaner{
		m:         m,
		file:      file,
		importMap: map[PackagePath]*packages.Package{},
		useMap:    map[PackagePath]bool{},
		dedupMap:  map[string]bool{},
	}

	i.imports()
	i.uses()
	i.rebuild()
}

type importCleaner struct {
	m         *Matcher
	file      *ast.File
	importMap map[PackagePath]*packages.Package
	useMap    map[PackagePath]bool
	dedupMap  map[string]bool
}

func (i *importCleaner) imports() {
	walkImportGroup(i.file, func(d *ast.GenDecl) {
		for _, spec := range d.Specs {
			s := spec.(*ast.ImportSpec)
			path := SpecPath(s)
			pkg, ok := i.m.All[path]
			assert(ok, path+" not loaded, missing matcher.WithLoadDepts() ?")
			i.importMap[path] = pkg
		}
	})
}

func (i *importCleaner) uses() {
	walkImportGroup(i.file, func(d *ast.GenDecl) {
		for _, spec := range d.Specs {
			s := spec.(*ast.ImportSpec)
			path := SpecPath(s)
			pkgName := i.tryGetPkgName(path)
			importedName := importName(s, pkgName)
			if staticUsesImport(i.file, s, importedName) == used {
				i.useMap[path] = true
			}
		}
	})

	// todo import "C" cgo/fack
	// todo mark dot-import, record all export symbol

	// can't mark type alias, ref testdata/import/typealias1.txt
	astutil.Apply(i.file, nil, func(c *astutil.Cursor) bool {
		// skip import spec
		if _, is := c.Parent().(*ast.ImportSpec); is {
			return true
		}
		switch n := c.Node().(type) {
		case *ast.Ident:
			// ignore selector.Sel
			if sel, ok := c.Parent().(*ast.SelectorExpr); ok {
				if sel.Sel == n {
					return true
				}
			}

			// only to mark top ident
			if n.Obj == nil {
				i.markUses(n)
			}
		case *ast.Field:
			for _, name := range n.Names {
				i.markUses(name)
			}
		}
		return true
	})
}

func (i *importCleaner) markUses(id *ast.Ident) {
	var pkg *types.Package

	switch obj := i.m.ObjectOf(id).(type) {
	case nil:
		return
	case *types.PkgName: // pkgName.xxx
		pkg = obj.Imported()
	default: // dot-import
		// skip self pkg
		if obj.Pkg() == i.m.Package {
			return
		}
		pkg = obj.Pkg()
	}
	if pkg == nil {
		return // illegal
	}

	path := pkg.Path()
	imported := i.importMap[path]
	if imported == nil {
		// ref testdata/import/typealias1.txt
		return
	}
	assert(imported != nil, "missing pkg: "+path)
	assert(pkg == imported.Types, "illegal state")
	i.useMap[path] = true
}

func (i *importCleaner) rebuild() {
	walkImportGroup(i.file, func(d *ast.GenDecl) {
		specs := make([]ast.Spec, 0, len(d.Specs))
		for _, spec := range d.Specs {
			s := spec.(*ast.ImportSpec)
			name := SpecName(s)
			path := SpecPath(s)
			if i.keepImport(name, path) {
				namePath := Fmt(s)
				s := &ast.ImportSpec{Path: &ast.BasicLit{Value: namePath, Kind: d.Tok}}
				specs = append(specs, s)
			}
		}

		d.Specs = specs
	})

	i.clearEmptyDecl()
}

func (i *importCleaner) keepImport(name, path string) bool {
	id := name + "#" + path
	use := i.useMap[path]
	dup := i.dedupMap[id]
	if !use || dup {
		return false
	}
	i.dedupMap[id] = true
	return true
}

func (i *importCleaner) clearEmptyDecl() {
	xs := make([]ast.Decl, 0, len(i.file.Decls))
	for _, decl := range i.file.Decls {
		d, ok := decl.(*ast.GenDecl)
		if !ok || d.Tok != token.IMPORT || len(d.Specs) > 0 {
			xs = append(xs, decl)
		}
	}
	i.file.Decls = xs
}

func (i *importCleaner) tryGetPkgName(path string) string {
	pkg := i.m.All[path]
	if pkg != nil {
		return pkg.Name
	} else {
		// guess
		lastSlash := strings.LastIndex(path, "/")
		if lastSlash == -1 {
			return path
		} else {
			return path[lastSlash+1:]
		}
	}
}

type useImport int

const (
	unknown useImport = iota + 1
	used
	unused
)
