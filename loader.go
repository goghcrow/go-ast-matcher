package matcher

import (
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"strings"

	"golang.org/x/tools/go/packages"
)

type (
	PackagePath = string
	FileName    = string
	GeneratedBy = string
)

var (
	PatternAll = []string{"./..."}
)

const (
	PatternStd = "std"

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
		packages.NeedTypesSizes |
		packages.NeedModule
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Loader ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type LoadFlags struct {
	Gopath      string
	BuildTag    string // comma-separated list of extra build tags (see: go help buildconstraint)
	Test        bool   // load includes tests packages
	LoadDepts   bool   // load all dependencies, may heavily slow
	PrintErrors bool
}

type Loader struct {
	Cfg       *packages.Config
	FSet      *token.FileSet
	Init      []*packages.Package
	All       map[PackagePath]*packages.Package
	Generated map[string]string
}

func NewLoader() *Loader {
	return &Loader{
		FSet:      token.NewFileSet(),
		Init:      []*packages.Package{},
		All:       map[PackagePath]*packages.Package{},
		Generated: map[FileName]GeneratedBy{},
	}
}

// Load Packages
// dir: run the build system's query tool
// patterns: (see: packages.Load)
func (d *Loader) Load(dir string, patterns []string, flags LoadFlags) {
	mode := LoadMode
	if flags.LoadDepts {
		mode |= LoadDepts
	}

	dir, err := filepath.Abs(dir)
	panicIfErr(err)

	d.Cfg = &packages.Config{
		Fset:       d.FSet,
		Mode:       mode,
		Tests:      flags.Test,
		Dir:        dir,
		BuildFlags: []string{"-tags=" + flags.BuildTag},
	}
	if flags.Gopath != "" {
		d.Cfg.Env = append(os.Environ(), "GOPATH="+flags.Gopath)
	}
	patterns = append(patterns)
	d.Init, err = packages.Load(d.Cfg, patterns...)
	panicIfErr(err)

	if len(d.Init) == 0 {
		errLog("no packages found")
	}

	packages.Visit(d.Init, nil, func(p *packages.Package) {
		if flags.PrintErrors {
			for _, err := range p.Errors {
				errLog(err)
			}
		}

		// Gather all packages
		d.All[p.PkgPath] = p

		// Gather generated files
		for _, file := range p.Syntax {
			if gen, is := Generator(file); is {
				f := p.Fset.File(file.Pos())
				d.Generated[f.Name()] = gen
			}
		}
	})
}

const GeneratedComment = "// Code generated by %s DO NOT EDIT.\n"

// ↓↓↓↓↓↓↓↓↓↓↓↓ copy from go1.21 ast.IsGenerated ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// IsGenerated reports whether the file was generated by a program,
// not handwritten, by detecting the special comment described
// at https://go.dev/s/generatedcode.
//
// The syntax tree must have been parsed with the ParseComments flag.
// Example:
//
//	f, err := parser.ParseFile(fset, filename, src, parser.ParseComments|parser.PackageClauseOnly)
//	if err != nil { ... }
//	gen := ast.IsGenerated(f)
//
// copy from go1.21 ast.IsGenerated
func IsGenerated(file *ast.File) bool {
	_, ok := Generator(file)
	return ok
}

// Generator copy from go1.21 ast.IsGenerated
func Generator(file *ast.File) (by string, is bool) {
	for _, group := range file.Comments {
		for _, comment := range group.List {
			if comment.Pos() > file.Package {
				break // after package declaration
			}
			// opt: check Contains first to avoid unnecessary array allocation in Split.
			const prefix = "// Code generated "
			if strings.Contains(comment.Text, prefix) {
				for _, line := range strings.Split(comment.Text, "\n") {
					if rest, ok := strings.CutPrefix(line, prefix); ok {
						if gen, ok := strings.CutSuffix(rest, " DO NOT EDIT."); ok {
							return gen, true
						}
					}
				}
			}
		}
	}
	return "", false
}
