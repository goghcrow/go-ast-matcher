package matcher

import (
	"fmt"
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/txtar"
)

func fatalIf(t *testing.T, err error) {
	if err != nil {
		t.Fatal(err)
	}
}

func basePositionOf(fset *token.FileSet, n ast.Node) string {
	pos := PosOfNode(fset, n)
	s := fmt.Sprintf("%s:%d:%d", filepath.Base(pos.Filename), pos.Line, pos.Column)
	if s == ".:0:0" {
		return ""
	}
	return s
}

var matchPatterns = map[string]func(m *Matcher) ast.Node{
	"ident/int": func(m *Matcher) ast.Node {
		// Can not only use Type to match
		// var id int
		// The type of id is int, and the type of int itself is also int
		// Match the identifier with typing int and naming id
		return Bind(m,
			"var",
			And(m,
				IdentNameIs(m, "id"),
				TypeIdentical[IdentPattern](m, m.MustLookupType("int")),
			),
		)
	},
	"ident/wildcard":              PatternOfWildcardIdent,
	"ident/gen_decl_var":          PatternOfVarDecl,
	"ident/gen_decl_const":        PatternOfConstDecl,
	"ident/val_spec":              PatternOfValSpec,
	"ident/func_name":             PatternOfAllFuncOrMethodDeclName,
	"append_with_no_value/append": PatternOfAppendWithNoValue,
	"call/println":                MkPatternOfCallFunOrMethodWithSpecName("println"),
	"atomic/adder":                PatternOfCallAtomicAdder,
	"atomic/swap_struct_field":    PatternOfAtomicSwapStructField,
	"basiclit/import":             PatternOfAllImportSpec,
	"basiclit/tag":                PatternOfStructFieldWithJsonTag,
	"nil/composite_lit":           PatternOfLitVal,

	// callback match
	"callee/callee":              PatternOfCallee,
	"callee/builtin_callee":      PatternOfBuiltinCallee,
	"callee/var_callee":          PatternOfVarCallee,
	"callee/funcOrMethod_callee": PatternOfFuncOrMethodCallee,
	"callee/func_callee":         PatternOfFuncCallee,
	"callee/method_callee":       PatternOfMethodCallee,
	"callee/static_callee":       PatternOfStaticCallee,
	"callee/iface_callee":        PatternOfIfaceCalleeOf,

	// exactly match
	"callee/builtin_callee_append":    PatternOfBuiltin_append,
	"callee/func_callee_callBuiltin":  PatternOfFunc_callBuiltin,
	"callee/method_callee_A_Method":   PatternOfMethod_A۰Method,
	"callee/iface_callee_Show_String": PatternOfIface_Show۰String,
}

const (
	Module = "github.com/goghcrow/go-ast-matcher/testdata/match"
)

func TestMatchRun(t *testing.T) {
	files, err := filepath.Glob("testdata/match/*.txt")
	fatalIf(t, err)

	for _, testFile := range files {
		filename := filepath.Base(testFile)
		ext := filepath.Ext(testFile)
		testName := filename[:len(filename)-len(ext)]

		t.Log(testFile)
		ar, err := txtar.ParseFile(testFile)
		fatalIf(t, err)

		dir := t.TempDir()
		err = os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module "+Module+"\n"), 0666)
		fatalIf(t, err)

		var arFiles = map[string]txtar.File{}
		for _, f := range ar.Files {
			if filepath.Ext(f.Name) == ".stdout" {
				arFiles[f.Name[:len(f.Name)-len(".stdout")]] = f
				continue
			}
			filename := filepath.Join(dir, f.Name)
			err = os.MkdirAll(filepath.Dir(filename), 0777)
			fatalIf(t, err)
			err = os.WriteFile(filename, f.Data, 0666)
			fatalIf(t, err)
		}

		// baseName := strings.TrimSpace(string(ar.Comment))
		for patternName, wantStdout := range arFiles {
			patternName = testName + "/" + patternName
			t.Run(patternName, func(t *testing.T) {
				stdout := ""

				m := NewMatcher(dir, []string{PatternAll})
				m.VisitAllFiles(func(m *Matcher, file *ast.File) {
					t.Log(patternName)
					pattern := matchPatterns[patternName](m)
					m.MatchNode(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
						n := c.Node()
						parent := c.Parent()
						bind := binds["var"]
						assert(n == stack[0], "")
						assert(parent == stack[1], "")
						cur := m.ShowNode(c.Node())
						if cur != "" {
							stdout += "node: " + cur + "\n"
						}
						if bind != nil {
							pos := basePositionOf(m.FSet, bind)
							if pos != "" {
								stdout += "pos : " + pos + "\n"
							}
							stdout += "var : " + m.ShowNode(bind) + "\n"
						}
						stdout += "\n"
					})
				})

				have := stdout
				want := string(wantStdout.Data)
				if strings.TrimSpace(want) != strings.TrimSpace(have) {
					t.Errorf("stdout:\n%s", have)
					t.Errorf("want:\n%s", want)
				}
			})
		}
	}
}
