package astmatcher

import (
	"fmt"
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"strconv"
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
	pos := ShowPos(fset, n)
	s := fmt.Sprintf("%s:%d:%d", filepath.Base(pos.Filename), pos.Line, pos.Column)
	if s == ".:0:0" {
		return ""
	}
	return s
}

var patterns = map[string]func(m *Matcher) ast.Node{
	"ident/wildcard": func(m *Matcher) ast.Node {
		return Bind(m,
			"var",
			Wildcard[IdentPattern](m),
		)
	},
	"basiclit/import": func(m *Matcher) ast.Node {
		return &ast.ImportSpec{
			Path: MkVar[BasicLitPattern](m, "var"),
		}
	},
	"ident/gen_decl_var": func(m *Matcher) ast.Node {
		return &ast.GenDecl{
			Tok:   token.VAR, // IMPORT, CONST, TYPE, or VAR
			Specs: MkVar[SpecsPattern](m, "var"),
		}
	},
	"ident/gen_decl_const": func(m *Matcher) ast.Node {
		return &ast.GenDecl{
			Tok:   token.CONST, // IMPORT, CONST, TYPE, or VAR
			Specs: MkVar[SpecsPattern](m, "var"),
		}
	},
	"ident/val_spec": func(m *Matcher) ast.Node {
		return &ast.ValueSpec{
			Names: MkVar[IdentsPattern](m, "var"),
			Type:  TypeIdentical[ExprPattern](m, m.MustLookupType("int")),
		}
	},
	"ident/int": func(m *Matcher) ast.Node {
		// Can not only use Type to match
		// var id int
		// The type of id is int, and the type of int itself is also int
		// Match the identifier with typing int and naming id
		return Bind(m,
			"var",
			And(m,
				IdentEqual(m, "id"),
				TypeIdentical[IdentPattern](m, m.MustLookupType("int")),
			),
		)
	},
	"ident/func_name": func(m *Matcher) ast.Node {
		return &ast.FuncDecl{
			Name: MkVar[IdentPattern](m, "var"),
		}
	},
	"basiclit/tag": func(m *Matcher) ast.Node {
		return &ast.Field{
			Tag: Bind(m,
				"var",
				MkPattern[BasicLitPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
					if n == nil /*ast.Node(nil)*/ {
						return false
					}
					tagLit, _ := n.(*ast.BasicLit)
					if tagLit == nil {
						return false
					}
					assert(tagLit.Kind == token.STRING, "")
					tag, _ := strconv.Unquote(tagLit.Value)
					return strings.Contains(tag, "json")
				}),
			),
		}
	},
	"append_with_no_value/append": PatternOfAppendWithNoValue,
}

func TestRun(t *testing.T) {
	files, err := filepath.Glob("testdata/*.txt")
	fatalIf(t, err)

	for _, file := range files {
		filename := filepath.Base(file)
		ext := filepath.Ext(file)
		testName := filename[:len(filename)-len(ext)]

		t.Run(filepath.Base(file), func(t *testing.T) {
			t.Log(file)
			ar, err := txtar.ParseFile(file)
			fatalIf(t, err)

			dir := t.TempDir()
			err = os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module m\n"), 0666)
			fatalIf(t, err)

			var testCase = map[string]txtar.File{}
			for _, file := range ar.Files {
				if filepath.Ext(file.Name) == ".stdout" {
					testCase[file.Name[:len(file.Name)-len(".stdout")]] = file
					continue
				}
				filename := filepath.Join(dir, file.Name)
				err = os.MkdirAll(filepath.Dir(filename), 0777)
				fatalIf(t, err)
				err = os.WriteFile(filename, file.Data, 0666)
				fatalIf(t, err)
			}

			// baseName := strings.TrimSpace(string(ar.Comment))
			for patternName, wantStdout := range testCase {
				stdout := ""
				patternName = testName + "/" + patternName

				m := NewMatcher(dir, []string{PatternAll})
				m.Walk(func(m *Matcher, file *ast.File) {
					t.Log(patternName)
					pattern := patterns[patternName](m)
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
			}
		})
	}
}

// Bind[NodePattern](m, "var", MkPattern[NodePattern](m))
// Bind[StmtPattern](m, "var", MkPattern[StmtPattern](m))
// Bind[ExprPattern](m, "var", MkPattern[ExprPattern](m))
// Bind[DeclPattern](m, "var", MkPattern[DeclPattern](m))
// Bind[IdentPattern](m, "var", MkPattern[IdentPattern](m))
// Bind[FieldPattern](m, "var", MkPattern[FieldPattern](m))
// Bind[FieldListPattern](m, "var", MkPattern[FieldListPattern](m))
// Bind[CallExprPattern](m, "var", MkPattern[CallExprPattern](m))
// Bind[FuncTypePattern](m, "var", MkPattern[FuncTypePattern](m))
// Bind[BlockStmtPattern](m, "var", MkPattern[BlockStmtPattern](m))
// Bind[TokenPattern](m, "var", MkPattern[TokenPattern](m))
// Bind[BasicLitPattern](m, "var", MkPattern[BasicLitPattern](m))
// Bind[StmtsPattern](m, "var", MkPattern[StmtsPattern](m))
// Bind[ExprsPattern](m, "var", MkPattern[ExprsPattern](m))
// Bind[IdentsPattern](m, "var", MkPattern[IdentsPattern](m))
// Bind[FieldsPattern](m, "var", MkPattern[FieldsPattern](m))
