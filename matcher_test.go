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
		return BindWith(m,
			"var",
			Wildcard[IdentPattern](m),
		)
	},
	"basiclit/import": func(m *Matcher) ast.Node {
		return &ast.ImportSpec{
			// Name: , // 只有 nil 默认是通配符,
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
		// 不能只用 Type 匹配
		// var id int
		// id 的类型是 int, int 本身的类型也是 int
		// 匹配 int 类型, 名字为 id 的标识符
		return BindWith(m,
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
			Tag: BindWith(m,
				"var",
				MkPattern[BasicLitPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
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
				WalkDir(dir, func(filename string, m *Matcher, file *ast.File) {
					patternName = testName + "/" + patternName
					t.Log(patternName)
					m.Match(patterns[patternName](m), file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
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
							pos := basePositionOf(m.FileSet, bind)
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

// BindWith[NodePattern](m, "var", MkPattern[NodePattern](m))
// BindWith[StmtPattern](m, "var", MkPattern[StmtPattern](m))
// BindWith[ExprPattern](m, "var", MkPattern[ExprPattern](m))
// BindWith[DeclPattern](m, "var", MkPattern[DeclPattern](m))
// BindWith[IdentPattern](m, "var", MkPattern[IdentPattern](m))
// BindWith[FieldPattern](m, "var", MkPattern[FieldPattern](m))
// BindWith[FieldListPattern](m, "var", MkPattern[FieldListPattern](m))
// BindWith[CallExprPattern](m, "var", MkPattern[CallExprPattern](m))
// BindWith[FuncTypePattern](m, "var", MkPattern[FuncTypePattern](m))
// BindWith[BlockStmtPattern](m, "var", MkPattern[BlockStmtPattern](m))
// BindWith[TokenPattern](m, "var", MkPattern[TokenPattern](m))
// BindWith[BasicLitPattern](m, "var", MkPattern[BasicLitPattern](m))
// BindWith[StmtsPattern](m, "var", MkPattern[StmtsPattern](m))
// BindWith[ExprsPattern](m, "var", MkPattern[ExprsPattern](m))
// BindWith[IdentsPattern](m, "var", MkPattern[IdentsPattern](m))
// BindWith[FieldsPattern](m, "var", MkPattern[FieldsPattern](m))
