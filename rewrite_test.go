package matcher

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/types"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/txtar"
)

//goland:noinspection NonAsciiCharacters
var rewriteTests = map[string]struct {
	match   func(m *Matcher) ast.Node
	rewrite Callback
}{
	// errors.New(fmt.Sprintf(...)) -> fmt.Errorf(...)
	"S1028.txt": {
		match: func(m *Matcher) ast.Node {
			funCall := FuncCalleeOf(m, func(t *types.Func) bool { return true })
			errors۰New := &ast.SelectorExpr{
				X:   &ast.Ident{Name: "errors"},
				Sel: &ast.Ident{Name: "New"},
			}
			fmt۰Sprintf := &ast.SelectorExpr{
				X:   &ast.Ident{Name: "fmt"},
				Sel: &ast.Ident{Name: "Sprintf"},
			}
			return And(m, funCall,
				PatternOf[CallExprPattern](m, &ast.CallExpr{
					Fun: errors۰New,
					Args: []ast.Expr{
						And(m, funCall,
							PatternOf[CallExprPattern](m, &ast.CallExpr{
								Fun:  fmt۰Sprintf,
								Args: MkVar[ExprsPattern](m, "args"),
							}),
						),
					},
				}),
			)
		},
		rewrite: func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			// todo: need to optimise imports
			astutil.AddImport(m.FSet, m.File, "fmt")
			c.Replace(&ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   &ast.Ident{Name: "fmt"},
					Sel: &ast.Ident{Name: "Errorf"},
				},
				Args: binds["args"].(ExprsNode),
			})
		},
	},
	// println -> fmt.Println
	"println.txt": {
		match: func(m *Matcher) ast.Node {
			return &ast.CallExpr{
				Fun: And(m,
					IdentNameOf(m, "println"),
					IsBuiltin(m),
				),
				Args: MkVar[ExprsPattern](m, "args"),
			}
		},
		rewrite: func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			// todo: need to optimise imports
			astutil.AddImport(m.FSet, m.File, "fmt")
			c.Replace(&ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   &ast.Ident{Name: "fmt"},
					Sel: &ast.Ident{Name: "Println"},
				},
				Args: binds["args"].(ExprsNode),
			})
		},
	},
}

func TestRewriteRun(t *testing.T) {
	files, err := filepath.Glob("testdata/rewrite/*.txt")
	fatalIf(t, err)

	for _, testFile := range files {
		t.Log(testFile)
		testCase := rewriteTests[filepath.Base(testFile)]

		ar, err := txtar.ParseFile(testFile)
		fatalIf(t, err)

		dir := t.TempDir()
		err = os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module m\n"), 0666)
		fatalIf(t, err)

		var wants = map[string]string{}
		for _, f := range ar.Files {
			if filepath.Ext(f.Name) == ".stdout" {
				wants[f.Name] = string(f.Data)
				continue
			}

			filename := filepath.Join(dir, f.Name)
			err = os.MkdirAll(filepath.Dir(filename), 0777)
			fatalIf(t, err)
			err = os.WriteFile(filename, f.Data, 0666)
			fatalIf(t, err)
		}

		m := NewMatcher(dir, PatternAll)
		m.VisitAllFiles(func(m *Matcher, file *ast.File) {
			name := filepath.Base(m.Filename)
			t.Run(testFile+"/"+name, func(t *testing.T) {
				m.MatchNode(testCase.match(m), file, testCase.rewrite)
				have := m.FormatFile(file)
				want := wants[name+".stdout"]
				if strings.TrimSpace(want) != strings.TrimSpace(have) {
					diff := Diff("have.go", []byte(have), "want.go", []byte(want))
					fmt.Println(string(diff))
					t.Errorf("stdout:\n%s", have)
					t.Errorf("want:\n%s", want)
				}
			})
		})
	}
}

func Diff(oldName string, old []byte, newName string, new []byte) []byte {
	f1, err := writeTempFile(old)
	panicIfErr(err)
	//goland:noinspection GoUnhandledErrorResult
	defer os.Remove(f1)

	f2, err := writeTempFile(new)
	panicIfErr(err)
	//goland:noinspection GoUnhandledErrorResult
	defer os.Remove(f2)

	data, err := exec.Command("diff", "-u", f1, f2).CombinedOutput()
	if err != nil && len(data) == 0 {
		panicIfErr(err)
	}

	if len(data) == 0 {
		return nil
	}

	i := bytes.IndexByte(data, '\n')
	if i < 0 {
		return data
	}
	j := bytes.IndexByte(data[i+1:], '\n')
	if j < 0 {
		return data
	}
	start := i + 1 + j + 1
	if start >= len(data) || data[start] != '@' {
		return data
	}

	return append([]byte(fmt.Sprintf("diff %s %s\n--- %s\n+++ %s\n", oldName, newName, oldName, newName)), data[start:]...)
}

func writeTempFile(data []byte) (string, error) {
	file, err := os.CreateTemp("", "diff")
	if err != nil {
		return "", err
	}
	_, err = file.Write(data)
	if err1 := file.Close(); err == nil {
		err = err1
	}
	if err != nil {
		_ = os.Remove(file.Name())
		return "", err
	}
	return file.Name(), nil
}
