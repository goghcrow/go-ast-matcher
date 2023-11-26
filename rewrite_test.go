package astmatcher

import (
	"bytes"
	"fmt"
	"go/ast"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/txtar"
)

var rewriteTests = map[string]struct {
	pattern  func(m *Matcher) ast.Node
	callback Callback
}{
	"println.txt": {
		pattern: func(m *Matcher) ast.Node {
			return &ast.CallExpr{
				Fun: And(m,
					IdentNameEqual(m, "println"),
					IsBuiltin(m),
				),
				Args: MkVar[ExprsPattern](m, "args"),
			}
		},
		callback: func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			// need to check if fmt has been imported
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

		m := NewMatcher(dir, []string{PatternAll})
		m.Walk(func(m *Matcher, file *ast.File) {
			name := filepath.Base(m.Filename)
			t.Run(testFile+"/"+name, func(t *testing.T) {
				m.MatchNode(testCase.pattern(m), file, testCase.callback)
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
		os.Remove(file.Name())
		return "", err
	}
	return file.Name(), nil
}
