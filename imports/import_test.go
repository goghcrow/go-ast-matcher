package imports

import (
	"fmt"
	"go/ast"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/goghcrow/go-ast-matcher"
	"golang.org/x/tools/txtar"
)

func TestOptimizeImports(t *testing.T) {
	files, err := filepath.Glob("testdata/import/*.txt")
	fatalIf(t, err)

	for _, testFile := range files {
		t.Log(testFile)
		ar, err := txtar.ParseFile(testFile)
		fatalIf(t, err)

		dir := t.TempDir()
		err = os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module a.b.c\n"), 0666)
		fatalIf(t, err)

		var wants = map[string]string{}
		for _, f := range ar.Files {
			if filepath.Ext(f.Name) == ".optimized" {
				wants[f.Name] = string(f.Data)
				continue
			}

			filename := filepath.Join(dir, f.Name)
			err = os.MkdirAll(filepath.Dir(filename), 0777)
			fatalIf(t, err)
			err = os.WriteFile(filename, f.Data, 0666)
			fatalIf(t, err)
		}

		m := matcher.NewMatcher(dir, matcher.PatternAll, matcher.WithLoadDepts()) // mustLoadDepts
		m.VisitAllFiles(func(m *Matcher, file *ast.File) {
			name := m.Filename[len(dir)+1:]

			want, ok := wants[name+".optimized"]
			if !ok {
				return
			}

			t.Run(testFile+"/"+name, func(t *testing.T) {
				Optimize(m, file, "a.b.c/project", []string{"a.b.c/company"})
				have := m.FormatFile()

				if strings.TrimSpace(want) != strings.TrimSpace(have) {
					diff := matcher.Diff("have.go", []byte(have), "want.go", []byte(want))
					fmt.Println(string(diff))
					t.Errorf("stdout:\n")
					println(have)
					t.Errorf("want:\n")
					println(want)
				}
			})
		})
	}
}

func fatalIf(t *testing.T, err error) {
	if err != nil {
		t.Fatal(err)
	}
}
