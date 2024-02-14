package astmatcher

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"github.com/goghcrow/go-loader"
	"github.com/goghcrow/go-matcher"
	. "github.com/goghcrow/go-matcher/combinator"
	"github.com/goghcrow/go-matcher/example"
	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/txtar"
)

const module = "github.com/goghcrow/go-ast-matcher/testdata/match"

var matchPatterns = map[string]func(am ASTMatcher) ast.Node{
	"ident/int": func(am ASTMatcher) ast.Node {
		m := am.Matcher
		// Can not only use Type to match
		// var id int
		// The type of id is int, and the type of int itself is also int
		// Match the identifier with typing int and naming id
		return Bind(m,
			"var",
			And(m,
				IdentNameOf(m, "id"),
				TypeIdentical[IdentPattern](m, am.Loader.MustLookupType("int")),
			),
		)
	},
	"ident/wildcard":              func(am ASTMatcher) ast.Node { return example.PatternOfWildcardIdent(am.Matcher) },
	"ident/gen_decl_var":          func(am ASTMatcher) ast.Node { return example.PatternOfVarDecl(am.Matcher) },
	"ident/gen_decl_const":        func(am ASTMatcher) ast.Node { return example.PatternOfConstDecl(am.Matcher) },
	"ident/val_spec":              func(am ASTMatcher) ast.Node { return example.PatternOfValSpec(am.Matcher) },
	"ident/func_name":             func(am ASTMatcher) ast.Node { return example.PatternOfAllFuncOrMethodDeclName(am.Matcher) },
	"append_with_no_value/append": func(am ASTMatcher) ast.Node { return example.PatternOfAppendWithNoValue(am.Matcher) },
	"call/println": func(am ASTMatcher) ast.Node {
		return example.PatternOfCallFunOrMethodWithSpecName("println", am.Matcher)
	},
	"atomic/adder":             func(am ASTMatcher) ast.Node { return example.PatternOfCallAtomicAdder(am.Matcher) },
	"atomic/swap_struct_field": func(am ASTMatcher) ast.Node { return example.PatternOfAtomicSwapStructField(am.Matcher) },
	"basiclit/import":          func(am ASTMatcher) ast.Node { return example.PatternOfAllImportSpec(am.Matcher) },
	"basiclit/tag":             func(am ASTMatcher) ast.Node { return example.PatternOfStructFieldWithJsonTag(am.Matcher) },
	"nil/composite_lit":        func(am ASTMatcher) ast.Node { return example.PatternOfLitVal(am.Matcher) },

	// callback match
	"callee/callee":              func(am ASTMatcher) ast.Node { return example.PatternOfCallee(am.Matcher) },
	"callee/builtin_callee":      func(am ASTMatcher) ast.Node { return example.PatternOfBuiltinCallee(am.Matcher) },
	"callee/var_callee":          func(am ASTMatcher) ast.Node { return example.PatternOfVarCallee(am.Matcher) },
	"callee/funcOrMethod_callee": func(am ASTMatcher) ast.Node { return example.PatternOfFuncOrMethodCallee(am.Matcher) },
	"callee/func_callee":         func(am ASTMatcher) ast.Node { return example.PatternOfFuncCallee(am.Matcher) },
	"callee/method_callee":       func(am ASTMatcher) ast.Node { return example.PatternOfMethodCallee(am.Matcher) },
	"callee/static_callee":       func(am ASTMatcher) ast.Node { return example.PatternOfStaticCallee(am.Matcher) },
	"callee/iface_callee":        func(am ASTMatcher) ast.Node { return example.PatternOfIfaceCalleeOf(am.Matcher) },

	// exactly match
	"callee/builtin_callee_append": func(am ASTMatcher) ast.Node {
		return BuiltinCallee(am.Matcher, "append")
	},
	"callee/func_callee_callBuiltin": func(am ASTMatcher) ast.Node {
		qualified := module + "." + "callBuiltin"
		funObj := am.Loader.Lookup(qualified)
		assert(funObj != nil, qualified+" not found")
		return FuncCallee(am.Matcher, funObj, "callBuiltin")
	},
	"callee/method_callee_A_Method": func(am ASTMatcher) ast.Node {
		qualified := module + "." + "A"
		tyObj := am.Loader.Lookup(qualified)
		assert(tyObj != nil, qualified+" not found")
		return MethodCallee(am.Matcher, tyObj, "Method", false)
	},
	"callee/iface_callee_Show_String": func(am ASTMatcher) ast.Node {
		qualified := module + "." + "Show"
		ifaceObj := am.Loader.Lookup(qualified)
		assert(ifaceObj != nil, qualified+" not found")
		return IfaceCallee(am.Matcher, ifaceObj, "String")
	},

	"rest/exprs": func(am ASTMatcher) ast.Node {
		ctxIface := am.Loader.MustLookup("context.Context").Type().Underlying().(*types.Interface)
		return example.PatternOfSecondArgIsCtx(am.Matcher, ctxIface)
	},
	"rest/stmts": func(am ASTMatcher) ast.Node { return example.PatternOfSecondStmtIsIf(am.Matcher) },
}

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
		err = os.WriteFile(filepath.Join(dir, "go.mod"), []byte("module "+module+"\n"), 0666)
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

				l := loader.MustNew(dir, loader.WithLoadDepts())
				m := matcher.New()
				am := New(l, m)

				l.VisitAllFiles(func(file *loader.File) {
					t.Log(patternName)
					pattern := matchPatterns[patternName](am)

					m.Match(file.Pkg, pattern, file.File, func(c matcher.Cursor, ctx *matcher.MatchCtx) {
						n := c.Node()
						parent := c.Parent()
						bind := ctx.Binds["var"]
						assert(n == ctx.Stack[0], "")
						assert(parent == ctx.Stack[1], "")
						cur := ctx.ShowNode(c.Node())
						if cur != "" {
							stdout += "node: " + cur + "\n"
						}
						if bind != nil {
							pos := basePositionOf(ctx.Pkg.Fset, bind)
							if pos != "" {
								stdout += "pos : " + pos + "\n"
							}
							stdout += "var : " + ctx.ShowNode(bind) + "\n"
						}
						stdout += "\n"
					})
				})

				have := stdout
				want := string(wantStdout.Data)
				if strings.TrimSpace(want) != strings.TrimSpace(have) {
					t.Errorf("stdout:\n")
					println(have)
					t.Errorf("want:\n")
					println(want)
				}
			})
		}
	}
}

//goland:noinspection NonAsciiCharacters
var rewriteTests = map[string]struct {
	match   func(m *Matcher) ast.Node
	rewrite func(Cursor, *MatchCtx, *loader.File)
}{
	// errors.New(fmt.Sprintf(...)) -> fmt.Errorf(...)
	"S1028.txt": {
		match: func(m *matcher.Matcher) ast.Node {
			funCall := FuncCalleeOf(m, func(ctx *matcher.MatchCtx, t *types.Func) bool { return true })
			errors۰New := &ast.SelectorExpr{
				X:   &ast.Ident{Name: "errors"},
				Sel: &ast.Ident{Name: "New"},
			}
			fmt۰Sprintf := &ast.SelectorExpr{
				X:   &ast.Ident{Name: "fmt"},
				Sel: &ast.Ident{Name: "Sprintf"},
			}
			return And(m, funCall,
				matcher.PatternOf[CallExprPattern](m, &ast.CallExpr{
					Fun: errors۰New,
					Args: []ast.Expr{
						And(m, funCall,
							matcher.PatternOf[CallExprPattern](m, &ast.CallExpr{
								Fun:  fmt۰Sprintf,
								Args: matcher.MkVar[matcher.ExprsPattern](m, "args"),
							}),
						),
					},
				}),
			)
		},
		rewrite: func(c matcher.Cursor, ctx *matcher.MatchCtx, f *loader.File) {
			// todo: need to optimise imports
			astutil.AddImport(ctx.Pkg.Fset, f.File, "fmt")
			c.Replace(&ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   &ast.Ident{Name: "fmt"},
					Sel: &ast.Ident{Name: "Errorf"},
				},
				Args: ctx.Binds["args"].(matcher.ExprsNode),
			})
		},
	},
	// println -> fmt.Println
	"println.txt": {
		match: func(m *matcher.Matcher) ast.Node {
			return &ast.CallExpr{
				Fun: And(m,
					IdentNameOf(m, "println"),
					IsBuiltin(m),
				),
				Args: matcher.MkVar[matcher.ExprsPattern](m, "args"),
			}
		},
		rewrite: func(c matcher.Cursor, ctx *matcher.MatchCtx, f *loader.File) {
			// todo: need to optimise imports
			astutil.AddImport(ctx.Pkg.Fset, f.File, "fmt")
			c.Replace(&ast.CallExpr{
				Fun: &ast.SelectorExpr{
					X:   &ast.Ident{Name: "fmt"},
					Sel: &ast.Ident{Name: "Println"},
				},
				Args: ctx.Binds["args"].(matcher.ExprsNode),
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

		l := loader.MustNew(dir)
		m := matcher.New()
		l.VisitAllFiles(func(f *loader.File) {
			name := filepath.Base(f.Filename)
			t.Run(testFile+"/"+name, func(t *testing.T) {
				m.Match(f.Pkg, testCase.match(m), f.File, func(c matcher.Cursor, ctx *matcher.MatchCtx) {
					testCase.rewrite(c, ctx, f)
				})
				have := l.FormatFile(f.File)
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

func fatalIf(t *testing.T, err error) {
	if err != nil {
		t.Fatal(err)
	}
}

func panicIf(err error) {
	if err != nil {
		panic(err)
	}
}

func assert(b bool, msg string) {
	if !b {
		panic(msg)
	}
}

func basePositionOf(fset *token.FileSet, n ast.Node) string {
	pos := fset.Position(n.Pos())
	s := fmt.Sprintf("%s:%d:%d", filepath.Base(pos.Filename), pos.Line, pos.Column)
	if s == ".:0:0" {
		return ""
	}
	return s
}

func Diff(oldName string, old []byte, newName string, new []byte) []byte {
	writeTempFile := func(data []byte) (string, error) {
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

	f1, err := writeTempFile(old)
	panicIf(err)
	//goland:noinspection GoUnhandledErrorResult
	defer os.Remove(f1)

	f2, err := writeTempFile(new)
	panicIf(err)
	//goland:noinspection GoUnhandledErrorResult
	defer os.Remove(f2)

	data, err := exec.Command("diff", "-u", f1, f2).CombinedOutput()
	if err != nil && len(data) == 0 {
		panicIf(err)
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
