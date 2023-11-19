## [WIP] go-ast-matcher

```go
package example

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"testing"

	"github.com/goghcrow/go-ansi"
	. "github.com/goghcrow/go-ast-matcher"
	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

func WalkDir(dir string, f func(filename string, m *Matcher, file *ast.File)) {
	fset, pkgs := LoadDir(dir)
	if packages.PrintErrors(pkgs) > 0 {
		log.Fatalf("packages contain errors")
	}
	all := map[string]*packages.Package{}
	packages.Visit(pkgs, nil, func(p *packages.Package) { all[p.ID] = p })
	for _, pkg := range pkgs {
		m := NewMatcher(fset, all, pkg)
		for i, filename := range pkg.CompiledGoFiles {
			f(filename, m, pkg.Syntax[i])
		}
	}
}

func TestGrepGormTablerTableName(t *testing.T) {
	projectDir := ""
	WalkDir(projectDir, func(filename string, m *Matcher, file *ast.File) {
		// types.Named -> types.Struct
		// gormDB := m.MustLookupType("gorm.io/gorm.DB").Underlying().(*types.Struct)
		// types.Named -> types.Interface
		gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)

		pattern := &ast.FuncDecl{
			// type of recv must implements gorm.Tabler
			Recv: MethodRecv(m, func(ident *ast.Ident, ty ast.Expr) bool {
				return TypeImplements(m.TypeOf(ty), gormTabler)
			}),
			Name: IdentEqual(m, "TableName"), //  method name must be "TableName"
			// Type: Wildcard[FuncTypePattern](m), // wildcard or nil, ignore type params
			// Body: matching `{ return "xxxx" }` or `{ return `xxxx` }` then binding matched string literal with bindVar "tableName" 
			// 
			Body: &ast.BlockStmt{
				List: []ast.Stmt{
					&ast.ReturnStmt{
						Results: []ast.Expr{
							BindWith[ExprPattern](m, "tableName", BasicLitKind(m, token.STRING)),
						},
					},
				},
			},
		}

		m.Match(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(filename))
			fmt.Println(m.ShowNode(c.Node()))
			table := binds["tableName"].(*ast.BasicLit)
			tableName, _ := strconv.Unquote(table.Value)
			fmt.Println(ansi.Red.Text(tableName))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}
```