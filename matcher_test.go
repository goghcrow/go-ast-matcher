package astmatcher

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"log"
	"path"
	"runtime"
	"strconv"
	"testing"

	"github.com/goghcrow/go-ansi"
	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

var TestDir = getCurrentDir()

func getCurrentDir() string {
	_, filename, _, _ := runtime.Caller(0)
	return path.Dir(filename)
}

func WalkDir(dir string, f func(filename string, m *Matcher, file *ast.File)) {
	fset, pkgs := LoadDir(dir)
	if false {
		// 检查编译错误
		if packages.PrintErrors(pkgs) > 0 {
			log.Fatalf("packages contain errors")
		}
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

func PatternExample(m *Matcher) {
	// func Function() {}
	// func (...) Function() {}
	// func Method() {}
	// func (...) Method() {}
	_ = &ast.FuncDecl{
		Name: Or[IdentPattern](m,
			IdentEqual(m, "Function"),
			IdentEqual(m, "Method"),
		),
	}
}

func TestGrepDBProxy(t *testing.T) {
	WalkDir(TestDir, func(filename string, m *Matcher, file *ast.File) {
		// 匹配 *.GetDBProxy($args), 并且把参数列表绑定到 args 变量
		// 相当于 regex:  .*?\.GetDBProxy\(?<args>.*?\)
		pattern := &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				// X: Wildcard[ExprPattern](m),
				Sel: Or[IdentPattern](m,
					IdentEqual(m, "GetDBProxy"),
					IdentEqual(m, "GetDB"),
				),
			},
			Args: m.MkExprsPatternVar("args"),
		}

		m.Match(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(filename))
			fmt.Println(m.ShowNode(c.Node()))
			xs := binds["args"].(ExprsNode)
			for _, x := range xs {
				println(m.ShowNode(x))
			}
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}

func TestGrepGormTabler(t *testing.T) {
	WalkDir(TestDir, func(filename string, m *Matcher, file *ast.File) {
		// types.Named -> types.Struct
		// gormDB := m.MustLookupType("gorm.io/gorm.DB").Underlying().(*types.Struct)

		// types.Named -> types.Interface
		gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)
		typeSpecPattern := &ast.TypeSpec{
			// 必须实现 gorm.Tabler 接口, 并且匹配节点绑定到变量
			Name: BindWith(m, "typeId", Implements[IdentPattern](m, gormTabler)),
			// 通配符可以省略
			// TypeParams: Wildcard[FieldListPattern](m), // 忽略类型变量
			// Type: Wildcard[ExprPattern](m), // 忽略类型定义
		}

		m.Match(typeSpecPattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(filename))
			typeId := binds["typeId"].(*ast.Ident)
			fmt.Println(m.TypeOf(typeId).String())
			fmt.Println(m.ShowNode(c.Node()))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}

func TestGrepGormTablerTableName(t *testing.T) {
	WalkDir(TestDir, func(filename string, m *Matcher, file *ast.File) {
		// types.Named -> types.Struct
		// gormDB := m.MustLookupType("gorm.io/gorm.DB").Underlying().(*types.Struct)
		// types.Named -> types.Interface
		gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)

		pattern := &ast.FuncDecl{
			// recv 必须实现 gorm.Tabler 接口
			Recv: MethodRecv(m, func(ident *ast.Ident, ty ast.Expr) bool {
				return TypeImplements(m.TypeOf(ty), gormTabler)
			}),
			Name: IdentEqual(m, "TableName"), // 方法名必须是 TableName
			// Type: Wildcard[FuncTypePattern](m), // 忽略类型签名
			// Body 匹配 { return "xxxx" } 或者 { return `xxxx` }, 并将字符串绑字面量绑定到 tableName 变量
			Body: &ast.BlockStmt{
				List: []ast.Stmt{
					&ast.ReturnStmt{
						Results: []ast.Expr{
							BindWith[ExprPattern](m, "tableName", BasicLitKind(m, token.STRING)),
							// Or
							// BindWith[BasicLitPattern](m, "tableName", MkPattern[BasicLitPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
							//	lit, _ := n.(*ast.BasicLit)
							//	if lit == nil {
							//		return false
							//	}
							//	return lit.Kind == token.STRING
							// })),
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

func TestGrepNestedLambda(t *testing.T) {
	WalkDir(TestDir, func(filename string, m *Matcher, file *ast.File) {
		// 查找 FuncLit 嵌套 FuncLit
		outerFuncLit := func(stack []ast.Node) ast.Node {
			for _, it := range stack {
				outer, _ := it.(*ast.FuncLit)
				if outer != nil {
					return outer
				}
			}
			return nil
		}
		m.Match(&ast.FuncLit{}, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			outer := outerFuncLit(stack[1:]) // 排除自身
			if outer == nil {
				return
			}
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			// fmt.Println(ansi.Blue.Text(filename))
			fmt.Println(ansi.Blue.Text(m.ShowPos(outer)))
			fmt.Println(m.ShowNode(outer))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}
