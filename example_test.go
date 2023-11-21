package astmatcher

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"strconv"

	"github.com/goghcrow/go-ansi"
	"golang.org/x/tools/go/ast/astutil"
)

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

func GrepDBProxy(dir string) {
	m := NewMatcher(dir, []string{PatternAll})
	m.Walk(func(m *Matcher, file *ast.File) {
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

		m.MatchNode(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(m.Filename))
			fmt.Println(m.ShowNode(c.Node()))
			xs := binds["args"].(ExprsNode)
			for _, x := range xs {
				println(m.ShowNode(x))
			}
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}

func GrepGormTabler(dir string) {
	// 需要 LookupType, 所以 WithLoadAll
	m := NewMatcher(dir, []string{PatternAll}, WithLoadAll())
	m.Walk(func(m *Matcher, file *ast.File) {
		// types.Named -> types.Struct
		// gormDB := m.MustLookupType("gorm.io/gorm.DB").Underlying().(*types.Struct)

		// types.Named -> types.Interface
		gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)
		typeSpecPattern := &ast.TypeSpec{
			// 必须实现 gorm.Tabler 接口, 并且匹配节点绑定到变量
			Name: BindWith(m, "typeId", TypeImplements[IdentPattern](m, gormTabler)),
			// 通配符可以省略
			// TypeParams: Wildcard[FieldListPattern](m), // 忽略类型变量
			// Type: Wildcard[ExprPattern](m), // 忽略类型定义
		}

		m.MatchNode(typeSpecPattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(m.Filename))
			typeId := binds["typeId"].(*ast.Ident)
			fmt.Println(m.TypeOf(typeId).String())
			fmt.Println(m.ShowNode(c.Node()))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}

func GrepGormTablerTableName(dir string) {
	// 需要 LookupType, 所以 WithLoadAll
	m := NewMatcher(dir, []string{PatternAll}, WithLoadAll())
	m.Walk(func(m *Matcher, file *ast.File) {
		// types.Named -> types.Struct
		// gormDB := m.MustLookupType("gorm.io/gorm.DB").Underlying().(*types.Struct)
		// types.Named -> types.Interface
		gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)

		pattern := &ast.FuncDecl{
			// recv 必须实现 gorm.Tabler 接口
			Recv: MethodRecv(m, func(ident *ast.Ident, ty ast.Expr) bool {
				return TypeX.Implements(m.TypeOf(ty), gormTabler)
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

		m.MatchNode(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(m.Filename))
			fmt.Println(m.ShowNode(c.Node()))
			table := binds["tableName"].(*ast.BasicLit)
			tableName, _ := strconv.Unquote(table.Value)
			fmt.Println(ansi.Red.Text(tableName))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}

func GrepNestedLambda(dir string) {
	m := NewMatcher(dir, []string{PatternAll})
	m.Walk(func(m *Matcher, file *ast.File) {
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
		m.MatchNode(&ast.FuncLit{}, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
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

func GrepInterface(dir string) {
	// 通过声明直接判断
	m := NewMatcher(dir, []string{PatternAll})
	m.Walk(func(m *Matcher, file *ast.File) {
		pattern := &ast.TypeSpec{
			Type: &ast.InterfaceType{},
		}
		m.MatchNode(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			typeSpec := c.Node().(*ast.TypeSpec)
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(m.ShowPos(typeSpec)))
			fmt.Println(m.ShowNode(typeSpec))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})

	// 通过类型判断, 可以识别 underlying
	m.Walk(func(m *Matcher, file *ast.File) {
		pattern := &ast.TypeSpec{
			Name: MkVar[IdentPattern](m, "typeName"), // BindWith[IdentPattern](m, "typeName", Wildcard[IdentPattern](m)),
		}
		m.MatchNode(pattern, file, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			id := binds["typeName"].(*ast.Ident)
			iface, _ := m.TypeOf(id).Underlying().(*types.Interface)
			if iface == nil {
				return
			}
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(m.ShowPos(c.Node())))
			fmt.Println(m.ShowNode(c.Node()))
			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		})
	})
}
