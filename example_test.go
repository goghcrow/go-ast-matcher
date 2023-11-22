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

func GrepMethodWithSpecTypeParameter(dir string, qualifiedType string, opts ...MatchOption) {
	m := NewMatcher(dir, []string{PatternAll}, opts...)
	ty := m.MustLookupType(qualifiedType)
	fieldTypeIs := PatternOf[FieldPattern](m, &ast.Field{
		Type: TypeAssignableTo[ExprPattern](m, ty),
	})
	pattern := &ast.FuncDecl{
		Recv: IsMethod(m),
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: Any[FieldPattern, FieldsPattern](m, fieldTypeIs),
			},
		},
	}
	m.Match(pattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(m.Filename))
		fmt.Println(m.ShowNode(c.Node()))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
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

func GrepDBProxy(dir string) {
	m := NewMatcher(dir, []string{PatternAll})

	// match *.GetDBProxy($args), and bind args to variable
	// equals to regex: .*?\.GetDBProxy\(?<args>.*?\)
	pattern := &ast.CallExpr{
		Fun: &ast.SelectorExpr{
			// X: Wildcard[ExprPattern](m),
			Sel: Or[IdentPattern](m,
				IdentEqual(m, "GetDBProxy"),
				IdentEqual(m, "GetDB"),
			),
		},
		Args: MkVar[ExprsPattern](m, "args"),
	}

	m.Match(pattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(m.Filename))
		fmt.Println(m.ShowNode(c.Node()))
		xs := binds["args"].(ExprsNode)
		for _, x := range xs {
			println(m.ShowNode(x))
		}
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepGormTabler(dir string) {
	// Notice: we want match node by outer type, so WithLoadAll needed
	m := NewMatcher(dir, []string{PatternAll}, WithLoadAll())

	// types.Named -> types.Interface
	gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)
	pattern := &ast.TypeSpec{
		// must impl gorm.Tabler, and bind node to variable
		Name: Bind(m, "typeId", TypeImplements[IdentPattern](m, gormTabler)),
		// wildcard pattern can be ignored
		// TypeParams: Wildcard[FieldListPattern](m), // ignore type params
		// Type: Wildcard[ExprPattern](m), // ignore type
	}

	m.Match(pattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(m.Filename))
		typeId := binds["typeId"].(*ast.Ident)
		fmt.Println(m.TypeOf(typeId).String())
		fmt.Println(m.ShowNode(c.Node()))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepGormTablerTableName(dir string) {
	// Notice: we want match node by outer type, so WithLoadAll needed
	m := NewMatcher(dir, []string{PatternAll}, WithLoadAll())

	// types.Named -> types.Interface
	gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)

	pattern := &ast.FuncDecl{
		// recv must impl gorm.Tabler
		Recv: MethodRecv(m, func(ident *ast.Ident, ty ast.Expr) bool {
			return implements(m.TypeOf(ty), gormTabler)
		}),
		// method name must be TableName
		Name: IdentEqual(m, "TableName"),
		// ignore type signature
		// Type: Wildcard[FuncTypePattern](m),
		// Body match { return "xxxx" } or { return `xxxx` },
		// and binding stringLit to the variable of tableName
		Body: &ast.BlockStmt{
			List: []ast.Stmt{
				&ast.ReturnStmt{
					Results: []ast.Expr{
						Bind[ExprPattern](m, "tableName", BasicLitKind(m, token.STRING)),
						// Or
						// Bind[BasicLitPattern](m, "tableName", MkPattern[BasicLitPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
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

	m.Match(pattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(m.Filename))
		fmt.Println(m.ShowNode(c.Node()))
		table := binds["tableName"].(*ast.BasicLit)
		tableName, _ := strconv.Unquote(table.Value)
		fmt.Println(ansi.Red.Text(tableName))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepNestedFuncLit(dir string) {
	m := NewMatcher(dir, []string{PatternAll})
	outerFuncLit := func(stack []ast.Node) ast.Node {
		for _, it := range stack {
			outer, _ := it.(*ast.FuncLit)
			if outer != nil {
				return outer
			}
		}
		return nil
	}
	m.Match(&ast.FuncLit{}, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		outer := outerFuncLit(stack[1:]) // exclude self
		if outer == nil {
			return
		}
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		// fmt.Println(ansi.Blue.Text(filename))
		fmt.Println(ansi.Blue.Text(m.ShowPos(outer)))
		fmt.Println(m.ShowNode(outer))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepInterface(dir string) {
	m := NewMatcher(dir, []string{PatternAll})

	// match by typeSpec
	m.Match(&ast.TypeSpec{
		Type: &ast.InterfaceType{},
	}, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		typeSpec := c.Node().(*ast.TypeSpec)
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(m.ShowPos(typeSpec)))
		fmt.Println(m.ShowNode(typeSpec))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})

	// match by type
	m.Match(&ast.TypeSpec{
		Name: MkVar[IdentPattern](m, "typeName"), // Bind[IdentPattern](m, "typeName", Wildcard[IdentPattern](m)),
	}, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
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

}
