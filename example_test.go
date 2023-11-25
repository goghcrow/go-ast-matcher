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

func PatternOfAppendWithNoValue(m *Matcher) ast.Node {
	return &ast.CallExpr{
		Fun: And(m,
			IdentEqual(m, "append"),
			IsBuiltin(m),
		),
		Args: SliceLenEQ[ExprsPattern](m, 1),
	}
}

func GrepMethodWithSpecTypeParameter(dir string, qualifiedType string, opts ...MatchOption) {
	m := NewMatcher(dir, []string{PatternAll}, opts...)
	ty := m.MustLookupType(qualifiedType)
	fieldTypeIs := PatternOf[FieldPattern](m, &ast.Field{
		Type: Or[ExprPattern](m,
			TypeAssignableTo[ExprPattern](m, ty),
			TypeAssignableTo[ExprPattern](m, types.NewPointer(ty)),
		),
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
		Name: Bind(m, "typeId",
			Or(m,
				TypeOf[IdentPattern](m, func(t types.Type) bool {
					return types.Implements(t, gormTabler)
				}),
				TypeOf[IdentPattern](m, func(t types.Type) bool {
					return types.Implements(types.NewPointer(t), gormTabler)
				}),
			),
		),
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
		// Recv: MethodRecvOf(m, func(recv *ast.Field) bool {
		// 	ty := m.TypeOf(recv.Type)
		// 	return types.Implements(ty, gormTabler) ||
		// 		types.Implements(types.NewPointer(ty), gormTabler)
		// }),
		// method name must be TableName
		// Name: IdentEqual(m, "TableName"),

		Name: And[IdentPattern](m,
			// IsMethod(m),
			IdentEqual(m, "TableName"),
			RecvTypeOf(m, func(ty types.Type) bool {
				return types.Implements(ty, gormTabler) ||
					types.Implements(types.NewPointer(ty), gormTabler)
			}),
		),

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

func GrepGormChainAPI(dir string, filter func(*Matcher, *ast.FuncDecl) bool, opts ...MatchOption) {
	// Notice: we want match node by outer type, so WithLoadAll needed
	m := NewMatcher(
		dir,
		[]string{PatternAll},
		append(opts, WithLoadAll())...,
	)

	ptrOfGormDB := types.NewPointer(m.MustLookupType("gorm.io/gorm.DB"))
	pattern := &ast.CallExpr{
		Fun: TypeOf[ExprPattern](m, func(t types.Type) bool {
			sig, ok := t.Underlying().(*types.Signature)
			if !ok {
				// not func call
				return false
			}
			xs := sig.Results()
			if xs.Len() != 1 {
				return false
			}
			retTy := xs.At(0).Type()
			return types.AssignableTo(retTy, ptrOfGormDB)
		}),
	}

	calls := map[ast.Node]*ast.FuncDecl{}
	m.Match(pattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		assert(stack[0].(*ast.CallExpr) == c.Node().(*ast.CallExpr), "impossible")
		collectChainCalls(m, calls, stack)
	})

	groupedChainCalls := groupChainCalls(calls)
	for fun, xs := range groupedChainCalls {
		if !filter(m, fun) {
			fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
			fmt.Println(ansi.Blue.Text(m.ShowPos(fun)))
			fmt.Println(ansi.Blue.Text(fun.Name.String()))
			fmt.Println(m.ShowNode(fun.Type))

			for _, it := range xs {
				fmt.Println(m.ShowNode(it))
			}

			fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
		}
	}
}

func collectChainCalls(m *Matcher, calls map[ast.Node]*ast.FuncDecl, stack []ast.Node) {
	// skip self
	// call{fun=select{x=call{fun={...}}}}
	// call / sel / call / sel / ...
	for i := 1; i < len(stack); i++ {
		var chainRoot ast.Node
		var chains []ast.Node
		n := stack[i]
		if i%2 == 0 { // call
			if IsNode[*ast.CallExpr](n) {
				continue
			}
			// the leafNode contains the longest chain methods in post-order
			chainRoot = stack[i-1].(*ast.SelectorExpr)
			chains = stack[:i]
		} else { // sel
			// sel != call
			if IsNode[*ast.SelectorExpr](n) {
				continue
			}
			// the leafNode contains the longest chain methods in post-order
			chainRoot = stack[i-1].(*ast.CallExpr)
			chains = stack[:i]
		}
		_ = chains
		if _, has := calls[chainRoot]; !has {
			calls[chainRoot] = outerFunDecl(m, i, stack)
		} else {
			// skip non-leafNode
		}
		break
	}
}

func outerFunDecl(m *Matcher, start int, stack []ast.Node) (fun *ast.FuncDecl) {
	for j := start; j < len(stack); j++ {
		if IsNode[*ast.FuncDecl](stack[j]) {
			fun = stack[j].(*ast.FuncDecl)
		}
	}
	return
}

func groupChainCalls(m map[ast.Node]*ast.FuncDecl) map[*ast.FuncDecl][]ast.Node {
	g := map[*ast.FuncDecl][]ast.Node{}
	for root, fun := range m {
		if fun == nil {
			continue
		}
		g[fun] = append(g[fun], root)
	}
	return g
}
