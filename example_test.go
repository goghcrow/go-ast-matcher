package matcher

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"reflect"
	"regexp"
	"strconv"

	"github.com/goghcrow/go-ansi"
	"golang.org/x/tools/go/ast/astutil"
)

func PatternOfWildcardIdent(m *Matcher) ast.Node {
	// bind matched wildcard ident to "var"
	return Bind(m,
		"var",
		Wildcard[IdentPattern](m),
	)
}

func PatternOfLitVal(m *Matcher) ast.Node {
	return &ast.CompositeLit{
		// Type: nil,
		// nil value is wildcard, so Nil Pattern is needed to represent exactly nil type expr
		Type: Nil[ExprPattern](m),
	}
}

func PatternOfVarDecl(m *Matcher) ast.Node {
	return &ast.GenDecl{
		Tok: token.VAR, // IMPORT, CONST, TYPE, or VAR
		// pattern variable, match any and bind to "var"
		Specs: MkVar[SpecsPattern](m, "var"),
	}
}

func PatternOfConstDecl(m *Matcher) ast.Node {
	return &ast.GenDecl{
		Tok: token.CONST, // IMPORT, CONST, TYPE, or VAR
		// pattern variable, match any and bind to "var"
		Specs: MkVar[SpecsPattern](m, "var"),
	}
}

func PatternOfValSpec(m *Matcher) ast.Node {
	return &ast.ValueSpec{
		// pattern variable, match any and bind to "var"
		Names: MkVar[IdentsPattern](m, "var"),
		Type:  TypeIdentical[ExprPattern](m, m.MustLookupType("int")),
	}
}

func PatternOfCallee(m *Matcher) ast.Node {
	return CalleeOf(m, func(f types.Object) bool {
		return f != nil
	})
}
func PatternOfBuiltinCallee(m *Matcher) ast.Node {
	return BuiltinCalleeOf(m, func(f *types.Builtin) bool {
		return f != nil
	})
}
func PatternOfVarCallee(m *Matcher) ast.Node {
	return VarCalleeOf(m, func(f *types.Var) bool {
		return f != nil
	})
}
func PatternOfFuncOrMethodCallee(m *Matcher) ast.Node {
	return FuncOrMethodCalleeOf(m, func(f *types.Func) bool {
		return f != nil
	})
}
func PatternOfFuncCallee(m *Matcher) ast.Node {
	return FuncCalleeOf(m, func(f *types.Func) bool {
		return f != nil
	})
}
func PatternOfMethodCallee(m *Matcher) ast.Node {
	return MethodCalleeOf(m, func(f *types.Func) bool {
		return f != nil
	})
}
func PatternOfStaticCallee(m *Matcher) ast.Node {
	return StaticCalleeOf(m, func(f *types.Func) bool {
		return f != nil
	})
}
func PatternOfIfaceCalleeOf(m *Matcher) ast.Node {
	return IfaceCalleeOf(m, func(f *types.Func) bool {
		return f != nil
	})
}

func PatternOfAllImportSpec(m *Matcher) ast.Node {
	return &ast.ImportSpec{
		// pattern variable, match any and bind to "var"
		Path: MkVar[BasicLitPattern](m, "var"),
	}
}

func PatternOfAllFuncOrMethodDeclName(m *Matcher) ast.Node {
	return &ast.FuncDecl{
		Name: MkVar[IdentPattern](m, "var"),
	}
}

func PatternOfFuncOrMethodDeclWithSpecName(name string) func(m *Matcher) ast.Node {
	return func(m *Matcher) ast.Node {
		return &ast.FuncDecl{
			Name: IdentNameIs(m, name),
		}
	}
}

func PatternOfStructFieldWithJsonTag(m *Matcher) ast.Node {
	return &ast.Field{
		Tag: Bind(m,
			"var",
			TagOf(m, func(tag *reflect.StructTag) bool {
				if tag == nil {
					return false
				}
				_, ok := tag.Lookup("json")
				return ok
			}),
		),
	}
}

func PatternOfDefine(m *Matcher) ast.Node {
	return &ast.AssignStmt{
		Tok: token.DEFINE,
	}
}

func PatternOfAssign(m *Matcher) ast.Node {
	return &ast.AssignStmt{
		Tok: MkPattern[TokenPattern](m, func(m *Matcher, n ast.Node, stack []ast.Node, binds Binds) bool {
			tok := n.(TokenNode)
			// token.XXX_ASSIGN
			return token.Token(tok) != token.DEFINE
		}),
	}
}

func PatternOfAppendWithNoValue(m *Matcher) ast.Node {
	return &ast.CallExpr{
		Fun: And(m,
			IdentNameIs(m, "append"),
			IsBuiltin(m),
		),
		Args: SliceLenEQ[ExprsPattern](m, 1),
	}
}

func MkPatternOfCallFunOrMethodWithSpecName(name string) func(m *Matcher) ast.Node {
	return func(m *Matcher) ast.Node {
		isSpecNameFun := IdentOf(m, func(id *ast.Ident) bool {
			isFun := !m.Types[id].IsType() // not type cast
			return isFun && id.Name == name
		})
		return &ast.CallExpr{
			Fun: Or(m,
				PatternOf[ExprPattern](m, isSpecNameFun),                         // cast id pattern to expr pattern
				PatternOf[ExprPattern](m, &ast.SelectorExpr{Sel: isSpecNameFun}), // cast to expr pattern
			),
		}
	}
}

func MkPatternOfSelectorNameReg(name *regexp.Regexp) func(m *Matcher) *ast.CallExpr {
	return func(m *Matcher) *ast.CallExpr {
		// match *.XXX($args), and bind args to variable
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				// X: Wildcard[ExprPattern](m),
				Sel: IdentOf(m, func(id *ast.Ident) bool {
					isFun := !m.Types[id].IsType() // not type cast
					return isFun && name.MatchString(id.Name)
				}),
			},
			Args: MkVar[ExprsPattern](m, "args"),
		}
	}
}

func PatternOfCallAtomicAdder(m *Matcher) ast.Node {
	adders := regexp.MustCompile("^(AddInt64|AddUintptr)$")
	return &ast.CallExpr{
		Fun: SelectorOfPkgPath(m, "sync/atomic", adders),
	}
}

func PatternOfAtomicSwapStructField(m *Matcher) ast.Node {
	// atomic.AddInt64(&structObject.field, *)
	return &ast.CallExpr{
		Fun: SelectorOfPkgPath(m, "sync/atomic", regexp.MustCompile("^SwapInt64$")),
		Args: []ast.Expr{
			PtrOf(SelectorOfStructField(m, func(t *types.Struct) bool {
				return true
			}, func(t *types.Var) bool {
				return true
			})),
			Wildcard[ExprPattern](m),
		},
	}
}

func PatternOfFuncDeclHasAnyParam(m *Matcher, param *ast.Field) *ast.FuncDecl {
	return &ast.FuncDecl{
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: Contains[FieldsPattern](m, param),
			},
		},
	}
}

func PatternOfFuncDeclHasAnyParamNode(m *Matcher, param *ast.Field) *ast.FuncDecl {
	return &ast.FuncDecl{
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: Contains[FieldsPattern](m, param),
			},
		},
	}
}

func PatternOfMethodHasAnyParam(m *Matcher, param *ast.Field) *ast.FuncDecl {
	return &ast.FuncDecl{
		Recv: IsMethodRecv(m),
		Type: &ast.FuncType{
			Params: &ast.FieldList{
				List: Contains[FieldsPattern](m, param),
			},
		},
	}
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Gorm ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func PatternOfNonCompositeModelCall(m *Matcher) func() ast.Node {
	gormDB := types.NewPointer(m.MustLookupType("gorm.io/gorm.DB"))
	return func() ast.Node {
		// db.Model(&Model{})
		// db.Model(Model{})
		return &ast.CallExpr{
			Fun: &ast.SelectorExpr{
				X:   TypeAssignableTo[ExprPattern](m, gormDB),
				Sel: IdentNameIs(m, "Model"),
			},
			Args: []ast.Expr{
				Not(m,
					Or(m,
						PatternOf[ExprPattern](m, PtrOf(&ast.CompositeLit{})),
						PatternOf[ExprPattern](m, &ast.CompositeLit{}),
					),
				),
			},
		}
	}
}

func GrepFuncDeclWithSpecTypeOfParam(dir string, qualifiedType string, opts ...MatchOption) {
	m := NewMatcher(dir, []string{PatternAll}, opts...)

	ty := m.MustLookupType(qualifiedType)
	pattern := PatternOfMethodHasAnyParam(m,
		&ast.Field{
			Type: Or(m,
				TypeAssignableTo[ExprPattern](m, ty),
				TypeAssignableTo[ExprPattern](m, types.NewPointer(ty)),
			),
		},
	)

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

	fNames := regexp.MustCompile("^(GetDBProxy|GetDB)$")
	pattern := MkPatternOfSelectorNameReg(fNames)(m)

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
	// Notice: we want match node by outer type, so WithLoadDepts needed
	m := NewMatcher(dir, []string{PatternAll}, WithLoadDepts())

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
	// Notice: we want match node by outer type, so WithLoadDepts needed
	m := NewMatcher(dir, []string{PatternAll}, WithLoadDepts())

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
		// Name: IdentNameIs(m, "TableName"),

		Name: And[IdentPattern](m,
			// IsMethod(m),
			IdentNameIs(m, "TableName"),
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
						Bind[ExprPattern](m, "tableName", BasicLitOfKind(m, token.STRING)),

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
	type (
		outerFun = *ast.FuncDecl
		rootNode = ast.Node
	)

	var (
		cache       = map[*ast.FuncDecl]bool{}
		filterFDecl = func(m *Matcher, f *ast.FuncDecl) bool {
			if ok, has := cache[f]; has {
				return ok
			}
			ok := filter(m, f)
			cache[f] = ok
			return ok
		}
		collectChainCalls = func(m *Matcher, calls map[rootNode]outerFun, stack []ast.Node) []ast.Node {
			// skip self
			// call{fun=select{x=call{fun={...}}}}
			// call / sel / call / sel / ...
			var chainRoot ast.Node
			var chains []ast.Node
			for i := 1; i < len(stack); i++ {
				n := stack[i]
				if i%2 == 0 { // call
					if IsNode[*ast.CallExpr](n) {
						continue
					}
					// the leafNode contains the longest chain methods in post-order
					chainRoot = stack[i-1]
					chains = stack[:i]
				} else { // sel
					// sel != call
					if IsNode[*ast.SelectorExpr](n) {
						continue
					}
					// the leafNode contains the longest chain methods in post-order
					chainRoot = stack[i-1]
					chains = stack[:i]
				}

				// skip non-leafNode
				if _, has := calls[chainRoot]; !has {
					f := InWhichFunDecl(i, stack)
					if filterFDecl(m, f) {
						calls[chainRoot] = f
					}
				}
				break
			}
			return chains
		}
	)

	// Notice: we want match node by outer type, so WithLoadDepts needed
	m := NewMatcher(
		dir,
		[]string{PatternAll},
		append(opts, WithLoadDepts())...,
	)

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
			return types.AssignableTo(
				retTy,
				types.NewPointer(m.MustLookupType("gorm.io/gorm.DB")),
			)
		}),
	}

	calls := map[rootNode]outerFun{}

	m.Match(pattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		collectChainCalls(m, calls, stack)
	})

	groupedChainCalls := GroupChainCalls(calls)
	for fun, xs := range groupedChainCalls {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(m.ShowPos(fun)))
		fmt.Println(ansi.Blue.Text(fun.Name.String()))
		fmt.Println(m.ShowNode(fun.Type))
		_ = xs
		// for _, it := range xs {
		// 	fmt.Println(m.ShowNode(it))
		// }
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	}
}

func ChainCalls(m *Matcher, cache map[ast.Node]bool, stack []ast.Node) []ast.Node {
	assert(IsNode[*ast.CallExpr](stack[0]), "must be Call Pattern callback")

	// skip self
	// call{fun=select{x=call{fun={...}}}}
	// call / sel / call / sel / ...
	var chainRoot ast.Node
	var chains []ast.Node
	for i := 1; i < len(stack); i++ {
		n := stack[i]
		// println(m.ShowNode(n)) // dbg
		if i%2 == 0 { // call
			if IsNode[*ast.CallExpr](n) {
				continue
			}
			// the leafNode contains the longest chain methods in post-order
			chainRoot = stack[i-1]
			chains = stack[:i]
		} else { // sel
			// sel != call
			if IsNode[*ast.SelectorExpr](n) {
				continue
			}
			// the leafNode contains the longest chain methods in post-order
			chainRoot = stack[i-1]
			chains = stack[:i]
		}

		// skip non-leafNode
		if _, has := cache[chainRoot]; !has {
			cache[chainRoot] = true
			// do something !!!
		}

		// the outermost found
		break
	}
	return chains
}

func GroupChainCalls(root2outerFun map[ast.Node]*ast.FuncDecl) map[*ast.FuncDecl][]ast.Node {
	g := map[*ast.FuncDecl][]ast.Node{}
	for root, inFun := range root2outerFun {
		if inFun == nil {
			continue
		}
		g[inFun] = append(g[inFun], root)
	}
	return g
}

func InWhichFunDecl(start int, stack []ast.Node) (fun *ast.FuncDecl) {
	for j := start; j < len(stack); j++ {
		if IsNode[*ast.FuncDecl](stack[j]) {
			fun = stack[j].(*ast.FuncDecl)
		}
	}
	return
}
