package example

import (
	"fmt"
	"go/ast"
	"go/types"
	"strconv"

	"github.com/goghcrow/go-ansi"
	. "github.com/goghcrow/go-ast-matcher"
	"github.com/goghcrow/go-loader"
	"github.com/goghcrow/go-matcher"
	. "github.com/goghcrow/go-matcher/combinator"
	"github.com/goghcrow/go-matcher/example"
)

func isNode[T ast.Node](n ast.Node) bool {
	_, ok := n.(T)
	return ok
}

func GrepGormTabler(dir string) {
	// Notice: we want match node by outer type, so WithLoadDepts needed
	l := loader.MustNew(dir, loader.WithLoadDepts())
	m := matcher.New()
	am := New(l, m)

	// types.Named -> types.Interface
	gormTabler := l.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)
	pattern := example.PatternOfGormTabler(m, gormTabler)
	am.Match(pattern, func(c Cursor, ctx Ctx) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(ctx.File.Filename))
		typeId := ctx.Binds["typeId"].(*ast.Ident)
		fmt.Println(ctx.TypeOf(typeId).String())
		fmt.Println(l.ShowNode(c.Node()))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepGormTablerTableName(dir string) {
	// Notice: we want match node by outer type, so WithLoadDepts needed
	l := loader.MustNew(dir, loader.WithLoadDepts())
	m := matcher.New()
	am := New(l, m)

	// types.Named -> types.Interface
	gormTabler := l.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)
	pattern := example.PatternOfGormTablerTableName(m, gormTabler)
	am.Match(pattern, func(c Cursor, ctx Ctx) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(ctx.File.Filename))
		fmt.Println(l.ShowNode(c.Node()))
		table := ctx.Binds["tableName"].(*ast.BasicLit)
		tableName, _ := strconv.Unquote(table.Value)
		fmt.Println(ansi.Red.Text(tableName))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepDBProxy(dir string) {
	l := loader.MustNew(dir)
	m := matcher.New()
	am := New(l, m)

	pattern := example.PatternOfGetDBProxy(am.Matcher)
	am.Match(pattern, func(c Cursor, ctx Ctx) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(ctx.File.Filename))
		fmt.Println(l.ShowNode(c.Node()))
		xs := ctx.Binds["args"].(ExprsNode)
		for _, x := range xs {
			println(l.ShowNode(x))
		}
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepGormChainAPI(dir string, filter func(ASTMatcher, Ctx, *ast.FuncDecl) bool, opts ...loader.Option) {
	type (
		outerFun = *ast.FuncDecl
		rootNode = ast.Node
	)

	var (
		cache       = map[*ast.FuncDecl]bool{}
		filterFDecl = func(m ASTMatcher, ctx Ctx, f *ast.FuncDecl) bool {
			if ok, has := cache[f]; has {
				return ok
			}
			ok := filter(m, ctx, f)
			cache[f] = ok
			return ok
		}
		collectChainCalls = func(m ASTMatcher, ctx Ctx, calls map[rootNode]outerFun, stack []ast.Node) []ast.Node {
			// skip self
			// call{fun=select{x=call{fun={...}}}}
			// call / sel / call / sel / ...
			var chainRoot ast.Node
			var chains []ast.Node
			for i := 1; i < len(stack); i++ {
				n := stack[i]
				if i%2 == 0 { // call
					if isNode[*ast.CallExpr](n) {
						continue
					}
					// the leafNode contains the longest chain methods in post-order
					chainRoot = stack[i-1]
					chains = stack[:i]
				} else { // sel
					// sel != call
					if isNode[*ast.SelectorExpr](n) {
						continue
					}
					// the leafNode contains the longest chain methods in post-order
					chainRoot = stack[i-1]
					chains = stack[:i]
				}

				// skip non-leafNode
				if _, has := calls[chainRoot]; !has {
					f := InWhichFunDecl(i, stack)
					if filterFDecl(m, ctx, f) {
						calls[chainRoot] = f
					}
				}
				break
			}
			return chains
		}
	)

	// Notice: we want match node by outer type, so WithLoadDepts needed
	l := loader.MustNew(dir, append(opts, loader.WithLoadDepts())...)
	m := matcher.New()
	am := New(l, m)

	pattern := &ast.CallExpr{
		Fun: TypeOf[ExprPattern](m, func(_ *MatchCtx, t types.Type) bool {
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
				types.NewPointer(l.MustLookupType("gorm.io/gorm.DB")),
			)
		}),
	}

	calls := map[rootNode]outerFun{}

	am.Match(pattern, func(c Cursor, ctx Ctx) {
		collectChainCalls(am, ctx, calls, ctx.Stack)
	})

	groupedChainCalls := GroupChainCalls(calls)
	for fun, xs := range groupedChainCalls {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(l.ShowPos(fun)))
		fmt.Println(ansi.Blue.Text(fun.Name.String()))
		fmt.Println(l.ShowNode(fun.Type))
		_ = xs
		// for _, it := range xs {
		// 	fmt.Println(l.ShowNode(it))
		// }
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	}
}

func ChainCalls(cache map[ast.Node]bool, stack []ast.Node) []ast.Node {
	if !isNode[*ast.CallExpr](stack[0]) {
		panic("must be Call Pattern callback")
	}

	// skip self
	// call{fun=select{x=call{fun={...}}}}
	// call / sel / call / sel / ...
	var chainRoot ast.Node
	var chains []ast.Node
	for i := 1; i < len(stack); i++ {
		n := stack[i]
		// println(l.ShowNode(n)) // dbg
		if i%2 == 0 { // call
			if isNode[*ast.CallExpr](n) {
				continue
			}
			// the leafNode contains the longest chain methods in post-order
			chainRoot = stack[i-1]
			chains = stack[:i]
		} else { // sel
			// sel != call
			if isNode[*ast.SelectorExpr](n) {
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
		if isNode[*ast.FuncDecl](stack[j]) {
			fun = stack[j].(*ast.FuncDecl)
		}
	}
	return
}
