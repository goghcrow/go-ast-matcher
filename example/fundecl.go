package example

import (
	"fmt"
	"go/ast"
	"go/types"

	"github.com/goghcrow/go-ansi"
	. "github.com/goghcrow/go-ast-matcher"
	"github.com/goghcrow/go-loader"
	"github.com/goghcrow/go-matcher"
	. "github.com/goghcrow/go-matcher/combinator"
	"github.com/goghcrow/go-matcher/example"
)

func GrepFuncDeclWithSpecTypeOfParam(dir string, qualifiedType string, opts ...loader.Option) {
	l := loader.MustNew(dir, opts...)
	m := matcher.New()
	am := New(l, m)

	ty := l.MustLookupType(qualifiedType)
	pattern := example.PatternOfMethodHasAnyParam(m,
		&ast.Field{
			Type: Or(m,
				TypeAssignableTo[ExprPattern](m, ty),
				TypeAssignableTo[ExprPattern](m, types.NewPointer(ty)),
			),
		},
	)

	am.Match(pattern, func(c *Cursor, ctx Ctx) {
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(ctx.File.Filename))
		fmt.Println(l.ShowNode(c.Node()))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}

func GrepNestedFuncLit(dir string) {
	l := loader.MustNew(dir)
	m := matcher.New()
	am := New(l, m)

	outerFuncLit := func(stack []ast.Node) ast.Node {
		for _, it := range stack {
			outer, _ := it.(*ast.FuncLit)
			if outer != nil {
				return outer
			}
		}
		return nil
	}
	am.Match(&ast.FuncLit{}, func(c *Cursor, ctx Ctx) {
		outer := outerFuncLit(ctx.Stack[1:]) // exclude self
		if outer == nil {
			return
		}
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		// fmt.Println(ansi.Blue.Text(filename))
		fmt.Println(ansi.Blue.Text(l.ShowPos(outer)))
		fmt.Println(l.ShowNode(outer))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}
