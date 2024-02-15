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
)

func GrepInterface(dir string) {
	l := loader.MustNew(dir)
	m := matcher.New()
	am := New(l, m)

	// match by typeSpec
	am.Match(&ast.TypeSpec{
		Type: &ast.InterfaceType{},
	}, func(c *Cursor, ctx Ctx) {
		typeSpec := c.Node().(*ast.TypeSpec)
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(l.ShowPos(typeSpec)))
		fmt.Println(l.ShowNode(typeSpec))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})

	// match by type
	am.Match(&ast.TypeSpec{
		Name: matcher.MkVar[IdentPattern](m, "typeName"), // Bind[IdentPattern](m, "typeName", Wildcard[IdentPattern](m)),
	}, func(c *Cursor, ctx Ctx) {
		id := ctx.Binds["typeName"].(*ast.Ident)
		iface, _ := ctx.TypeOf(id).Underlying().(*types.Interface)
		if iface == nil {
			return
		}
		fmt.Println("↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓")
		fmt.Println(ansi.Blue.Text(l.ShowPos(c.Node())))
		fmt.Println(l.ShowNode(c.Node()))
		fmt.Println("↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑↑")
	})
}
