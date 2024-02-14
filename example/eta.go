package example

import (
	"go/ast"

	. "github.com/goghcrow/go-ast-matcher"
	"github.com/goghcrow/go-matcher"
	. "github.com/goghcrow/go-matcher/combinator"
)

// EtaReduction
// fun(...args) { return return f(...args) }  ==>  f
func EtaReduction(am ASTMatcher) {
	m := am.Matcher
	pattern := &ast.FuncLit{
		Type: &ast.FuncType{
			Params: matcher.MkVar[FieldListPattern](m, "params"),
		},

		Body: &ast.BlockStmt{
			List: []ast.Stmt{
				&ast.ReturnStmt{
					Results: []ast.Expr{
						&ast.CallExpr{
							Fun:  matcher.MkVar[ExprPattern](m, "fun"),
							Args: matcher.MkVar[ExprsPattern](m, "args"),
						},
					},
				},
			},
		},
	}

	// assume type-checked
	matched := func(ctx Ctx, paramsFields []*ast.Field, argsExprs []ast.Expr) bool {
		if paramsFields == nil && argsExprs == nil {
			return true
		}
		var args []*ast.Ident
		for _, argExpr := range argsExprs {
			arg, _ := argExpr.(*ast.Ident)
			if arg == nil {
				return false // must be ident
			}
			args = append(args, arg)
		}

		var params []*ast.Ident
		for _, paramGroup := range paramsFields {
			for _, param := range paramGroup.Names {
				params = append(params, param)
			}
		}

		if len(args) != len(params) {
			return false
		}

		for i, arg := range args {
			param := params[i]
			if arg.Name != param.Name {
				return false
			}
			if ctx.ObjectOf(arg) != ctx.ObjectOf(param) {
				return false
			}
		}

		return true
	}

	am.Match(
		pattern,
		func(c Cursor, ctx Ctx) {
			params := ctx.Binds["params"].(*ast.FieldList).List
			args := ctx.Binds["args"].(ExprsNode)
			if matched(ctx, params, args) {
				c.Replace(ctx.Binds["fun"])
			}
		},
	)
}
