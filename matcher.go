package astmatcher

import (
	"go/ast"

	"github.com/goghcrow/go-loader"
	"github.com/goghcrow/go-matcher"
	"golang.org/x/tools/go/ast/astutil"
)

type (
	ASTMatcher struct {
		Loader  *loader.Loader
		Matcher *matcher.Matcher
	}
	Cursor = *astutil.Cursor
)

type Ctx struct {
	*matcher.MatchCtx
	File *loader.File
}

func (c Ctx) Match(pattern, node ast.Node, f matcher.Matched) {
	c.Matcher.Match(c.Pkg, pattern, node, f)
}

func (c Ctx) Matched(pattern, root ast.Node) bool {
	return c.Matcher.Matched(c.Pkg, pattern, root)
}

func New(l *loader.Loader, m *matcher.Matcher) ASTMatcher {
	return ASTMatcher{Loader: l, Matcher: m}
}

// Match by pattern in all packages
func (m ASTMatcher) Match(ptn ast.Node, cb func(Cursor, Ctx)) {
	m.Loader.VisitAllFiles(func(f *loader.File) {
		m.Matcher.Match(f.Pkg, ptn, f.File, func(c Cursor, ctx *matcher.MatchCtx) {
			cb(c, Ctx{ctx, f})
		})
	})
}
