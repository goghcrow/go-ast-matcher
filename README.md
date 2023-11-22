## [WIP] go-ast-matcher

Match and rewrite ast by ast pattern.

```go
func GrepGormTabler(dir string) {
	// Notice: we want match node by outer type, so WithLoadAll needed
	m := NewMatcher(dir, []string{PatternAll}, WithLoadAll())
	m.Walk(func(m *Matcher, file *ast.File) {
		// types.Named -> types.Struct
		// gormDB := m.MustLookupType("gorm.io/gorm.DB").Underlying().(*types.Struct)

		// types.Named -> types.Interface
		gormTabler := m.MustLookupType("gorm.io/gorm/schema.Tabler").Underlying().(*types.Interface)
		typeSpecPattern := &ast.TypeSpec{
			// must impl gorm.Tabler, and bind node to variable
			Name: BindWith(m, "typeId", TypeImplements[IdentPattern](m, gormTabler)),
			// wildcard pattern can be ignored
			// TypeParams: Wildcard[FieldListPattern](m), // ignore type params
			// Type: Wildcard[ExprPattern](m), // ignore type
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
```