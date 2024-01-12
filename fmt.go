package matcher

import (
	"bytes"
	"fmt"
	"go/ast"
	"go/format"
	"go/printer"
	"go/token"
	"io"
	"os"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓ format ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func WriteFile(fset *token.FileSet, filename string, f *ast.File, comment string) {
	fh, err := os.Create(filename)
	panicIfErr(err)
	defer func() {
		err = fh.Close()
		panicIfErr(err)
	}()

	if comment != "" {
		_, err = fmt.Fprint(fh, comment)
		panicIfErr(err)
	}

	err = format.Node(fh, fset, f)
	panicIfErr(err)
}

func FmtFile(fset *token.FileSet, f *ast.File) []byte {
	buf := new(bytes.Buffer)
	err := format.Node(buf, fset, f)
	panicIfErr(err)
	return buf.Bytes()
}

func WriteFileRaw(output io.Writer, file *ast.File) {
	p := printer.Config{Tabwidth: 8, Mode: printer.RawFormat | normalizeNumbers}
	err := p.Fprint(output, token.NewFileSet(), file)
	panicIfErr(err)
}

const normalizeNumbers = 1 << 30
