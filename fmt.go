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
	"path/filepath"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓ format ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func WriteFile(fset *token.FileSet, filename string, f *ast.File, comment string) {
	mustMkDir(filepath.Dir(filename))
	fh, err := os.Create(filename)
	panicIf(err)
	defer func() {
		err = fh.Close()
		panicIf(err)
	}()

	if comment == "" {
		err = format.Node(fh, fset, f)
		panicIf(err)
		return
	}

	src := FmtFile(fset, f)
	if !bytes.HasPrefix(src, []byte(comment)) {
		_, err = fmt.Fprint(fh, comment)
		panicIf(err)
	}

	_, err = fh.Write(src)
	panicIf(err)
}

func GeneratedBy(by string) string {
	return fmt.Sprintf(GeneratedComment, by)
}

const GeneratedComment = "// Code generated by %s DO NOT EDIT.\n"

func WriteGeneratedFile(fset *token.FileSet, filename string, f *ast.File, by string) {
	WriteFile(fset, filename, f, GeneratedBy(by))
}

func FmtFile(fset *token.FileSet, f *ast.File) []byte {
	buf := new(bytes.Buffer)
	err := format.Node(buf, fset, f)
	panicIf(err)
	return buf.Bytes()
}

func WriteFileRaw(output io.Writer, f *ast.File) {
	p := printer.Config{Tabwidth: 8, Mode: printer.RawFormat | normalizeNumbers}
	err := p.Fprint(output, token.NewFileSet(), f)
	panicIf(err)
}

const normalizeNumbers = 1 << 30
