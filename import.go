package matcher

import (
	"encoding/json"
	"go/ast"
	"go/token"
	"go/types"
	"sort"
	"strconv"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
	"golang.org/x/tools/go/packages"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓ uses import ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func UsesImport(m *Matcher, file *ast.File, pkg *types.Package) (use bool) {
	if ImportsAs(file, "_", pkg.Path()) {
		return true
	}

	usePkg := func(id *ast.Ident) bool {
		switch obj := m.ObjectOf(id).(type) {
		case nil:
			return false
		case *types.PkgName:
			return obj.Imported() == pkg
		default:
			return obj.Pkg() == pkg
		}
	}

	astutil.Apply(file, nil, func(c *astutil.Cursor) bool {
		if _, is := c.Parent().(*ast.ImportSpec); is {
			return true
		}
		n := c.Node()
		switch n := n.(type) {
		case *ast.Ident:
			use = usePkg(n)
		case *ast.SelectorExpr:
			use = usePkg(n.Sel)
		case *ast.Field:
			for _, name := range n.Names {
				use = usePkg(name)
				if use {
					break
				}
			}
		}
		return !use // stop walking
	})
	return
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ uses import ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func OptimizeImports(
	m *Matcher,
	file *ast.File,
	projectPkgPrefix string,
	companyPkgPrefix []string,
) {
	CleanImports(m, file)
	SortImports(file, projectPkgPrefix, companyPkgPrefix)
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ clean unused import ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

// CleanImports dedup and remove unused
func CleanImports(m *Matcher, file *ast.File) {
	i := &importCleaner{
		m:         m,
		file:      file,
		importMap: map[PackagePath]*packages.Package{},
		useMap:    map[PackagePath]bool{},
		dedup:     map[string]bool{},
	}

	i.imports()
	i.uses()
	i.rebuild()
}

type importCleaner struct {
	m         *Matcher
	file      *ast.File
	importMap map[PackagePath]*packages.Package
	useMap    map[PackagePath]bool
	dedup     map[string]bool
}

func (i *importCleaner) imports() {
	walkImportGroup(i.file, func(d *ast.GenDecl) {
		for _, spec := range d.Specs {
			s := spec.(*ast.ImportSpec)
			path := ImportPath(s)
			pkg, ok := i.m.All[path]
			assert(ok, path+" not loaded, missing matcher.WithLoadDepts() ?")
			i.importMap[path] = pkg
		}
	})
}

func (i *importCleaner) uses() {
	walkImportGroup(i.file, func(d *ast.GenDecl) {
		for _, spec := range d.Specs {
			s := spec.(*ast.ImportSpec)
			path := ImportPath(s)
			if s.Name.String() == "_" {
				i.useMap[path] = true
			}
		}
	})

	astutil.Apply(i.file, nil, func(c *astutil.Cursor) bool {
		if _, is := c.Parent().(*ast.ImportSpec); is {
			return true
		}
		n := c.Node()
		switch n := n.(type) {
		case *ast.Ident:
			i.markUses(n)
		case *ast.SelectorExpr:
			i.markUses(n.Sel)
		case *ast.Field:
			for _, name := range n.Names {
				i.markUses(name)
			}
		}
		return true
	})
}

func (i *importCleaner) markUses(id *ast.Ident) {
	switch obj := i.m.ObjectOf(id).(type) {
	case nil:
		return
	case *types.PkgName: // pkgName.xxx
		assert(obj.Pkg() == i.m.Package, "illegal state")
		pkg := obj.Imported()
		path := pkg.Path()
		if imported, ok := i.importMap[path]; ok {
			assert(pkg == imported.Types, "illegal state")
			i.useMap[path] = true
		}
	default: // import . "xxxx"
		selfPkg := obj.Pkg() == i.m.Package
		if selfPkg || obj.Pkg() == nil {
			return
		}
		path := obj.Pkg().Path()
		// !ok e.g. a.b.c, c in other pkg, no need importing
		if pkg, ok := i.importMap[path]; ok {
			assert(obj.Pkg() == pkg.Types, "illegal state")
			i.useMap[path] = true
		}
	}
}

func (i *importCleaner) rebuild() {
	walkImportGroup(i.file, func(d *ast.GenDecl) {
		specs := make([]ast.Spec, 0, len(d.Specs))
		for _, spec := range d.Specs {
			s := spec.(*ast.ImportSpec)
			name := ImportName(s)
			path := ImportPath(s)
			if i.keepImport(name, path) {
				namePath := FmtImport(s)
				s := &ast.ImportSpec{Path: &ast.BasicLit{Value: namePath, Kind: d.Tok}}
				specs = append(specs, s)
			}
		}

		d.Specs = specs
	})

	i.clearEmptyDecl()
}

func (i *importCleaner) keepImport(name, path string) bool {
	id := name + "#" + path
	use := i.useMap[path]
	dup := i.dedup[id]
	if !use || dup {
		return false
	}
	i.dedup[id] = true
	return true
}

func (i *importCleaner) clearEmptyDecl() {
	xs := make([]ast.Decl, 0, len(i.file.Decls))
	for _, decl := range i.file.Decls {
		d, ok := decl.(*ast.GenDecl)
		if !ok || d.Tok != token.IMPORT || len(d.Specs) > 0 {
			xs = append(xs, decl)
		}
	}
	i.file.Decls = xs
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ sort import ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func SortImports(
	file *ast.File,
	projectPkgPrefix string,
	companyPkgPrefix []string,
) {
	walkImportGroup(file, func(d *ast.GenDecl) {
		// only sort imports in block
		if !d.Lparen.IsValid() {
			return
		}

		i := &importSorter{
			decl:             d,
			projectPkgPrefix: projectPkgPrefix,
			companyPkgPrefix: companyPkgPrefix,
		}
		i.collect()
		i.sort()
		i.rebuild()
	})
}

type importSorter struct {
	decl *ast.GenDecl

	projectPkgPrefix string
	companyPkgPrefix []string

	std     []string
	other   []string
	company []string
	project []string
}

func (i *importSorter) collect() {
	for _, spec := range i.decl.Specs {
		s := spec.(*ast.ImportSpec)
		path := ImportPath(s)
		namePath := FmtImport(s)
		if i.isStd(path) {
			i.std = append(i.std, namePath)
		} else if i.isCompany(path) {
			i.company = append(i.company, namePath)
		} else if i.isProject(path) {
			i.project = append(i.project, namePath)
		} else {
			i.other = append(i.other, namePath)
		}
	}
}

func (i *importSorter) sort() {
	sort.Strings(i.std)
	sort.Strings(i.other)
	sort.Strings(i.company)
	sort.Strings(i.project)
}

func (i *importSorter) isStd(pkg string) bool     { return stdPkg[pkg] }
func (i *importSorter) isProject(pkg string) bool { return strings.HasPrefix(pkg, i.projectPkgPrefix) }
func (i *importSorter) isCompany(pkg string) bool {
	for _, prefix := range i.companyPkgPrefix {
		if strings.HasPrefix(pkg, prefix) &&
			!strings.HasPrefix(pkg, i.projectPkgPrefix) {
			return true
		}
	}
	return false
}

func (i *importSorter) sorting() [][]string {
	return [][]string{i.std, i.other, i.company, i.project}
}

func (i *importSorter) rebuild() {
	var specs []ast.Spec

	sorting := i.sorting()
	for j, xs := range sorting {
		for _, v := range xs {
			specs = append(specs, &ast.ImportSpec{
				Path: &ast.BasicLit{Value: v, Kind: i.decl.Tok},
			})
		}
		if len(xs) != 0 && j != len(sorting)-1 {
			s := &ast.ImportSpec{Path: &ast.BasicLit{Value: "", Kind: token.STRING}}
			specs = append(specs, s)
		}
	}

	i.decl.Specs = specs
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ etc ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

func walkImportGroup(file *ast.File, f func(*ast.GenDecl)) {
	for _, decl := range file.Decls {
		d, ok := decl.(*ast.GenDecl)
		if !ok || d.Tok != token.IMPORT {
			continue
		}
		f(d)
	}
}

func ImportsAs(f *ast.File, name, path string) bool {
	for _, s := range f.Imports {
		if ImportPath(s) == path {
			return s.Name.String() == name
		}
	}
	return false
}

func Imports(f *ast.File, path string) bool {
	return ImportSpec(f, path) != nil
}

func ImportSpec(f *ast.File, path string) *ast.ImportSpec {
	for _, s := range f.Imports {
		if ImportPath(s) == path {
			return s
		}
	}
	return nil
}

func ImportPath(s *ast.ImportSpec) string {
	// for unparsed: ast.File.Decls.Specs(ImportSpec).Path.Value
	// for parsed: ast.File.Imports
	path := s.Path.Value
	xs := strings.Split(path, " ")
	if len(xs) > 1 {
		path = xs[1]
	}
	t, err := strconv.Unquote(path)
	panicIfErr(err)
	return t
}

func ImportName(s *ast.ImportSpec) string {
	name := s.Name.String()
	xs := strings.Split(s.Path.Value, " ")
	if len(xs) > 1 {
		assert(name == "<nil>", "illegal state")
		name = xs[0]
	}
	if name == "<nil>" {
		return ""
	}
	return name
}

func ParseImportName(f *ast.File, path string, pkgName string) (name string) {
	spec := ImportSpec(f, path)
	if spec == nil {
		return
	}
	name = spec.Name.String()
	if name == "<nil>" {
		return ""
	}
	return
}

func FmtImport(s *ast.ImportSpec) string {
	if s.Name == nil {
		return s.Path.Value
	}
	return s.Name.Name + " " + s.Path.Value
}

// ↓↓↓↓↓↓↓↓↓↓↓↓ std Packages ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

const stdPkgJson = `{"archive/tar":true,"archive/zip":true,"bufio":true,"bytes":true,"cmp":true,"compress/bzip2":true,"compress/flate":true,"compress/gzip":true,"compress/lzw":true,"compress/zlib":true,"container/heap":true,"container/list":true,"container/ring":true,"context":true,"crypto":true,"crypto/aes":true,"crypto/cipher":true,"crypto/des":true,"crypto/dsa":true,"crypto/ecdh":true,"crypto/ecdsa":true,"crypto/ed25519":true,"crypto/elliptic":true,"crypto/hmac":true,"crypto/internal/alias":true,"crypto/internal/bigmod":true,"crypto/internal/boring":true,"crypto/internal/boring/bbig":true,"crypto/internal/boring/bcache":true,"crypto/internal/boring/sig":true,"crypto/internal/edwards25519":true,"crypto/internal/edwards25519/field":true,"crypto/internal/nistec":true,"crypto/internal/nistec/fiat":true,"crypto/internal/randutil":true,"crypto/md5":true,"crypto/rand":true,"crypto/rc4":true,"crypto/rsa":true,"crypto/sha1":true,"crypto/sha256":true,"crypto/sha512":true,"crypto/subtle":true,"crypto/tls":true,"crypto/x509":true,"crypto/x509/internal/macos":true,"crypto/x509/pkix":true,"database/sql":true,"database/sql/driver":true,"debug/buildinfo":true,"debug/dwarf":true,"debug/elf":true,"debug/gosym":true,"debug/macho":true,"debug/pe":true,"debug/plan9obj":true,"embed":true,"embed/internal/embedtest":true,"encoding":true,"encoding/ascii85":true,"encoding/asn1":true,"encoding/base32":true,"encoding/base64":true,"encoding/binary":true,"encoding/csv":true,"encoding/gob":true,"encoding/hex":true,"encoding/json":true,"encoding/pem":true,"encoding/xml":true,"errors":true,"expvar":true,"flag":true,"fmt":true,"go/ast":true,"go/build":true,"go/build/constraint":true,"go/constant":true,"go/doc":true,"go/doc/comment":true,"go/format":true,"go/importer":true,"go/internal/gccgoimporter":true,"go/internal/gcimporter":true,"go/internal/srcimporter":true,"go/internal/typeparams":true,"go/parser":true,"go/printer":true,"go/scanner":true,"go/token":true,"go/types":true,"hash":true,"hash/adler32":true,"hash/crc32":true,"hash/crc64":true,"hash/fnv":true,"hash/maphash":true,"html":true,"html/template":true,"image":true,"image/color":true,"image/color/palette":true,"image/draw":true,"image/gif":true,"image/internal/imageutil":true,"image/jpeg":true,"image/png":true,"index/suffixarray":true,"internal/abi":true,"internal/bisect":true,"internal/buildcfg":true,"internal/bytealg":true,"internal/cfg":true,"internal/coverage":true,"internal/coverage/calloc":true,"internal/coverage/cformat":true,"internal/coverage/cmerge":true,"internal/coverage/decodecounter":true,"internal/coverage/decodemeta":true,"internal/coverage/encodecounter":true,"internal/coverage/encodemeta":true,"internal/coverage/pods":true,"internal/coverage/rtcov":true,"internal/coverage/slicereader":true,"internal/coverage/slicewriter":true,"internal/coverage/stringtab":true,"internal/coverage/test":true,"internal/coverage/uleb128":true,"internal/cpu":true,"internal/dag":true,"internal/diff":true,"internal/fmtsort":true,"internal/fuzz":true,"internal/goarch":true,"internal/godebug":true,"internal/godebugs":true,"internal/goexperiment":true,"internal/goos":true,"internal/goroot":true,"internal/goversion":true,"internal/intern":true,"internal/itoa":true,"internal/lazyregexp":true,"internal/lazytemplate":true,"internal/nettrace":true,"internal/obscuretestdata":true,"internal/oserror":true,"internal/pkgbits":true,"internal/platform":true,"internal/poll":true,"internal/profile":true,"internal/race":true,"internal/reflectlite":true,"internal/safefilepath":true,"internal/saferio":true,"internal/singleflight":true,"internal/syscall/execenv":true,"internal/syscall/unix":true,"internal/sysinfo":true,"internal/testenv":true,"internal/testlog":true,"internal/testpty":true,"internal/trace":true,"internal/txtar":true,"internal/types/errors":true,"internal/unsafeheader":true,"internal/xcoff":true,"internal/zstd":true,"io":true,"io/fs":true,"io/ioutil":true,"log":true,"log/internal":true,"log/slog":true,"log/slog/internal":true,"log/slog/internal/benchmarks":true,"log/slog/internal/buffer":true,"log/slog/internal/slogtest":true,"log/syslog":true,"maps":true,"math":true,"math/big":true,"math/bits":true,"math/cmplx":true,"math/rand":true,"mime":true,"mime/multipart":true,"mime/quotedprintable":true,"net":true,"net/http":true,"net/http/cgi":true,"net/http/cookiejar":true,"net/http/fcgi":true,"net/http/httptest":true,"net/http/httptrace":true,"net/http/httputil":true,"net/http/internal":true,"net/http/internal/ascii":true,"net/http/internal/testcert":true,"net/http/pprof":true,"net/internal/socktest":true,"net/mail":true,"net/netip":true,"net/rpc":true,"net/rpc/jsonrpc":true,"net/smtp":true,"net/textproto":true,"net/url":true,"os":true,"os/exec":true,"os/exec/internal/fdtest":true,"os/signal":true,"os/user":true,"path":true,"path/filepath":true,"plugin":true,"reflect":true,"reflect/internal/example1":true,"reflect/internal/example2":true,"regexp":true,"regexp/syntax":true,"runtime":true,"runtime/cgo":true,"runtime/coverage":true,"runtime/debug":true,"runtime/internal/atomic":true,"runtime/internal/math":true,"runtime/internal/sys":true,"runtime/internal/wasitest":true,"runtime/metrics":true,"runtime/pprof":true,"runtime/race":true,"runtime/trace":true,"slices":true,"sort":true,"strconv":true,"strings":true,"sync":true,"sync/atomic":true,"syscall":true,"testing":true,"testing/fstest":true,"testing/internal/testdeps":true,"testing/iotest":true,"testing/quick":true,"testing/slogtest":true,"text/scanner":true,"text/tabwriter":true,"text/template":true,"text/template/parse":true,"time":true,"time/tzdata":true,"unicode":true,"unicode/utf16":true,"unicode/utf8":true,"unsafe":true,"vendor/golang.org/x/crypto/chacha20":true,"vendor/golang.org/x/crypto/chacha20poly1305":true,"vendor/golang.org/x/crypto/cryptobyte":true,"vendor/golang.org/x/crypto/cryptobyte/asn1":true,"vendor/golang.org/x/crypto/hkdf":true,"vendor/golang.org/x/crypto/internal/alias":true,"vendor/golang.org/x/crypto/internal/poly1305":true,"vendor/golang.org/x/net/dns/dnsmessage":true,"vendor/golang.org/x/net/http/httpguts":true,"vendor/golang.org/x/net/http/httpproxy":true,"vendor/golang.org/x/net/http2/hpack":true,"vendor/golang.org/x/net/idna":true,"vendor/golang.org/x/net/nettest":true,"vendor/golang.org/x/net/route":true,"vendor/golang.org/x/sys/cpu":true,"vendor/golang.org/x/text/secure/bidirule":true,"vendor/golang.org/x/text/transform":true,"vendor/golang.org/x/text/unicode/bidi":true,"vendor/golang.org/x/text/unicode/norm":true}`

var stdPkg = map[string]bool{}

func init() {
	_ = json.Unmarshal([]byte(stdPkgJson), &stdPkg)

	if false {
		var stdPkg = map[string]bool{}
		xs, err := packages.Load(nil, PatternStd)
		panicIfErr(err)
		for _, pkg := range xs {
			stdPkg[pkg.ID] = true
		}
		raw, err := json.Marshal(stdPkg)
		panicIfErr(err)
		println(string(raw))
	}
}
