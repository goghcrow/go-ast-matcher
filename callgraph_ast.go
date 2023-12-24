package matcher

import (
	"fmt"
	"go/ast"
	"go/build"
	"go/types"
	"net/url"
	"os/exec"
	"strconv"
	"strings"

	"golang.org/x/tools/go/ast/astutil"
)

// CallGraph static-ast-based call graph
func (m *Matcher) CallGraph(
	callerPattern *ast.FuncDecl,
	calleeOf Predicate[types.Object],
) *Graph {

	var objectOfCall func(m *Matcher, call ast.Expr) types.Object
	objectOfCall = func(m *Matcher, call ast.Expr) types.Object {
		switch calleeExpr := call.(type) {
		case *ast.Ident:
			return m.ObjectOf(calleeExpr)
		case *ast.SelectorExpr:
			sel := m.Selections[calleeExpr]
			if sel == nil {
				return nil
			}
			// sig := sel.Type().(*types.Signature)
			return sel.Obj()
		case *ast.IndexExpr: // generic
			return objectOfCall(m, calleeExpr.X)
		}
		return nil
	}

	g := NewGraph()

	callPtn := CalleeOf(m, calleeOf)
	m.Match(callerPattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		funDecl := c.Node().(*ast.FuncDecl)
		callerObj := m.ObjectOf(funDecl.Name)
		fun := callerObj.(*types.Func)

		m.MatchNode(callPtn, funDecl, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			callExpr := c.Node().(*ast.CallExpr)
			calleeExpr := callExpr.Fun
			calleeObj := objectOfCall(m, calleeExpr)
			if calleeObj == nil {
				if m.PrintErrors {
					errLog("empty callee object: " + m.ShowNodeWithPos(c.Node()))
				}
				return
			}

			switch obj := calleeObj.(type) {
			case *types.Func:
				caller := fun
				callee := obj
				g.addStaticEdge(callExpr, caller, callee)
			case *types.Var:
				caller := fun
				callee := obj
				g.addDynamicEdge(callExpr, caller, callee)
			default:
				panic("unreachable")
			}
		})
	})

	return g
}

type (
	ID       = string
	NodeType int
)

const (
	NodeFunc NodeType = iota + 1
	NodeRecv
	NodePkg
)

type Node struct {
	ID    ID       `json:"id"`
	PID   ID       `json:"parent"`
	Label string   `json:"label"`
	Desc  string   `json:"desc"`
	Attrs []string `json:"attrs"`

	Type   NodeType
	Object any // union of  *types.Func | *types.Var |*types.Package
}

type Edge struct {
	ID     ID       `json:"id"`
	Source ID       `json:"source"`
	Target ID       `json:"target"`
	Attrs  []string `json:"attrs"`
}

type Graph struct {
	idCounter uint64
	idMap     map[string]ID

	Nodes map[ID]*Node
	Edges map[ID]*Edge
}

func NewGraph() *Graph {
	return &Graph{
		idMap: make(map[string]ID),
		Nodes: make(map[ID]*Node),
		Edges: make(map[ID]*Edge),
	}
}

type Dot string

func (dot Dot) OpenOnline() {
	// https://dreampuf.github.io/GraphvizOnline
	// https://edotor.net/
	// https://www.devtoolsdaily.com/graphviz
	// http://magjac.com/graphviz-visual-editor/
	cmd := exec.Command("open", "https://dreampuf.github.io/GraphvizOnline/#"+url.PathEscape(string(dot)))
	_, _ = cmd.Output()
}

func (dot Dot) String() string {
	return string(dot)
}

func (g *Graph) Dot() Dot {
	dot := strings.Builder{}
	dot.WriteString(`
digraph callgraph {
	outputorder=edgesfirst
	graph[rankdir=LR, center=true]
	node [color=grey, style=filled, fontname="Sans serif", fontsize=13]
	// node[shape=none,  fontsize=13]
	edge[arrowsize=0.6, arrowhead=vee, color=gray]
`)

	for _, node := range g.Nodes {
		switch node.Type {
		case NodeFunc:
			// isFun := strings.Index(node.Label, ".") == -1
			f := node.Object.(*types.Func)
			isFun := f.Type().(*types.Signature).Recv() == nil
			if isFun {
				dot.WriteString(fmt.Sprintf("\t\"%s\" [color=lightblue, target=\"_top\"]\n", node.Label))
			} else {
				dot.WriteString(fmt.Sprintf("\t\"%s\" [color=lightpink, target=\"_top\"]\n", node.Label))
			}
		case NodeRecv:
			dot.WriteString(fmt.Sprintf("\t\"%s\" [fontcolor=\"#3182bd\", target=\"_top\"]\n", node.Label))
		case NodePkg:
			dot.WriteString(fmt.Sprintf("\t\"%s\" [fontcolor=\"#3182bd\", target=\"_top\"]\n", node.Label))
		default:
			panic("unreached")
		}

	}
	for _, edge := range g.Edges {
		from := g.Nodes[edge.Source].Label
		to := g.Nodes[edge.Target].Label
		dot.WriteString(fmt.Sprintf("\t\"%s\" -> \"%s\"\n", from, to))
	}
	dot.WriteString("}")
	return Dot(dot.String())
}

func (g *Graph) getID(fullName string, isNode bool) (id ID, isNew bool) {
	if id, ok := g.idMap[fullName]; ok {
		return id, false
	}

	g.idCounter++
	id = "e"
	if isNode {
		id = "n"
	}
	id += strconv.FormatUint(g.idCounter, 16)
	g.idMap[fullName] = id
	return id, true
}

func (g *Graph) addNode(fun *types.Func) ID {
	funcName := g.normalizeFunName(fun.FullName())
	fullName := fmt.Sprintf("func ~ %s", funcName)
	id, isNew := g.getID(fullName, true)
	if !isNew {
		return id
	}

	node := &Node{
		ID: id,

		Type:   NodeFunc,
		Object: fun,
	}

	if false { // TODO
		node.PID = g.addPkgNode(fun.Pkg())
	}

	if last := strings.LastIndex(funcName, "."); last >= 0 {
		// node.Label = funcName[last:]
		node.Label = funcName
	} else {
		node.Label = funcName
	}

	sig := fun.Type().(*types.Signature)
	if recv := sig.Recv(); recv != nil {
		if false { // TODO
			node.PID = g.addRecvTypeNode(recv)
		}
	}

	inGoRoot := func(pkg *types.Package) bool {
		if pkg == nil {
			return true
		}
		buildPkg, _ := build.Import(g.pkgPath(pkg), "", 0)
		return buildPkg.Goroot
	}

	if inGoRoot(fun.Pkg()) {
		node.Attrs = append(node.Attrs, "go_root")
	}
	if fun.Parent() == nil {
		node.Attrs = append(node.Attrs, "global")
	}
	if !fun.Exported() {
		node.Attrs = append(node.Attrs, "unexported")
	}

	g.Nodes[id] = node
	return id
}

func (g *Graph) addDynamicEdge(callExpr *ast.CallExpr, caller *types.Func, callee *types.Var) ID {
	// TODO
	return ""
}

func (g *Graph) addStaticEdge(callExpr *ast.CallExpr, caller, callee *types.Func) ID {
	// fullName := fmt.Sprintf("call @%d ~ %s -> %s",
	// 	callExpr.Pos(), normalize(caller.FullName()), normalize(callee.FullName()))

	// rm Pos, only addOnce
	fullName := fmt.Sprintf("call %s -> %s", g.normalizeFunName(caller.FullName()), g.normalizeFunName(callee.FullName()))
	id, isNew := g.getID(fullName, true)
	if !isNew {
		return id
	}

	callerId := g.addNode(caller)

	// todo 排除
	if callee.Name() == "UnwrapErr" {
		return id
	}

	if callee.Pkg().Name() == "pkg" {
		calleeId := g.addNode(callee)
		cEdge := &Edge{
			ID:     id,
			Source: callerId,
			Target: calleeId,
			Attrs:  []string{"static"}, // todo go / defer / closure call ?
		}

		g.Edges[id] = cEdge
	}

	return id
}

func (g *Graph) addRecvTypeNode(recv *types.Var) ID {
	pkg := recv.Pkg()
	tyStr := recv.Type().String()

	fullName := fmt.Sprintf("recv ~ %s ~ %s", g.pkgPath(pkg), tyStr)
	id, isNew := g.getID(fullName, true)
	if !isNew {
		return id
	}

	node := &Node{
		ID:  id,
		PID: g.addPkgNode(recv.Pkg()),

		Type:   NodeRecv,
		Object: recv,
	}

	if last := strings.LastIndex(tyStr, "."); last >= 0 {
		node.Label = tyStr[last+1:]
	} else {
		node.Label = tyStr
	}

	node.Attrs = append(node.Attrs, "type")
	if recv.Embedded() {
		node.Attrs = append(node.Attrs, "embedded")
	}
	if recv.IsField() {
		node.Attrs = append(node.Attrs, "field")
	}
	if !recv.Exported() {
		node.Attrs = append(node.Attrs, "unexported")
	}

	g.Nodes[id] = node
	return id
}

func (g *Graph) addPkgNode(pkg *types.Package) ID {
	if pkg == nil {
		return ""
	}
	pkgPath := g.pkgPath(pkg)

	fullName := fmt.Sprintf("pkg ~ %s", pkgPath)
	id, isNew := g.getID(fullName, true)
	if !isNew {
		return id
	}

	path := pkgPath
	node := &Node{
		ID:    id,
		Label: pkg.Name(),
		Desc:  path,
		Attrs: []string{"package"},

		Type:   NodePkg,
		Object: pkg,
	}
	g.Nodes[id] = node
	return id
}

func (g *Graph) normalizeFunName(id string) string {
	return strings.ReplaceAll(id, "command-line-arguments.", "")
}

func (g *Graph) pkgPath(pkg *types.Package) string {
	if pkg == nil {
		return ""
	}
	if pkg.Path() == "command-line-arguments" {
		return ""
	}
	return pkg.Path()
}
