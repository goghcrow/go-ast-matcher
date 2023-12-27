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

type CallGraphFlags struct {
	FunDeclPattern   *ast.FuncDecl
	CallExprPattern  *ast.CallExpr
	ShowOrphanedNode bool
}

type CallGraphOption func(*CallGraphFlags)

func WithCallerPattern(funPtn *ast.FuncDecl) CallGraphOption {
	return func(flags *CallGraphFlags) { flags.FunDeclPattern = funPtn }
}
func WithCallExprPattern(callPtn *ast.CallExpr) CallGraphOption {
	return func(flags *CallGraphFlags) { flags.CallExprPattern = callPtn }
}
func WithShowOrphanedNode() CallGraphOption {
	return func(flags *CallGraphFlags) { flags.ShowOrphanedNode = true }
}

// CallGraph statical ast-pattern-based call graph
func (m *Matcher) CallGraph(opts ...CallGraphOption) *Graph {
	flags := &CallGraphFlags{
		FunDeclPattern: &ast.FuncDecl{}, // all func decl
		CallExprPattern: CalleeOf(m, func(obj types.Object) bool {
			return obj != nil
		}),
	}
	for _, opt := range opts {
		opt(flags)
	}

	g := newGraph()

	m.Match(flags.FunDeclPattern, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
		funDecl := c.Node().(*ast.FuncDecl)
		callerObj := m.ObjectOf(funDecl.Name)
		caller := callerObj.(*types.Func)

		if flags.ShowOrphanedNode {
			g.addNode(caller)
		}

		m.MatchNode(flags.CallExprPattern, funDecl, func(m *Matcher, c *astutil.Cursor, stack []ast.Node, binds Binds) {
			callExpr := c.Node().(*ast.CallExpr)
			calleeObj := m.ObjectOfCall(callExpr)
			if calleeObj == nil {
				if m.PrintErrors {
					errLog("empty callee object: " + m.ShowNodeWithPos(c.Node()))
				}
				return
			}

			switch callee := calleeObj.(type) {
			case *types.Func:
				g.addStaticEdge(callExpr, caller, callee)
			case *types.Var:
				g.addDynamicEdge(callExpr, caller, callee)
			case *types.Builtin:
				g.addBuiltinEdge(callExpr, caller, callee)
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

func newGraph() *Graph {
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
	edge[arrowsize=0.6, arrowhead=vee, color=gray]

`)

	for id, node := range g.Nodes {
		switch node.Type {
		case NodeFunc:
			// isFun := strings.Index(node.Label, ".") == -1
			f := node.Object.(*types.Func)
			isFun := f.Type().(*types.Signature).Recv() == nil
			color, fontSize := "lightpink", 13
			if isFun {
				color = "lightblue"
			}
			// if f.Exported() { fontSize = 14 }
			label := node.Label
			// label += "\n" + strings.Join(node.Attrs, ",")
			dot.WriteString(fmt.Sprintf("\t%s [label=%q, fontsize=%d, color=%q, target=\"_top\"]\n",
				id, label, fontSize, color))
		case NodeRecv:
			color := "#3182bd"
			dot.WriteString(fmt.Sprintf("\t%s [label=%q, fontcolor=%q, target=\"_top\"]\n",
				id, node.Label, color))
		case NodePkg:
			color := "#3182bd"
			dot.WriteString(fmt.Sprintf("\t%s [label=%q, fontcolor=%q, target=\"_top\"]\n",
				id, node.Label, color))
		default:
			panic("unreached")
		}

	}
	for _, edge := range g.Edges {
		dot.WriteString(fmt.Sprintf("\t%s -> %s\n", edge.Source, edge.Target))
	}
	dot.WriteString("}")
	return Dot(dot.String())
}

func (g *Graph) SubGraph(rootPred Predicate[*Node]) *Graph {
	findRoots := func() (roots []ID) {
		for id, node := range g.Nodes {
			if rootPred(node) {
				roots = append(roots, id)
			}
		}
		return
	}

	type (
		nodeID = ID
		edgeID = ID
		pair   = struct {
			edgeID
			nodeID
		}
	)
	edges := map[nodeID][]pair{}
	for eid, edge := range g.Edges {
		edges[edge.Source] = append(edges[edge.Source], pair{eid, edge.Target})
	}

	subGraph := newGraph()
	var dfs func(nodeID)
	dfs = func(source nodeID) {
		if subGraph.Nodes[source] != nil {
			return
		}
		subGraph.Nodes[source] = g.Nodes[source]
		for _, it := range edges[source] {
			subGraph.Edges[it.edgeID] = g.Edges[it.edgeID]
			dfs(it.nodeID)
		}
	}
	for _, root := range findRoots() {
		dfs(root)
	}
	return subGraph
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
	funcName := g.FunName(fun)
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
		buildPkg, _ := build.Import(g.PkgPath(pkg), "", 0)
		return buildPkg.Goroot
	}

	if inGoRoot(fun.Pkg()) {
		node.Attrs = append(node.Attrs, "go_root")
	}
	if fun.Parent() == nil {
		node.Attrs = append(node.Attrs, "global")
	}

	g.Nodes[id] = node
	return id
}

func (g *Graph) addDynamicEdge(callExpr *ast.CallExpr, caller *types.Func, callee *types.Var) ID {
	return "" // TODO
}

func (g *Graph) addBuiltinEdge(callExpr *ast.CallExpr, caller *types.Func, callee *types.Builtin) ID {
	return "" // TODO
}

func (g *Graph) addStaticEdge(callExpr *ast.CallExpr, caller, callee *types.Func) ID {
	// fullName := fmt.Sprintf("call @%d ~ %s -> %s",
	// 	callExpr.Pos(), normalize(caller.FullName()), normalize(callee.FullName()))

	// rm Pos, only addOnce
	fullName := fmt.Sprintf("call %s -> %s", g.FunName(caller), g.FunName(callee))
	id, isNew := g.getID(fullName, true)
	if !isNew {
		return id
	}

	callerId := g.addNode(caller)
	calleeId := g.addNode(callee)
	cEdge := &Edge{
		ID:     id,
		Source: callerId,
		Target: calleeId,
		Attrs:  []string{"static"}, // todo go / defer / closure call ?
	}

	g.Edges[id] = cEdge
	return id
}

func (g *Graph) addRecvTypeNode(recv *types.Var) ID {
	pkg := recv.Pkg()
	tyStr := recv.Type().String()

	fullName := fmt.Sprintf("recv ~ %s ~ %s", g.PkgPath(pkg), tyStr)
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

	g.Nodes[id] = node
	return id
}

func (g *Graph) addPkgNode(pkg *types.Package) ID {
	if pkg == nil {
		return ""
	}
	pkgPath := g.PkgPath(pkg)

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

func (g *Graph) PkgPath(pkg *types.Package) string {
	return PkgPath(pkg)
}

func (g *Graph) FunName(fun *types.Func) string {
	return FunName(fun)
}
