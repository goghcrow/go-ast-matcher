package matcher

import (
	"golang.org/x/tools/go/callgraph"
	"golang.org/x/tools/go/callgraph/cha"
	"golang.org/x/tools/go/callgraph/rta"
	"golang.org/x/tools/go/callgraph/static"
	"golang.org/x/tools/go/callgraph/vta"
	"golang.org/x/tools/go/ssa"
	"golang.org/x/tools/go/ssa/ssautil"
)

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ Option ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type CallGraphFlags struct {
	LoadFlags
}

type CallGraphOption func(*CallGraphFlags)

func WithCallGraphBuildTag(tag string) CallGraphOption {
	return func(opts *CallGraphFlags) { opts.BuildTag = tag }
}
func WithCallGraphGopath(gopath string) CallGraphOption {
	return func(opts *CallGraphFlags) { opts.Gopath = gopath }
}
func WithCallGraphLoadTest() CallGraphOption { return func(opts *CallGraphFlags) { opts.Test = true } }
func WithCallGraphSuppressErrors() CallGraphOption {
	return func(opts *CallGraphFlags) { opts.PrintErrors = false }
}

// ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓ CallGraph ↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓↓

type Algo uint64

const (
	Static Algo = iota
	CHA
	RTA
	VTA
)

type CallGraph struct {
	*CallGraphFlags
	*Loader

	Prog  *ssa.Program
	Pkgs  []*ssa.Package
	Mains []*ssa.Package

	Graph *callgraph.Graph
}

func NewCallGraph(
	dir string,
	patterns []string,
	opts ...CallGraphOption,
) *CallGraph {
	flags := &CallGraphFlags{}
	flags.PrintErrors = true
	flags.LoadDepts = true // must
	for _, opt := range opts {
		opt(flags)
	}
	g := &CallGraph{Loader: NewLoader(), CallGraphFlags: flags}
	g.Load(dir, patterns, flags.LoadFlags)
	g.analysis()
	return g
}

func (c *CallGraph) analysis() {
	var (
		prog     *ssa.Program
		initPkgs []*ssa.Package
	)

	mode := ssa.InstantiateGenerics
	prog, initPkgs = ssautil.AllPackages(c.Init, mode)
	prog.Build()

	if c.PrintErrors {
		c.doPrintErrors(initPkgs)
	}

	c.Prog = prog
	c.Pkgs = prog.AllPackages()
	c.Mains = c.mainPackages()
}

func (c *CallGraph) doPrintErrors(initPkgs []*ssa.Package) {
	for i, p := range initPkgs {
		if p == nil && c.Init[i].Name != "" {
			errLog("Fail getting SSA for pkg: " + c.Init[i].PkgPath)
		}
	}
}

func (c *CallGraph) mainPackages() (mains []*ssa.Package) {
	for _, p := range c.Pkgs {
		if p != nil && p.Pkg.Name() == "main" && p.Func("main") != nil {
			mains = append(mains, p)
		}
	}
	return
}

func (c *CallGraph) CallGraph(algo Algo) (cg *callgraph.Graph) {
	switch algo {
	case Static:
		cg = static.CallGraph(c.Prog)
	case CHA:
		cg = cha.CallGraph(c.Prog)
	case RTA:
		var roots []*ssa.Function
		for _, main := range c.Mains {
			roots = append(roots, main.Func("init"), main.Func("main"))
		}
		cg = rta.Analyze(roots, true).CallGraph
	case VTA:
		cg = vta.CallGraph(ssautil.AllFunctions(c.Prog), cha.CallGraph(c.Prog))
	default:
		panic("unsupported algo")
	}
	cg.DeleteSyntheticNodes()
	return
}
