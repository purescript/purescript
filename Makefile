package = purescript
exe_target = purs
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

help: ## Print documentation
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

ghcid: ## Run ghcid to quickly reload code on save.
	ghcid --command "stack ghci purescript:lib purescript:test:tests --ghci-options -fno-code"

ghcid-test: ## Run ghcid to quickly reload code and run tests on save.
	ghcid --command "stack ghci purescript:lib purescript:test:tests --ghci-options -fobject-code" \
	    --test "Main.main"

build: ## Build the package.
	$(stack) build $(package)

build-dirty: ## Force recompilation of the entire package.
	$(stack) build --ghc-options=-fforce-recomp $(package)

run: ## Run the compiler.
	$(stack) build --fast && $(stack) exec -- $(exe_target)

install: ## Install the executables to stack's path
	$(stack) install

ghci: ## Open GHCi with the PureScript library
	$(stack) ghci $(package):lib

test: ## Run the tests.
	$(stack) test --fast $(package)

test-ghci: ## Open GHCi with the test suite loaded.
	$(stack) ghci $(package):test:$(package)-tests

# If you want to profile a particular test, such
# as LargeSumType.purs, add -p to the test arguments like so:
# stack test --executable-profiling --ta '-p LargeSum +RTS -pj -RTS'

# Also, you'll need flamegraph.pl and ghc-prof-aeson-flamegraph
# (cf. dev-deps), I git cloned the FlameGraph repository and
# symlinked the Perl script into my path.
# Open the SVG with your browser, you can reload the browser when you
# rerun the profiled test run.
test-profiling: ## Run the tests, with profiling enabled. Also builds a flamegraph of the test.
	$(stack) test --executable-profiling --ta '+RTS -pj -RTS' $(package)
	cat tests.prof | stack exec ghc-prof-aeson-flamegraph | flamegraph.pl > tests.svg

bench: ## Run benchmarks for PureScript
	$(stack) bench $(package)

# if you want these to be globally available run it outside of purescript
# but incompatibilities might arise between ghcid and the version of GHC
# you're using to build PureScript.
dev-deps: ## Install helpful development tools.
	stack install ghcid ghc-prof-aeson-flamegraph

.PHONY : build build-dirty run install ghci test test-ghci test-profiling ghcid dev-deps
