package = purescript
exe_target = purs
stack_yaml = STACK_YAML="stack.yaml"
stack = $(stack_yaml) stack

build:
	$(stack) build $(package)

build-dirty:
	$(stack) build --ghc-options=-fforce-recomp $(package)

run:
	$(stack) build --fast && $(stack) exec -- $(package)

install:
	$(stack) install

ghci:
	$(stack) ghci $(package):lib

test:
	$(stack) test --fast $(package)

test-ghci:
	$(stack) ghci $(package):test:$(package)-tests

# If you want to profile a particular test, such
# as LargeSumType.purs, add -p to the test arguments like so:
# stack test --executable-profiling --ta '-p LargeSum +RTS -pj -RTS'

# Also, you'll need flamegraph.pl and ghc-prof-aeson-flamegraph
# (cf. dev-deps), I git cloned the FlameGraph repository and
# symlinked the Perl script into my path.
# Open the SVG with your browser, you can reload the browser when you
# rerun the profiled test run.
test-profiling:
	$(stack) test --executable-profiling --ta '+RTS -pj -RTS' $(package)
	cat tests.prof | stack exec ghc-prof-aeson-flamegraph | flamegraph.pl > tests.svg

bench:
	$(stack) bench $(package)

ghcid:
	$(stack) exec -- ghcid -c "stack ghci $(package):lib --test --ghci-options='-fobject-code -fno-warn-unused-do-bind'"

# if you want these to be globally available run it outside of purescript
# but incompatibilities might arise between ghcid and the version of GHC
# you're using to build PureScript.
dev-deps:
	stack install ghcid ghc-prof-aeson-flamegraph

.PHONY : build build-dirty run install ghci test test-ghci test-profiling ghcid dev-deps
