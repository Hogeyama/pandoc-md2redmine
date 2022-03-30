all: setup build test lint

.PHONY: build
build:
	cabal v2-build

.PHONY: repl
repl:
	cabal v2-repl

.PHONY: test
test: doctest spectest

.PHONY: doctest
doctest:
	cabal v2-test doctests

.PHONY: spectest
spectest:
	cabal v2-test spec

.PHONY: bench
bench:
	cabal v2-bench

.PHONY: clean
clean:
	cabal v2-clean

.PHONY: doc
doc:
	cabal v2-haddock --haddock-hyperlink-source

.PHONY: lint
lint:
	hlint app lib

.PHONY: format
format: format-src format-cabal

.PHONY: format-cabal
format-cabal:
	cabal-fmt pandoc-md2redmine.cabal -i

.PHONY: format-src
format-src:
	find . | grep '^./\(app\|lib\|test\).*\.hs' | xargs -n1 fourmolu -i
