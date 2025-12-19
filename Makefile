.ONESHELL:

all: setup build test lint

.PHONY: build
build:
	cabal build

.PHONY: repl
repl:
	cabal repl

.PHONY: test
test:
	@r=$$(cabal build spec 2>&1)
	if [ "$$?" -ne 0 ]; then
	  echo "$$r"
	  exit 1
	fi
	cabal run spec

.PHONY: clean
clean:
	cabal clean

.PHONY: doc
doc:
	cabal haddock --haddock-hyperlink-source

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
