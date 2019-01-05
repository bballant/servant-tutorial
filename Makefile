.DEFAULT_GOAL = help
export SHELL = /bin/bash

build: ## build and lint
	stack build $(STACK_OPTS) servant-tutorial
	find src -name '*.hs' | xargs hlint
	find src -name '*.hs' | xargs stylish-haskell -i
clean: ## clean
	stack clean
clobber: clean ## clean and remove stack's working directory
	rm -rf .stack-work/*
ghci: stack ghci ## ghci session
ghcid-devel: ## low-feature ghc-based IDE
	ghcid --command "stack ghci servant-tutorial"
start-server: ## start the local server on port 1234
	stack exec servant-tutorial
help: ## help screen
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
	| awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-18s\033[0m %s\n", $$1, $$2}'
setup: ## install required tools for this project
	brew update
	stack install hlint stylish-haskell
