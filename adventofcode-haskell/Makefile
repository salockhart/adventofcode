year := $(shell date +'%Y')
day := $(shell date +'%d')

help:
	@echo "adventofcode"
	@echo "Usage: make [target]"
	@echo ""
	@echo "Commands"
	@awk '/^##/{c=substr($$0,4);next}c&&/^[[:alpha:]][[:alnum:]_-]+:/{print substr($$1,1,index($$1,":")),c}1{c=0}' $(MAKEFILE_LIST) | column -s: -t

## build the project, defaulting to the current year/day
build:
	@stack build

## test the project, defaulting to the current year/day
test: build
	@stack test adventofcode$(year) --allow-different-user

## test the project, reloading when files change
test-watch: build
	@stack test adventofcode$(year) --file-watch --allow-different-user

solve:
	@stack ghci \
		adventofcode$(year) \
		--ghci-options \
		./src/Day$(day).hs

## debug
debug:
	@echo year=$(year) day=$(day)
