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

## run and submit the solution
solve:
	@echo "main" | stack ghci \
		adventofcode$(year) \
		--ghci-options \
		./src/Day$(day).hs

## setup the project for a new day
setup:
	@cat templates/Day.hs.template | sed "s/{day}/$(day)/g" | sed "s/{year}/$(year)/g" > adventofcode$(year)/src/Day$(day).hs
	@cat templates/DaySpec.hs.template | sed "s/{day}/$(day)/g" | sed "s/{year}/$(year)/g" > adventofcode$(year)/test/Day$(day)Spec.hs

## debug
debug:
	@echo year=$(year) day=$(day)
