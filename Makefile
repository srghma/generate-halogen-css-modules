.PHONY: test

build:
	stack build

build_watch:
	stack build --file-watch

test:
	stack test

test_watch:
	stack test --file-watch
