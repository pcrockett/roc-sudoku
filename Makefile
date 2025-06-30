all: clean build test
.PHONY: all

clean:
	rm -rf build
.PHONY: clean

build: build/main
.PHONY: build

run: build
	@./build/main
.PHONY: run

test: build
	roc test
.PHONY: test

build/main: main.roc
	mkdir -p build
	roc build --output build/
