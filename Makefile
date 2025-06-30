all: clean build
.PHONY: all

clean:
	rm -rf build
.PHONY: clean

build: build/main
.PHONY: build

run: build
	@./build/main
.PHONY: run

build/main: main.roc
	mkdir -p build
	roc build --output build/
