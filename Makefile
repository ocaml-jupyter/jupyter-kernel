
all: build

build:
	@dune build @install

clean:
	@dune clean

watch:
	@dune build @install -w
