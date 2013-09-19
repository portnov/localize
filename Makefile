GHC=ghc --make

all: example

example:
	$(MAKE) -C examples/

clean:
	find . -name *.o -delete
	find . -name *.hi -delete
	$(MAKE) -C examples/ clean
