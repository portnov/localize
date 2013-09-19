GHC=ghc --make

all: hgettext example

hgettext: hgettext.hs
	$(GHC) $<

example:
	$(MAKE) -C examples/

clean:
	find . -name *.o -delete
	find . -name *.hi -delete
	$(MAKE) -C examples/ clean
