GHC=ghc --make -i..

all: gmotest gmodump mos run

gmotest:
	$(GHC) gmotest.hs

gmodump:
	$(GHC) gmodump.hs

mos: mo/fr.mo mo/ru.mo

mo/%.mo: po/%.po
	msgfmt -o $@ $<

run: gmotest
	./gmotest

clean:
	find . -name *.o -delete
	find . -name *.hi -delete
	find . -name *.mo -delete
	rm -f gmotest gmodump

