GHC = stack ghc --
PANDOC = pandoc

SRC = SatysfiFilter.hs \
  SatysfiFilter/Config.hs \
  SatysfiFilter/Misc.hs \
  SatysfiFilter/Shell.hs

all: test
test: test/test.html

test/test.html: test/test.md satysfiFilter
	cd test \
	&& pandoc -f markdown -t html --filter ../satysfiFilter -o test.html test.md

satysfiFilter: $(SRC)
	$(GHC) --make -o $@ $<

.PHONY: clean
clean:
	rm -rf satysfiFilter test/test.html test/generated
