GHC = stack ghc --
PANDOC = pandoc

SRC = SatysfiFilter.hs \
  SatysfiFilter/Config.hs \
  SatysfiFilter/Misc.hs \
  SatysfiFilter/Shell.hs

all: test
test: test/test.html

test/test.html: test/test.md satysfiFilter
	pandoc -f markdown -t html --filter ./satysfiFilter -o $@ $<
	cat $@

satysfiFilter: $(SRC)
	$(GHC) --make -o $@ $<

.PHONY: clean
clean:
	rm -f satysfiFilter test/test.html
