GHC = stack ghc --
PANDOC = pandoc

all: test
test: test/test.html

test/test.html: test/test.md satysfiFilter
	pandoc -f markdown -t html --filter ./satysfiFilter -o $@ $<
	cat $@

satysfiFilter: satysfiFilter.hs
	$(GHC) --make -o $@ $^

.PHONY: clean
clean:
	rm -f satysfiFilter test/test.html
