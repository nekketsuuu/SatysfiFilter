GHC = stack ghc --
PANDOC = pandoc

.PHONY: all test
all: test
test: test/test.html

test/test.html: test/test.md satysfiFilter
	pandoc -f markdown -t html --filter ./satysfiFilter -o $@ $<
	cat $@

satysfiFilter: satysfiFilter.hs
	$(GHC) --make -o $@ $^

clean:
	rm -f satysfiFilter test/test.html
