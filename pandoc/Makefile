GHC = stack ghc --
PANDOC = pandoc

SRC = SatysfiFilter.hs \
  SatysfiFilter/Config.hs \
  SatysfiFilter/Misc.hs \
  SatysfiFilter/Shell.hs
TRASH = SatysfiFilter.hi SatysfiFilter.o \
  SatysfiFilter/Config.hi SatysfiFilter/Config.o \
  SatysfiFilter/Misc.hi SatysfiFilter/Misc.o \
  SatysfiFilter/Shell.hi SatysfiFilter/Shell.o \
  satysfiFilter \
  test/test.html test/generated

all: satysfiFilter

satysfiFilter: $(SRC)
	$(GHC) --make -o $@ $<

test: test/test.html
test/test.html: test/test.md satysfiFilter
	cd test \
	&& pandoc -f markdown -t html --filter ../satysfiFilter -o test.html test.md

.PHONY: clean
clean:
	rm -rf $(TRASH)
