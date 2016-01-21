GHC ?= ghc-stage2

all: Main

submod:
	git config --local url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
	git config --local url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
	git config --local url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
	git config --local url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/ghc/packages/
	git config --local url."git@github.com:/ghc/packages-".insteadOf      git@github.com:/ghc/packages/
	git submodule update --init --recursive

Main: Main.hs
	$(GHC) --make -Wall Main.hs

clean:
	rm -f Main Main.hi Main.o

.PHONY: clean
