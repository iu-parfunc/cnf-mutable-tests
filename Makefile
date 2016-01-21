GHC ?= ghc-stage2

all: Main

submod:
	git clone --recursive git://git.haskell.org/ghc.git
	cd ghc
	git remote add fork git@github.com:iu-parfunc/ghc.git
	git fetch --all
	git checkout -f 9df22e29ddb74aca893925296e2606f3b962374b

# I can't get this to work:
submod2:
	git config --local url."git://github.com/ghc/packages-".insteadOf     git://github.com/iu-parfunc/packages/
	git config --local url."http://github.com/ghc/packages-".insteadOf    http://github.com/iu-parfunc/packages/
	git config --local url."https://github.com/ghc/packages-".insteadOf   https://github.com/iu-parfunc/packages/
	git config --local url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/iu-parfunc/packages/
	git config --local url."git@github.com:/ghc/packages-".insteadOf      git@github.com:/iu-parfunc/packages/
	git submodule update --init --recursive

Main: Main.hs
	$(GHC) --make -Wall Main.hs

clean:
	rm -f Main Main.hi Main.o

.PHONY: clean
