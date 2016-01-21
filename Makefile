# GHC ?= ghc-stage2
# Stage1 should be sufficient for us:
GHC ?= ghc/inplace/bin/ghc-stage1

.PHONY: all ghc

all: Main

docker:
	time docker build -t cnf-mutable-tests .

submod:
	git clone --recursive git://git.haskell.org/ghc.git
#	(cd ghc && git remote add fork git@github.com:iu-parfunc/ghc.git)
	(cd ghc && git remote add fork https://github.com/iu-parfunc/ghc.git)
	(cd ghc && git fetch fork)
	(cd ghc && git checkout 9df22e29ddb74aca893925296e2606f3b962374b)
	(cd ghc && git submodule update)


ghc:
	sed 's/#BuildFlavour = devel1/BuildFlavour = devel1/' ghc/mk/build.mk.sample > ghc/mk/build.mk
	(cd ghc && ./boot && ./configure --prefix $(HOME)/opt/ghc-cnf-mutable && make -j)

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
