# GHC ?= ghc-stage2
# Stage1 should be sufficient for us:
GHC ?= ghc/inplace/bin/ghc-stage1
JOBS ?=

# This should track the cnf/mutable branch.
# It is hardcoded for reproducibility.
# We could use a script to update it in lieu of "git submodule" commands.
SUBMOD_SHA = dc163976391fe22de211c72f0bdd21e1cafd747b
# Alternatively, there may be some way that we can go back to using 
# a real submodule, but still add an extra remote and use it for recursive
# cloning....

.PHONY: all docker clean submod pull build_ghc

all: Main

docker:
	time docker build -t cnf-mutable-tests .

submod: ghc
ghc: 
	git clone --quiet --recursive git://git.haskell.org/ghc.git
	(cd ghc && git remote add fork https://github.com/iu-parfunc/ghc.git)
	$(MAKE) pull
pull:
	(cd ghc && git fetch fork)
	(cd ghc && git checkout $(SUBMOD_SHA))
	(cd ghc && git reset --hard && git clean -dfx)
	(cd ghc && git submodule update --quiet --init --recursive)

build_ghc: ghc
	sed -e 's/#BuildFlavour = devel1/BuildFlavour = devel1/' \
	    -e 's/#V=0/V=0/' ghc/mk/build.mk.sample \
	    > ghc/mk/build.mk
	(cd ghc && ./boot && ./configure --quiet --prefix $(HOME)/opt/ghc-cnf-mutable && make -j $(JOBS))

Main: build_ghc Main.hs
	$(GHC) --make -Wall Main.hs

clean:
	rm -f Main Main.hi Main.o


# A handy way to bring the "submodule" up to date.
# FINISHME: Need to grab the latest SHA on the branch:
submod_latest: submod
	(cd ghc && git fetch --all && git checkout cnf/mutable)
	sed -i '/SUBMOD_SHA =/c\SUBMOD_SHA = NEWSHA' Makefile

