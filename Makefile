PREFIX ?= $(HOME)/opt/ghc-cnf-mutable
GHC ?= $(PREFIX)/bin/ghc
CABAL ?= cabal
JOBS ?=
SRC = $(wildcard *.hs)

# This should track the cnf/mutable branch.
# It is hardcoded for reproducibility.
# We could use a script to update it in lieu of "git submodule" commands.
SUBMOD_SHA = 61a27d33d48d566e20b026357ffbfe04edeca30b
# Alternatively, there may be some way that we can go back to using
# a real submodule, but still add an extra remote and use it for recursive
# cloning....

.PHONY: all docker clean pull exe

all: exe

docker:
	time docker build -t cnf-mutable-tests .

ghc:
	git clone --quiet --recursive git://git.haskell.org/ghc.git
	(cd ghc && git remote add fork https://github.com/iu-parfunc/ghc.git)
	$(MAKE) pull

pull:
	(cd ghc && git fetch fork)
	(cd ghc && git checkout $(SUBMOD_SHA))
	(cd ghc && git reset --hard && git clean -dfx)
	(cd ghc && git submodule update --quiet --init --recursive)

$(GHC): ghc
	sed -e 's/#BuildFlavour = quick/BuildFlavour = quick/' \
	    -e 's/#V=0/V=0/' ghc/mk/build.mk.sample \
	    > ghc/mk/build.mk
	(cd ghc && ./boot && ./configure --quiet --prefix $(PREFIX) && make -j $(JOBS))
	(cd ghc && make install)

exe: $(GHC) $(SRC)
	$(CABAL) install -w $(GHC) --only-dep
	$(CABAL) configure -w $(GHC) --enable-tests
	$(CABAL) build

clean:
	$(CABAL) clean

# A handy way to bring the "submodule" up to date.
submod_latest: ghc
	(cd ghc && git fetch --all && git checkout cnf/mutable)
	$(eval NEWSHA := $(shell cd ghc && git rev-parse HEAD))
	sed -i "/^SUBMOD_SHA =/c\SUBMOD_SHA = $(NEWSHA)" Makefile
