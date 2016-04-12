
# This should track the cnf/mutable branch.
# It is hardcoded for reproducibility.
# We could use a script to update it in lieu of "git submodule" commands.
MUTABLECNF_VER = 0.3
# SUBMOD_SHA = 61a27d33d48d566e20b026357ffbfe04edeca30b
# RRN: This appears to be the 0.2 release, 2/26/2016:
# SUBMOD_SHA = 14302c4be0f6a2fc1e66a8db59a224dfef1bb723
# 0.3 release, 4/11/2016:
SUBMOD_SHA = 8c17982648434fae28dedb102ab4624a7323476b

# Alternatively, there may be some way that we can go back to using
# a real submodule, but still add an extra remote and use it for recursive
# cloning....

PREFIX ?= $(HOME)/opt/ghc-mutable-cnf-$(MUTABLECNF_VER)
GHC ?= $(PREFIX)/bin/ghc
CABAL ?= cabal
STACK ?= stack
JOBS ?=
SRC = $(wildcard *.hs)

.PHONY: all docker clean pull exe ghc test

all: exe

docker:
	time docker build --build-arg JOBS=$(JOBS) -t cnf-mutable-tests .

# With the conditional, this should be idempotent:
ghc:
	if ! [ -d ./ghc ]; then git clone --quiet --recursive git://git.haskell.org/ghc.git; \
           (cd ghc && git remote add fork https://github.com/iu-parfunc/ghc.git); fi
	$(MAKE) pull

pull:
	@echo "Pulling latest GHC changes and checking out the designated revision."
	(cd ghc && git fetch fork)
	(cd ghc && git checkout $(SUBMOD_SHA))
	(cd ghc && git reset --hard && git clean -dfx)
	(cd ghc && git submodule update --quiet --init --recursive)

$(GHC): ghc
	@echo ""
	@echo "Now building GHC."
	sed -e 's/#BuildFlavour = quick/BuildFlavour = quick/' \
	    -e 's/#V=0/V=0/' ghc/mk/build.mk.sample \
	    > ghc/mk/build.mk
	(cd ghc && ./boot && ./configure --quiet --prefix $(PREFIX) && make -j $(JOBS))
	(cd ghc && make install)

exe: $(GHC) $(SRC)
	$(CABAL) install -w $(GHC) --only-dep
	$(CABAL) configure -w $(GHC) --enable-tests
	$(CABAL) build

test:
	$(STACK) test

clean:
	$(CABAL) clean

# A handy way to bring the "submodule" up to date.
submod_latest: ghc
	(cd ghc && git fetch --all && git checkout cnf/mutable)
	$(eval NEWSHA := $(shell cd ghc && git rev-parse HEAD))
	sed -i "/^SUBMOD_SHA =/c\SUBMOD_SHA = $(NEWSHA)" Makefile
