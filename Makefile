# GHC ?= ghc-stage2
# Stage1 should be sufficient for us:
GHC ?= ghc/inplace/bin/ghc-stage1

.PHONY: all docker clean

all: Main

docker:
	time docker build -t cnf-mutable-tests .

gitconfig:
	git config --global url."git://github.com/ghc/packages-".insteadOf     git://github.com/ghc/packages/
	git config --global url."http://github.com/ghc/packages-".insteadOf    http://github.com/ghc/packages/
	git config --global url."https://github.com/ghc/packages-".insteadOf   https://github.com/ghc/packages/
	git config --global url."ssh://git@github.com/ghc/packages-".insteadOf ssh://git@github.com/ghc/packages/
	git config --global url."git@github.com:/ghc/packages-".insteadOf      git@github.com:/ghc/packages/

submod: gitconfig
	git clone --recursive git://git.haskell.org/ghc.git
	(cd ghc && git remote add fork https://github.com/iu-parfunc/ghc.git)
	(cd ghc && git fetch fork)
	(cd ghc && git checkout cnf/mutable)
	(cd ghc && git reset --hard && git clean -dfx)
	(cd ghc && git submodule update --init --recursive)

ghc: submod
	sed 's/#BuildFlavour = devel1/BuildFlavour = devel1/' ghc/mk/build.mk.sample > ghc/mk/build.mk
	(cd ghc && ./boot && ./configure --prefix $(HOME)/opt/ghc-cnf-mutable && make -j)

Main: ghc Main.hs
	$(GHC) --make -Wall Main.hs

clean:
	rm -f Main Main.hi Main.o
