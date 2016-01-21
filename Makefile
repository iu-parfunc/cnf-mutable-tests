GHC ?= ghc-stage2

all: Main

Main: Main.hs
	$(GHC) --make -Wall Main.hs

clean:
	rm -f Main Main.hi Main.o

.PHONY: clean
