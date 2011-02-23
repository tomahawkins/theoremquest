.PHONY: all
all: tq tqd lib

.PHONY: lib
lib:
	cd theoremquest        && cabal install
	
.PHONY: tq
tq:
	cd theoremquest-client && cabal install
	
.PHONY: tqd
tqd:
	cd theoremquest-server && cabal install

.PHONY: clean
clean:
	cd theoremquest        && cabal clean
	cd theoremquest-server && cabal clean
	cd theoremquest-client && cabal clean

