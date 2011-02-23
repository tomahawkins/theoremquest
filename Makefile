.PHONY: all
all:
	cd theoremquest        && cabal install
	cd theoremquest-server && cabal install
	cd theoremquest-client && cabal install

.PHONY: clean
clean:
	cd theoremquest        && cabal clean
	cd theoremquest-server && cabal clean
	cd theoremquest-client && cabal clean

