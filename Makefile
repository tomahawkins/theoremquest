.PHONY: all
all: lib tq tqd

.PHONY: lib
lib:
	cd theoremquest        && cabal install
	
.PHONY: tq
tq:
	cd theoremquest-client && cabal install
	
.PHONY: tqd
tqd:
	cd theoremquest-server && cabal install

.PHONY: upload
upload:
	scp ~/.cabal/bin/tqd   hawkit1@tomahawkins.org:.
	scp ~/.cabal/bin/tqcgi hawkit1@tomahawkins.org:cgi-bin

.PHONY: clean
clean:
	cd theoremquest        && cabal clean
	cd theoremquest-server && cabal clean
	cd theoremquest-client && cabal clean

