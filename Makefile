default: all
all: bin/HangedMan bin/Nim

bin/HangedMan: HangedMan.hs
	ghc -o bin/HangedMan HangedMan.hs
bin/Nim: Nim.hs
	ghc -o bin/Nim Nim.hs

.PHONY: all default
