
sunroof-example: Main.hs
	ghc --make Main.hs -o sunroof-example -threaded \
             -fforce-recomp -O2 -dcore-lint 

run-sunroof-example: sunroof-example
	./sunroof-example

# This version can be used before cabal install
#
# You need to have done cabal build before this will find the 
# ../dist/build/autogen/Paths_kansas_comet.hs
sunroof-example-inplace: Main.hs # ../Web/KansasComet.hs 
	ghc --make Main.hs -o sunroof-example-inplace -threaded \
		-fforce-recomp -O2 -dcore-lint \
		-i..:../dist/build/autogen/

run-sunroof-example-inplace: sunroof-example-inplace
	export sunroof_datadir=.. ; ./sunroof-example-inplace

ghci:
	ghci Main.hs \
		-i..:../dist/build/autogen/
