
tractor-example::
	rm -f Main.o
# 	ghc --make Main.hs -o tractor-example -threaded
	ghc --make Main.hs -o tractor-example -threaded -i..:../dist/build/autogen/
	export tractor_datadir=.. ; ./tractor-example
