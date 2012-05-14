
tractor-example::
	rm -f Main.o
# 	ghc --make Main.hs -o tractor-example -threaded
	ghc --make Main.hs -o tractor-example -threaded -i..:../dist/build/autogen/
	export tractor_datadir=.. ; ./tractor-example

tictactoe::
	rm -f TicTacToe.o
	ghc --make TicTacToe.hs -o tictactoe -threaded -i..:../dist/build/autogen/
	export tractor_datadir=.. ; ./tictactoe
