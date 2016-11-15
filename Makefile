ai:
	dist/build/poet/poet ai -i benchmarks/basic/ai/$(TEST).c 
	dot -Tpdf benchmarks/basic/ai/$(TEST).dot > benchmarks/basic/ai/$(TEST).pdf 

poetst:
	poet stid -i=/home/msousa/steroid/input.ll -s=1

poet:
	poet stid -i=/home/msousa/steroid/input.ll

all:
	cabal install
	poet stid -i=/home/msousa/steroid/input.ll

clean:
	rm *.{hi,o,hp,prof}
	rm Main
