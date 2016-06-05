all:
	ghc Main.hs
	gcc secd.c -o secd -g

run:
	./Main $(ARG) ex.secd
	./secd < ex.secd

test:
	valgrind ./secd < ex.secd

inter:
	./Main $(ARG) ex.secd 1
