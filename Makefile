all:
	gcc -O2 -c ./lib/runtime.c -o ./lib/runtime.o
	stack build --copy-bins

clean:
	stack clean
	rm -f ./lib/runtime.o
	rm -f latc_x86_64
	rm -f espi