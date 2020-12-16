all:
	stack build --copy-bins

clean:
	stack clean
	rm -f latc