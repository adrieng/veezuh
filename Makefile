.PHONY: all clean uninstall install run runbig runall

all:
	jbuilder build

clean:
	rm -rf _build

install: all
	jbuilder install

uninstall: all
	jbuilder uninstall

run: all
	./_build/default/src/veezuh.exe ./datasets/fib.32.4proc.std.sqlite

runbig: all
	./_build/default/src/veezuh.exe ./datasets/fib.32.4proc.hugemem.sqlite

runall: all
	./_build/default/src/veezuh.exe ./datasets/*.sqlite
