.PHONY: all clean uninstall install run

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
