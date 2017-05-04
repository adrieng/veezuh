.PHONY: all clean uninstall install run run_all

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

run_all: all
	./_build/default/src/veezuh.exe ./datasets/*.sqlite
