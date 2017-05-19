BIN=./_build/default/src/veezuh.exe

.PHONY: all clean uninstall install prepare run runbig runall stats

all:
	jbuilder build

clean:
	rm -rf _build

install: all
	jbuilder install

uninstall: all
	jbuilder uninstall

prepare: all
	parallel --will-cite $(BIN) -reprep ::: datasets/*.sqlite

run: all
	$(BIN) ~/tmp/datasets/fib.32.4proc.hh.sqlite

runbig: all
	$(BIN) ~/tmp/datasets/fib.42.4proc.spoon.sqlite

runall: all
	$(BIN) ~/tmp/datasets/*.sqlite

stats: all
	$(BIN) -stats ~/tmp/datasets/fib.32.4proc.std.sqlite
