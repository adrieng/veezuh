BIN=./_build/default/src/veezuh.exe

.PHONY: all clean uninstall install

all:
	jbuilder build

clean:
	rm -rf _build

install: all
	jbuilder install

uninstall: all
	jbuilder uninstall

.PHONY: prepare run stats runbig statsbig runall prepare

DSETDIR?=~/tmp/datasets
DSET_SMALL?=$(DSETDIR)/fib.hierarchical-heap_4proc_32.sqlite
DSET_BIG?=$(DSETDIR)/fib.hierarchical-heap_8proc_42.sqlite

run: all
	$(BIN) $(DSET_SMALL)

stats: all
	$(BIN) -stats $(DSET_SMALL)

runbig: all
	$(BIN) $(DSET_BIG)

statsbig: all
	$(BIN) -stats $(DSET_BIG)

runall: all
	$(BIN) $(DSETDIR)/*.sqlite

prepare: all
	parallel --will-cite $(BIN) -reprep ::: $(DSETDIR)/*.sqlite
