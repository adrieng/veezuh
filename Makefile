.PHONY: all clean install run

all:
	jbuilder build

clean:
	rm -rf _build

install: all
	jbuilder install

run: all
	./_build/default/src/veezuh.exe
