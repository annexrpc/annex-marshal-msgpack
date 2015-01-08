PROJECT = annex_marshal_msgpack

# dependencies

DEPS = msgpack

dep_msgpack = git https://github.com/msgpack/msgpack-erlang.git 69d5aa8ff5f100ec99c6f31662c05baa3f7276f8

include erlang.mk

repl: all bin/start
	@bin/start rl make

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
