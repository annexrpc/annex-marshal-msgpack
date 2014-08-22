PROJECT = annex_marshal_msgpack

# dependencies

DEPS = msgpack

dep_msgpack = git https://github.com/msgpack/msgpack-erlang.git 7cc8266afd06639d24a33e1ed60234983aab6443

include erlang.mk

repl: all bin/start
	@bin/start rl make

bin/start:
	@mkdir -p bin
	@curl https://gist.githubusercontent.com/camshaft/372cc332241ac95ae335/raw/start -o $@
	@chmod a+x $@

.PHONY: repl
