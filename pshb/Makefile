#	Makefile for erlang
.SUFFIXES:	.erl	.beam	.yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = pshb pshb_utils pshb_publisher pshb_subscriber \
	couchdb webhook socnode content_mgr cb

all:	compile

compile:	${MODS:%=%.beam}

# subdirs:
#	cd dir1: make
#	cd dir2: make

clean:
	rm -rf *.beam erl_crash.dump
#	cd dir1: make clean
#	cd dir2: make clean

