#	Makefile for erlang
.SUFFIXES:	.erl	.beam	.yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = pshb pshb_utils pshb_publisher pshb_subscriber \
	couchdb webhook socnode content_mgr cb \
	salmon \
	uuid mochijson mochijson2

all:	compile

ct:
	/usr/local/erlang/test_tools/common_test-1.4.5/priv/bin/run_test -dir .

start:
	erl -boot start_sasl -config debug

production:
	erl -boot start_sasl -config production

compile:	${MODS:%=%.beam}

# subdirs:
#	cd dir1: make
#	cd dir2: make

clean:
	rm -rf *.beam erl_crash.dump
#	cd dir1: make clean
#	cd dir2: make clean

clean_test:
	rm -rf ct_run.ct@www*
