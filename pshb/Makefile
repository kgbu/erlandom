#	Sample Makefile
#
.SUFFIXES:	.erl	.beam	.yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

MODS = pshb pshb_utils pshb_publisher pshb_subscriber \
	webhook socnode content_mgr cb couchdb \
	salmon \
	erlang_couchdb \
	uuid mochijson mochijson2

all:	compile

compile:	${MODS:%=%.beam}

#	execute common test suites
#
ct:	compile
	cp *.beam ./test
	/usr/local/lib/erlang/lib//common_test-1.4.5/priv/bin/run_test -dir . -logdir ./test_logs

#	start Erlang interpreter in various environments
#
start:
	erl -boot start_sasl -config debug

production:
	erl -boot start_sasl -config production


# subdirs:
#	cd dir1: make
#	cd dir2: make

clean_all:	clean clean_test
clean:
	rm -rf *.beam erl_crash.dump
#	cd dir1: make clean
#	cd dir2: make clean

#	clean up common test results(directories)
clean_test:
	rm -rf ./test_logs/ct_run.ct@www*
