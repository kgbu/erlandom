#	Sample Makefile
#
.SUFFIXES:	.erl	.beam	.yrl

.erl.beam:
	erlc -W +debug_info $<

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
#	FIXME: -dir option shall be "./test" (default handling by run_test)
#	FIXME: path setting shall be fixed to remove BEAM copy
ct:	compile
	cp *.beam ./test
	cp *.hrl ./test
	cp saved_db ./test
	/usr/local/lib/erlang/lib//common_test-1.4.5/priv/bin/run_test -dir . -logdir ./webhook/htdocs/couchtest -cover ./config/pshb.coverspec

#	start Erlang interpreter in various environments
#
start:
	erl -boot start_sasl -config debug -s pshb start

production:
	erl -boot start_sasl -config production


#	clean ups
#
clean_all:	clean clean_test
clean:
	rm -rf *.beam erl_crash.dump

#	clean up common test results(directories)
clean_test:
	rm -rf ./webhook/htdocs/couchtest/ct_run.ct@www*
