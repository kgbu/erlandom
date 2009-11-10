-module(cb_SUITE).
-compile(export_all).
-include("ct.hrl").

all() -> [badnotice, goodnotice, response_to_verification].

init_per_suite(Config) ->
    crypto:start(),
    inets:start(),
    uuid:start(),
    webhook:start(),
    case whereis(pshb_subscriber) of
		undefined ->	
			ct:print("start pshb_subscriber", []),
			pshb_subscriber:start()
			;
		_ -> ok
    end,
    case whereis(content_mgr) of
		undefined ->
			ct:print("start content_mgr", []),
			content_mgr:start()
			;
		_ -> ok
    end,
    Config
    .

end_per_suite(_Config) ->
    inets:stop(),
    ok
    .

suite() ->
    [{timetrap, {minutes, 1}}]
    .

badnotice() ->
    [{userdata,[{doc,"bad notice args, return false."}]}]
    .

badnotice(_Config) ->
    false = cb:notice(self(), "dfa", "dfsa")
    .

goodnotice() ->
    [{userdata,[{doc,"well-formed notice args, return false."}]}]
    .

goodnotice(_Config) ->
    Res = cb:notice(self(), [{request_method, "get"},{query_string, "hi=a"}], "ddfsa"),
    ct:print("~p~n", [Res]),
    ok
    .

response_to_verification() ->
    [{userdata,[{doc,"verification failure, return false."}]}]
    .

response_to_verification(_Config) ->
    ct:print("~p~n", [cb:response_verification([{request_method, "get"},{query_sstring, "hi=a"}], "dfsa")]),
    ok
    .

