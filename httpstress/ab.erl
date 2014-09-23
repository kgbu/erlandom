-module(ab).

-export([start/0,start/1,start/2]).

%% for spawn
-export([req/2]).


start() ->
	start(1000, "http://192.168.1.127/").

start(N) ->
	start(N, "http://192.168.1.127/").

start(0, Url) ->
        io:format("~p end.~n",[Url]),
	ok;
start(N, Url) ->
	io:format("~p started~n", [N]),
	spawn (ab, req, [N, Url]),
	start (N - 1, Url).

req(N, Url) ->
	httpc:request(Url),
	io:format("~p end~n",[N]).
