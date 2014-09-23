-module(ab).

-export([start/0,start/1,start/2,start/3]).

%% for spawn
-export([req/3]).

-define(DEFAULTURL, "http://192.168.1.127").

start() ->
	start(1000, ?DEFAULTURL, 1).

start(N) ->
	start(N, ?DEFAULTURL, 1).

start(N, Url) ->
	start(N, Url, 1).

start(0, Url, _) ->
        io:format("~p end.~n",[Url]),
	ok;
start(N, Url, Count) ->
	spawn (ab, req, [N, Url, Count]),
	io:format("~p/~p started~n", [N, Count]),
	start (N - 1, Url, Count).

req(N, Url, 0) ->
	httpc:request(Url),
	io:format("~p totally end~n",[N]);
req(N, Url, Count) ->
	httpc:request(Url),
	io:format("~p/~p end~n",[N, Count]),
	req(N, Url, Count - 1).
