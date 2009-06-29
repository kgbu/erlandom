%%% FIXME !!
%%%
%%%

-module(rt).
-export([sign_in/2, spritzer/2, follow/1, track/1]).

%%% This module shows how to handle Twitter Streaming API from Erlang.
%%%
%%% References
%%% - http://apiwiki.twitter.com/Streaming-API-Documentation

%%% Requirements
%%% json library from YAWS(http://yaws.hyber.org/)
%%%                or LShift(http://hg.opensource.lshift.net/erlang-rfc4627)
%%%   this version uses rfc4627.erl from LShift

-define(TTL, 10000).

%%%
%%% Methods (public only)
%%%

%%
%%  Random-sampled real-time tweet stream
%%
%% query parameters : delimited 

spritzer(User, PW, Delimited) ->
	connect_get("/spritzer.json").

%%
%%  Follow specific users streams
%%
%% query prameters : follow

follow(FollowList) ->
	connect_post("/follow.json", FollowList).

%%
%%  Track keywords
%%
%% query prameters : track

track(Followee) ->
	connect_post("/track.json", TrackItemList).

sign_in(User, PW) ->
	Auth = auth_hdr(User, PW),
	Authheader = "Authorization: Basic " ++ Auth,
	put(auth, Auth).

connect_get(Path, Count) ->
	Auth = get(auth),
	Authheader = "Authorization: Basic " ++ Auth,
	{ok, Socket} = gen_tcp:connect("stream.twitter.com", 80, [binary,
		 {packet, line}, {recbuf, 65536}]),
	ReqStr = "GET " ++ Path ++ " HTTP/1.0\r\n" ++ Authheader ++ "\r\n\r\n",
	ok = gen_tcp:send(Socket, ReqStr),
	receive_data(Socket, Count + 1).

connect_post(Path, Count, ParamList) ->
	Auth = get(auth),
	Authheader = "Authorization: Basic " ++ Auth,
	{ok, Socket} = gen_tcp:connect("stream.twitter.com", 80, [binary,
		 {packet, line}, {recbuf, 65536}]),
	ReqStr = "POST " ++ Path ++ " HTTP/1.0\r\n" ++ Authheader ++ "\r\n\r\n"
		++ ParamStr,
	ok = gen_tcp:send(Socket, ReqStr),
	receive_data(Socket, Count + 1).

receive_data(Socket, N) ->
	receive
               {tcp, Socket, <<"\r\n">> } ->
               		receive_json(Socket, [], N);
			Pid = spawn(fun(TmpJson) -> receive_json(Socket, [], N) end);
               {tcp, Socket, _} ->
               		receive_data(Socket, N);
               {tcp_closed, Socket} ->
                	N
	end.

receive_json(_, Store, 1) ->
	lists:reverse(Store);
receive_json(Socket, Store, N) ->
	receive
               {tcp, Socket, Bin} ->
			{ok,TmpJson} = rfc4627:decode(Bin),
			Pid = spawn(fun(TmpJson) -> processloop(TmpJson, ?TTL) end),
			io:format("~p\n", [Pid]),
               		receive_json(Socket, [Store | TmpJson], N - 1);
               {tcp_closed, Socket} ->
                	exit
	end.

%%%
%%% Utilities
%%%

auth_hdr(nil, nil) -> [];
auth_hdr(U, P) when is_binary(U) ->
	auth_hdr(binary_to_list(U), P);
auth_hdr(U, P) when is_binary(P) ->
        auth_hdr(U, binary_to_list(P));
auth_hdr(U, P) ->
        binary_to_list(base64:encode(U ++ ":" ++ P)).

