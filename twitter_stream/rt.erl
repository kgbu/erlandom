%%% FIXME !!
%%%
%%%
-module(rt).
-export([sign_in/2,
		sample/0, sample/1, sample_stream/0, sample_stream/1,
		follow/1,
		track/1
		]).

%%% This is sample module to show how to handle Twitter Streaming API 
%%%
%%% References
%%% - http://apiwiki.twitter.com/Streaming-API-Documentation

%%% Requirements
%%% json library from YAWS(http://yaws.hyber.org/)
%%%                or LShift(http://hg.opensource.lshift.net/erlang-rfc4627)
%%%   this version uses rfc4627.erl from LShift

-define(TTL, 10000).
-define(DEFAULTNUMBEROFENTRIES, 10).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%

sign_in(User, PW) ->
	Auth = auth_hdr(User, PW),
	Authheader = "Authorization: Basic " ++ Auth,
	put(authheader, Authheader),
	ok
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%  Random-sampled real-time tweet stream
%%
%% query parameters : count, delimited 

sample() ->
	Delimited = "2",
	sample(Delimited)
	.

sample(Delimited) ->
	{ok, Socket} = connect_get("/1/statuses/sample.json?" ++
				"delimited=" ++ Delimited),
	receive_stream(Socket, ?DEFAULTNUMBEROFENTRIES)
	.

sample_stream() ->
	sample_stream(fun(X) -> sample_callback(X) end)
	.

sample_stream(CallbackFunc) ->
	{ok, Socket} = connect_stream("/1/statuses/sample.json?delimited=1"),
	feed_stream(Socket, CallbackFunc)
	.

%%
%%  Follow specific users streams
%%
%% query prameters : follow

follow(FollowList) ->
	{ok, Socket} = connect_post("/1/statuses/filter.json", [{"follow", FollowList}]),
	receive_data(Socket)
	.

%%
%%  Track keywords
%%
%% query prameters : track

track(TrackList) ->
	{ok, Socket} = connect_post("/1/statuses/filter.json", [{"track", TrackList}]),
	receive_data(Socket)
	.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%
connect_get(Path) ->
	Authheader = get(authheader),
	{ok, Socket} = gen_tcp:connect("stream.twitter.com", 80, [binary,
		 {packet, line}, {recbuf, 65536}]),
	ReqStr = "GET " ++ Path ++ " HTTP/1.0\r\n" ++ Authheader ++ "\r\n\r\n",
	error_logger:error_msg("~p~n", [ReqStr]),
	ok = gen_tcp:send(Socket, ReqStr),
	{ok, Socket}
	.

connect_stream(Path) ->
	Authheader = get(authheader),
	{ok, Socket} = gen_tcp:connect("stream.twitter.com", 80, [binary,
		 {packet, line}, {recbuf, 65536}]),
	ReqStr = "GET " ++ Path ++ " HTTP/1.0\r\n" ++ Authheader ++ "\r\n\r\n",
	error_logger:error_msg("~p~n", [ReqStr]),
	ok = gen_tcp:send(Socket, ReqStr),
	{ok, Socket}
	.

connect_post(Path, ParamList) ->
	Authheader = get(authheader),
	{ok, Socket} = gen_tcp:connect("stream.twitter.com", 80, [binary,
		 {packet, line}, {recbuf, 65536}]),
	ReqStr = "POST " ++ Path ++ " HTTP/1.0\r\n" ++ Authheader ++ "\r\n\r\n"
		++ param_to_body(ParamList),
	ok = gen_tcp:send(Socket, ReqStr),
	{ok, Socket}
	.

%
%	Note : just before receive data, Socket must be closed normally.
%
receive_data(Socket) ->
	receive
    	{tcp, Socket, <<"\r\n">> } ->
        	receive_json(Socket, []);
        {tcp, Socket, _A} ->
            receive_data(Socket);
        {tcp_closed, Socket} ->
			error_logger:error_msg("unexpected close"),
			gen_tcp:close(Socket),
			exit(unexpected_close)
	end
	.

receive_stream(Socket, N) ->
	receive
    	{tcp, Socket, <<"\r\n">> } ->
        	receive_json_stream(Socket, [], N);
        {tcp, Socket, _A} ->
            receive_stream(Socket, N);
        {tcp_closed, Socket} ->
			error_logger:error_msg("unexpected close"),
			gen_tcp:close(Socket),
			exit(unexpected_close)
	end
	.

feed_stream(Socket, CallbackFunc) ->
	receive
    	{tcp, Socket, <<"\r\n">> } ->
        	feed_json(Socket, CallbackFunc);
        {tcp, Socket, _A} ->
            feed_stream(Socket, CallbackFunc);
        {tcp_closed, Socket} ->
			error_logger:error_msg("unexpected close"),
			gen_tcp:close(Socket),
        	exit(unexpected_close)
	end
	.

%%%%%%%%%%%%% json handling

receive_json(Socket, Store) ->
	receive
		{tcp, Socket, Bin} ->
			case rfc4627:decode(Bin) of
				{ok,TmpJson, _} ->
            		receive_json(Socket, [Store | TmpJson]);
				_Any ->
					error_logger:error_msg("not valid JSON~n~p~n",[_Any]),
					gen_tcp:close(Socket),
					exit(normal)
			end;
		{tcp_closed, Socket} ->
            lists:reverse(Store);
		_Any ->
			error_logger:error_msg("other than normal term~n~p~n",[_Any]),
			gen_tcp:close(Socket),
			exit(normal)
	end
	.

receive_json_stream(Socket, Store, 0) ->
	gen_tcp:close(Socket),
	lists:reverse(Store)
	;
receive_json_stream(Socket, Store, N) ->
	receive
		{tcp, Socket, Bin} ->
			case rfc4627:decode(Bin) of
				{ok,TmpJson, _} ->
            		receive_json_stream(Socket, [Store | TmpJson], N - 1);
				_Any ->
					error_logger:error_msg("not valid JSON~n~p~n",[_Any]),
					gen_tcp:close(Socket),
					exit(not_valid_json)
			end;
		{tcp_closed, Socket} ->
			error_logger:error_msg("unexpected close"),
			exit(unexpected_close);
		_Any ->
			error_logger:error_msg("other than normal term~n~p~n",[_Any]),
			gen_tcp:close(Socket),
			exit(invalid_data)
	end
	.

feed_json(Socket, Callback) ->
	receive
		{tcp, Socket, Bin} ->
			case rfc4627:decode(Bin) of
				{ok,TmpJson, _} ->
					Callback(TmpJson),
            		feed_json(Socket, Callback);
				_Any ->
					error_logger:error_msg("not valid JSON term~n~p~n",[_Any]),
					gen_tcp:close(Socket),
					exit(invalid_json_term)
			end;
		{tcp_closed, Socket} ->
			exit(unexpoected_close);
		_Any ->
			error_logger:error_mgs("other than normal term~n~p~n",[_Any]),
			gen_tcp:close(Socket),
			exit(unexpected_data)
	end
	.

%%%
%%% Utilities
%%%

auth_hdr(nil, nil) -> [];
auth_hdr(U, P) when is_binary(U) ->
	auth_hdr(binary_to_list(U), P);
auth_hdr(U, P) when is_binary(P) ->
    auth_hdr(U, binary_to_list(P));
auth_hdr(U, P) ->
    binary_to_list(base64:encode(U ++ ":" ++ P))
	.

param_to_body(PreParam) ->
    ParamList = [ {X, Y} || {X, Y} <- PreParam, Y =/= undefined],
    PreBody = lists:flatten(lists:map(fun({A,B}) -> [A, "=", B, "&"] end, ParamList)),
    string:sub_string(PreBody, 1, length(PreBody) - 1)
	.


sample_callback(Str) ->
	io:format("~p~n",[Str])
	.
