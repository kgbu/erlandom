%%%
%%% twitter user query
%%%

%%% SYNOPSIS
%%
%%	tw_q:get_id(["user1", "user2", "user3"]).
%%

-module(tw_q).
-export([get_id/1, user/1]).

%%% TODO
%%% cache user data on DETS (persistent storage)

get_singleid(Name) ->
	{ok, {struct, S}} = user(Name),
	[Id || {id, Id} <- S ].

get_id(NameList) ->
	get_id(NameList,[]).

get_id([], Store) ->
	Store;
get_id([ H | T], Store) ->
	get_id(T, [get_singleid(H)|Store]).

user(Name) ->
        {ok, Socket} = gen_tcp:connect("twitter.com", 80, [binary, {packet, 0}]),
        ReqStr = "GET /users/show/" ++ Name ++ ".json HTTP/1.1"
		 ++ "\r\nhost: twitter.com\r\n\r\n",
        ok = gen_tcp:send(Socket, ReqStr),
        Lines = string:tokens(binary_to_list(receive_data(Socket,[])), "\r\n"),
	parse(Lines).

receive_data(Socket, Store) ->
        receive
		{tcp, Socket, Bin} ->
			receive_data(Socket, [Bin | Store]);
                {tcp_closed, Socket} ->
			list_to_binary(lists:reverse(Store))
	end.

parse([]) ->
	[];
parse([ [${ | H]| _T ]) ->
	json:decode_string([ ${ | H]);
parse([ _ | T ]) ->
	parse(T).

	
