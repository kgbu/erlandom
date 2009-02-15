-module(port8080).
-export([new/0]).

new() ->
    {ok, Listen} = gen_tcp:listen(8080, [binary, {packet, 0},
					{reuseaddr, true},
					{active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).


loop(Socket) ->
    receive
	{tcp, Socket, Bin} ->
	    io:format("Server received binary = ~p~n", [Bin]),
	    Str = binary_to_list(Bin),
	    io:format("Server unpacked ~p~n", [Str]),
	    gen_tcp:send(Socket, term_to_binary({"hohoho"})),
	    loop(Socket);
	{tcp_closed, Socket} ->
	    io:format("Server socket closed~n")
    end.
