%%
%%

-module(manager).
-export([start/1]).

start([_Host, RawLogdir, RawTimeout, MaxClients, Interval, KeepAlive])->
	Logdir = atom_to_list(RawLogdir),
	Timeout = list_to_integer(atom_to_list(RawTimeout)),
	MC = list_to_integer(atom_to_list(MaxClients)),
	Intv = list_to_integer(atom_to_list(Interval)),
	KA = list_to_integer(atom_to_list(KeepAlive)),

	{ok, Loglist}=file:list_dir(Logdir),

	attach_reader(Logdir, Loglist),
	Requestqueue = wait_reader([], Loglist),

	attach_clients(MC, Intv, KA),

	dispatchloop(Timeout, Requestqueue).

%%
%% private functions
%%

attach_reader(_Logdir, [])->0;
attach_reader(Logdir, [Hostname|L])->
	Myself = self(),
	spawn(fun() -> logreader:new(Myself, Logdir ++ "/" ++ Hostname, Hostname) end),
	attach_reader(Logdir, L).

wait_reader(Requests, [])-> Requests;
wait_reader(Requests, [_H|List])->
	receive
		{done, Req} -> 
			wait_reader(Req ++ Requests, List);
		_Other -> 
			wait_reader(Requests, [_H|List])
	after 5000 ->
		io:format("wait_reader timeout",[])
	end.

attach_clients(0, _, _)->0;
attach_clients(N, Interval, KeepAlive)->
	Myself = self(),
	spawn(fun() -> client:new(Myself, Interval, KeepAlive) end),
	attach_clients(N - 1, Interval, KeepAlive).

dispatchloop(_Timeout, [])-> done;
dispatchloop(Timeout, [ReqTerm|Rest])->
	{Req} = ReqTerm,
	receive
		{Pid, ready} -> 
			%% io:format("request for ~p: ~p~n", [Pid, Req]),
			Pid ! {self(), Req},
			dispatchloop(Timeout, Rest)
	after Timeout ->
		io:format("may be complete", []),
		{done}
	end.
