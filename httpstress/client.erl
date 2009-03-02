-module(client).
-compile(export_all).

new(ManagerPid, Interval, KeepAlive) ->
	spawn(fun() -> init(ManagerPid, Interval, KeepAlive) end).

init(ManagerPid, Interval, KeepAlive) ->
	inets:start(),
	
	%% set keepalive (pesistent connection)
	%%
	http:set_options([
		{max_sessions, 100},
		{pipeline_timeout, KeepAlive} 
	]),
	loop(ManagerPid, Interval).

%%
%%
loop(ManagerPid, Interval) ->
	ManagerPid ! {self(), ready},
	try
		receive
			{_Pid, Request} -> 
				send_req(Request);
			_ -> donothing
		end
	catch
		Type:Why -> io:format("~p : ~p~n", [Type, Why]) 
	end,
	%% sleep for Interval msec
	receive
	after Interval ->
		true
	end,
	loop(ManagerPid, Interval).

%%
%%
send_req(Request) ->
	try
		{ok, {{_Version, _RetCode, _ReasonPhrase}, _Headers, _Body}} =				 http:request(Request)
	catch
		Type:Why -> io:format("~p : ~p~n", [Type, Why]) 
	end.
	
