%%% content_mgr - gateway to DB
%%%
%%%
-module(content_mgr).
-export([start/0, loop/2, stop/0, updater/3]).

-include("pshb.hrl").

start() ->
	case ets:file2tab(?SAVED_ETS) of
		{ok, Table} ->
			Table;
		{error, {read_error,{not_a_log_file, _}}} ->
			Table = ets:new(?TABLE_NAME, []),
			ets:tab2file(Table, ?SAVED_ETS)
	end,
    PidContent = spawn(?MODULE, loop, [[], Table]),
    erlang:register(content_mgr, PidContent)
	.


loop(State, Table) ->
    receive
		{UserPid, access, Topic, _Query} ->
			UserPid ! couchdb:get(Topic),
			loop(State, Table)
			;
        {updated, {Topic, Data}} ->
			S = self(),
			WorkerPid = spawn(?MODULE, updater, [S, Topic, Data]),
			loop([{Topic, WorkerPid} | State], Table)
			;
        {update_completed, {Topic, WorkerPid}} ->
			loop(lists:delete({Topic, WorkerPid}, State), Table)
			;
		{shutdown} ->
			ets:tab2file(Table, ?SAVED_ETS),
			exit(shutdown)
			;
        _ ->
            loop(State, Table)
    end
    .

stop() ->
	content_mgr ! {shutdown}
	.


%%
%	tasks
%

updater(Pid, Topic, Data) ->
	couchdb:post(topic2path(Topic), Data),
	Pid ! {update_completed, {Topic, self()}}
	.

topic2path(Str) ->
	% FIXME : smarter path analysis
	erlang:md5(Str)
	.
