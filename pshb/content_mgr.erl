%%% content_mgr - gateway to DB
%%%
%%%
-module(content_mgr).
-export([start/0, rpc/1]).


%%% internal loop use
-export([loop/2, updater/3]).

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

rpc(Command) ->
	content_mgr ! {self(), Command}
	.


loop(State, Table) ->
    receive
		{_UserPid, {access, Topic, _Query}} ->
			couchdb:get(Topic),
			loop(State, Table)
			;
		{_UserPid, {post, Path, Topic}} ->
			couchdb:post(Path, Topic),
			loop(State, Table)
			;
        {_Pid, {updated, Topic, Data}} ->
			S = self(),
			WorkerPid = spawn(?MODULE, updater, [S, Topic, Data]),
			loop([{Topic, WorkerPid} | State], Table)
			;
        {WorkerPid, {update_completed, Topic}} ->
			loop(lists:delete({Topic, WorkerPid}, State), Table)
			;
		{_Pid, shutdown} ->
			ets:tab2file(Table, ?SAVED_ETS),
			exit(shutdown)
			;
        _ ->
            loop(State, Table)
    end
    .


%%
%	worker for subtask
%

updater(Pid, Topic, Data) ->
	couchdb:post(topic2path(Topic), Data),
	Pid ! {update_completed, {Topic, self()}}
	.

topic2path(Str) ->
	% FIXME : smarter path analysis
	erlang:md5(Str)
	.