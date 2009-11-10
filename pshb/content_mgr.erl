%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%% content_mgr - gateway to DB for webservice (socnode, this time) 
%%%	couchdb module is backend DB (shall be pluggable)
%%%

-module(content_mgr).
-export([start/0, rpc/1, stop/0]).
-export([access_id/1, access_range/2, post_entry/2, post_comment/3]).
-export([get_couchdb/1, put_couchdb/1, post_couchdb/2, delete_couchdb/1]).


%%% internal loop use
-export([loop/2, updater/2]).

-include("pshb.hrl").

start() ->
	access_couchdb(),
	case ets:file2tab(?SAVED_ETS) of
		{ok, Table} ->
			Table
			;
		{error, {read_error,{not_a_log_file, _}}} ->
			error_logger:info_msg("ets not_a_log_file error~n",[]),
			Table = ets:new(?TABLE_NAME, []),
			ets:tab2file(Table, ?SAVED_ETS);
		{error, Reason} ->
			error_logger:error_msg("ets error: ~p~n",[Reason]),
			Table = ets:new(?TABLE_NAME, []),
			ets:tab2file(Table, ?SAVED_ETS)
	end,
    PidContent = spawn(?MODULE, loop, [[], Table]),
    erlang:register(content_mgr, PidContent)
	.

access_couchdb() ->
	error_logger:info_msg("accessing database ~p on CouchDB ~p ~p",[?SOCNODEDB, ?COUCHDBSVR, ?COUCHDBPORT]),
	case erlang_couchdb:database_info({?COUCHDBSVR, ?COUCHDBPORT}, ?SOCNODEDB) of
		{error, _Reason} -> 
			erlang_couchdb:create_database({?COUCHDBSVR, ?COUCHDBPORT}, ?SOCNODEDB)
			;
		_ -> ok
	end
	.

stop() ->
	error_logger:info_msg("stoppin",[]),
	case whereis(content_mgr) of
		undefined ->
			do_nothing
			;
		_ ->
			?MODULE:rpc({stop})
	end,
	{ok}
	.

access_id(Id) ->
	error_logger:info_msg("access by Id ~p",[Id]),
	Topic = ?CONTENTPATHBASE ++ Id,
	?MODULE:rpc({get, Topic})
	.

access_range(Offset, Length) ->
	Topic = ?CONTENTPATHBASE ++ "?skip=" ++ Offset ++ "&limit=" ++ Length,
	?MODULE:rpc({get, Topic})
	.

post_entry(_Author, Content) ->
	Topic = ?CONTENTPATHBASE,
	?MODULE:rpc({post, Topic, Content})
	.

post_comment(Ref, _Author, Content) ->
	Topic = ?CONTENTPATHBASE ++ Ref,
	?MODULE:rpc({post, Topic, Content})
	.

get_couchdb(Topic) ->
	?MODULE:rpc({get, Topic})
	.

put_couchdb(Topic) ->
	?MODULE:rpc({put, Topic})
	.

post_couchdb(Topic, Data) ->
	?MODULE:rpc({post, Topic, Data})
	.

delete_couchdb(Topic) ->
	?MODULE:rpc({get, Topic})
	.

%%	operation backend
%

rpc(Command) ->
	content_mgr ! {self(), Command},
	receive
		{content_mgr, Result} ->
			Result;
		_ ->
			false
	end
	.


loop(State, Table) ->
    receive
		%%
		%%	SYNC call event (do immediate job and reply) 
		%%
		{UserPid, {access_id, Id}} ->
			Topic = id_to_topic(Id),
			UserPid ! {self(), erlang_couchdb:get(Topic)},
			loop(State, Table)
			;
		%
		%	couchDB generic operation
		%
		{UserPid, {get, Path}} ->
			UserPid ! {self(), couchdb:rest_get(Path)},
			loop(State, Table)
			;
		{UserPid, {put, Path, Data}} ->
			couchdb:rest_put(Path, Data),
			UserPid ! {ok},
			loop(State, Table)
			;
		{UserPid, {post, Path, Data}} ->
			couchdb:rest_post(Path, Data),
			UserPid ! {ok},
			loop(State, Table)
			;
		{UserPid, {delete, Path}} ->
			couchdb:rest_delete(Path),
			UserPid ! {ok},
			loop(State, Table)
			;
		%	service operation
		{UserPid, stop} ->
			ets:tab2file(Table, ?SAVED_ETS),
			UserPid ! {ok},
			exit(shutdown)
			;
		%%
		%%	cast (ASYNC call) event (do job, and set completion status later)
		%%
        {UserPid, {updated, Topic, Data}} ->
			S = self(),
			WorkerPid = spawn(?MODULE, updater, [S, Topic, Data]),
			UserPid ! {ok},
			loop([{Topic, WorkerPid} | State], Table)
			;
        {WorkerPid, {update_completed, Topic}} ->
			loop(lists:delete({Topic, WorkerPid}, State), Table)
			;
        _ ->
            loop(State, Table)
    end
    .


%%
%	worker for ASYNC subtask
%

updater(Topic, Data) ->
	erlang_couchdb:post(topic2path(Topic), Data),
	content_mgr ! {self(), {update_completed, Topic}}
	.

topic2path(Str) ->
	% FIXME : smarter path analysis
	erlang:md5(Str)
	.


%%
%	couchDB specific request transformation
%
id_to_topic(Id) ->
	Id
	.
