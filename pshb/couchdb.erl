%%%	CouchDB access methods
%%%
-module(couchdb).
-export([connect/1, status/0, get/1, put/1, post/2, delete/1]).

-include("pshb.hrl").

-define(CONF(X), proplists:get_value(X, erlang:get(?COUCHDBREF))).

connect(Config) when is_list(Config) ->
	erlang:put(?COUCHDBREF, Config),
	{ok, Config}
	;
connect(_BadConfig)  ->
	{error, no_proplist}
	.

%%%
%%%	API
%%%

status() ->
	{ok,{_Status, _ResponceHeaders, Json}} = ?MODULE:get("/"),
	Json
	.

get(Path) ->
	Server = ?CONF(server),
	{ok, _Result} = http:request(get,{string:join([Server, Path],""),[]},[],[])
	.

put(Path) ->
	Server = ?CONF(server),
	{ok, _Result} = http:request(put,{string:join([Server, Path],""),[]},[],[])
	.

post(Path, Body) when is_binary(Body) ->
	Server = ?CONF(server),
	ContentType = "multipart/form-data",
	{ok, _Result} = http:request(post,{string:join([Server, Path], ""),[],ContentType,Body},[],[])
	;
post(Path, Body) ->
	Server = ?CONF(server),
	ContentType = "application/x-www-form-urlencoded",
	{ok, _Result} = http:request(post,{string:join([Server, Path], ""),[],ContentType,Body},[],[])
	.

delete(Path) ->
	Server = ?CONF(server),
	ContentType = "application/x-www-form-urlencoded",
	{ok, _Result} = http:request(delete,{string:join([Server, Path], ""),[], ContentType, ""},[],[])
	.
