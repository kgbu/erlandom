%%%	CouchDB access methods
%%%
%%%	referces
%%%		http://couchdb.apache.org/
%%%		http://books.couchdb.org/relax/
%%%		http://wiki.apache.org/couchdb/HTTP_Document_API
%%%		http://wiki.apache.org/couchdb/HTTP_view_API?action=show&redirect=HttpViewApi
%%%	
-module(couchdb).
-export([connect/1, status/0,
		 rest_get/1, rest_put/2, rest_post/2, rest_delete/1]).

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

rest_get(Path) ->
	Server = ?CONF(server),
	{ok, _Result} = http:request(get,{string:join([Server, Path],""),[]},[],[])
	.

rest_put(Path, Body) ->
	Server = ?CONF(server),
	ContentType = ?JSONTYPE,
	{ok, _Result} = http:request(put,{string:join([Server, Path], ""),[],ContentType,Body},[],[])
	.

rest_post(Path, Body) when is_binary(Body) ->
	Server = ?CONF(server),
	ContentType = ?JSONTYPE,
	{ok, _Result} = http:request(post,{string:join([Server, Path], ""),[],ContentType,Body},[],[])
	;
rest_post(Path, Body) ->
	Server = ?CONF(server),
	ContentType = "application/x-www-form-urlencoded",
	{ok, _Result} = http:request(post,{string:join([Server, Path], ""),[],ContentType,Body},[],[])
	.

rest_delete(Path) ->
	Server = ?CONF(server),
	ContentType = "application/x-www-form-urlencoded",
	{ok, _Result} = http:request(delete,{string:join([Server, Path], ""),[], ContentType, ""},[],[])
	.


%%%
%%%	Query design
%%%
