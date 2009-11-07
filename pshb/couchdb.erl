%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%% @doc
%%%	CouchDB access methods (wrapper for erlang_couchdb)
%%%
%%%	referces
%%%		http://couchdb.apache.org/
%%%		http://books.couchdb.org/relax/
%%%		http://wiki.apache.org/couchdb/HTTP_Document_API
%%%		http://wiki.apache.org/couchdb/HTTP_view_API?action=show&redirect=HttpViewApi
%%%	better solution (raw interface of this module)
%%%		http://github.com/ngerakines/erlang_couchdb/
%%%
-module(couchdb).
-export([
		 connect/1, status/0
		]).
-export([
		 all_db/0, create_db/1, delete_db/1, info_db/1
		]).
-export([
		 all_docs/1, get_doc/2, rev_doc/2,
		 create_doc/2, create_docs/2, update_doc/3,
		 delete_doc/3
		]).
%%	RESTful API
-export([
		 rest_get/1, rest_put/2, rest_post/2, rest_delete/1
		]).

-include("pshb.hrl").

-define(CONF(X), proplists:get_value(X, erlang:get(?COUCHDBREF))).

connect({Server, Port}) ->
	Config = [{server, {Server, Port}}],
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
	erlang_couchdb:server_info(?CONF(server))
	.

all_db() ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:retrieve_all_dbs({Server, Port})
	.

info_db(Db) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:database_info({Server, Port}, Db)
	.
	
create_db(Db) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:create_database({Server, Port}, Db)
	.

delete_db(Db) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:delete_database({Server, Port}, Db)
	.

all_docs(Db) ->
	{ok,{_Status, _ResponceHeaders, Json}} = rest_get("/" ++ Db ++ "/_all_docs"),
	Json
	.

rev_doc(Db, Id) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:document_revision({Server, Port}, Db, Id)
	.

get_doc(Db, Id) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:retrieve_document({Server, Port}, Db, Id)
	.

create_doc(Db, Attribute) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:create_document({Server, Port}, Db, Attribute)
	.

create_docs(Db, Documents) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:create_documents({Server, Port}, Db, Documents)
	.

update_doc(Db, Id, Attribute) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:update_document({Server, Port}, Db, Id, Attribute)
	.

delete_doc(Db, Id, Revision) ->
	{Server, Port} = ?CONF(server),
	erlang_couchdb:delete_document({Server, Port}, Db, Id, Revision)
	.

rest_get(Path) ->
	{Server, Port} = ?CONF(server),
	{ok, _Result} = http:request(get,{string:join(["http://", Server, ":", integer_to_list(Port), Path],""),[]},[],[])
	.

rest_put(Path, Body) ->
	{Server, Port} = ?CONF(server),
	ContentType = ?JSONTYPE,
	{ok, _Result} = http:request(put,{string:join(["http://", Server, ":", integer_to_list(Port) ,Path], ""),[],ContentType,Body},[],[])
	.

rest_post(Path, Body) when is_binary(Body) ->
	{Server, Port} = ?CONF(server),
	ContentType = ?JSONTYPE,
	{ok, _Result} = http:request(post,{string:join(["http://", Server, ":", integer_to_list(Port), Path], ""),[],ContentType,Body},[],[])
	;
rest_post(Path, Body) ->
	{Server, Port} = ?CONF(server),
	ContentType = "application/x-www-form-urlencoded",
	{ok, _Result} = http:request(post,{string:join(["http://", Server, ":", integer_to_list(Port), Path], ""),[],ContentType,Body},[],[])
	.

rest_delete(Path) ->
	{Server, Port} = ?CONF(server),
	ContentType = "application/x-www-form-urlencoded",
	{ok, _Result} = http:request(delete,{string:join(["http://", Server, ":", Port, Path], ""),[], ContentType, ""},[],[])
	.


%%%
%%%	Query design
%%%
