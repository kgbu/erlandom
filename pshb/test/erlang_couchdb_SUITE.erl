-module(erlang_couchdb_SUITE).
-compile(export_all).

-include("ct.hrl").

-define(CONNECTION, {"localhost", 5984}).
-define(DBNAME, "dfsfakkkweii_test").

%%--------------------------------------------------------------------
%% Test server callback functions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Function: suite() -> DefaultData
%% DefaultData: [tuple()]  
%% Description: Require variables and set default values for the suite
%%--------------------------------------------------------------------
suite() -> [{timetrap,{minutes,1}}].



all() ->
	[serverinfo, databaselifecycle, documentlifecycle]
	.

init_per_suite(Config) ->
    crypto:start(),
    inets:start(),
    uuid:start(),
    Config
    .

end_per_suite(_Config) ->
    inets:stop(),
    ok
    .

serverinfo() ->
	[{userdata,[{doc,"Server connection, and information"}]}]
	.

serverinfo(_Config) ->
	erlang_couchdb:server_info(?CONNECTION),
	ct:print(test_category, "server info ~p~n", [erlang_couchdb:database_info(?CONNECTION, ?DBNAME)]),
	ok
	.

databaselifecycle() ->
	[{userdata,[{doc,"Database cration, information, and deletion"}]}]
	.

databaselifecycle(_Config) ->
	erlang_couchdb:create_database(?CONNECTION, ?DBNAME),
	erlang_couchdb:database_info(?CONNECTION, ?DBNAME),
	erlang_couchdb:delete_database(?CONNECTION, ?DBNAME),
	ok
	.

documentlifecycle() ->
	[{userdata,[{doc,"Document creation, retrieve, and deletion"}]}]
	.

documentlifecycle(_Config) ->
	erlang_couchdb:create_database(?CONNECTION, ?DBNAME),
	{json,{struct,[_, {<<"id">>, Id},_]}} = erlang_couchdb:create_document(
		?CONNECTION, ?DBNAME, {struct, [{<<"foo">>, <<"bar">> } ]}),
	ct:print(test_category, "id: ~p", [Id]),
	erlang_couchdb:retrieve_document(?CONNECTION, ?DBNAME, binary_to_list(Id)),
	ok
	.
	
