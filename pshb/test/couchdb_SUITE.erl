-module(couchdb_SUITE).
-compile(export_all).
-include("ct.hrl").

all() -> [status].

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

suite() ->
	[{timetrap, {minutes, 1}}]
	.

status() -> 
	[{userdata,[{doc,"Perform a status check of CouchDB server."}]}]
	.
	
status(_Config) -> 
    couchdb:connect({"localhost", 5984}),
    couchdb:status()
	.
