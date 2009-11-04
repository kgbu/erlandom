%%%	@author OGAKI, Kazutaka <kgbu@keynauts.com>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%%	PubSubHubbub system 
%%%
%%% FIXME : rewrite in supervisor behaviour
%%%
%%% ref: http://pubsubhubbub.googlecode.com/svn/trunk/pubsubhubbub-core-0.2.html

%%%
%%  registered server
%
%	0) supervisor :	service management loop
%	0.5) uuid			:	uuid server
%   1) webhook			:   httpd for 
%								pshb_callback 
%								socnode web view
%								socnode entry 
%   2) pshb_subscriber  :   RPC endpoint
%   3) content_mgr      :   update contents/notice to browser

%	feed db	: couchDB

-module(pshb).
-export([start/0]).

%	just for spawn
-export([loop/1]).

-include("pshb.hrl").

%%	@spec start() -> pid()
%%
%%	@doc
%%	start services
%%
start() ->
	crypto:start(),
	inets:start(),

	uuid:start(),
	
	couchdb:connect({?COUCHDBSVR, ?COUCHDBPORT}),
	couchdb:status(),

	webhook:start(),
	pshb_subscriber:start(),
    content_mgr:start(),

    Pid = spawn(?MODULE, loop, [[]]),
    erlang:register(pshb, Pid)
    .

loop(State) ->
	receive
		{stop} ->
		exit(normal);
		_ ->
			loop(State)
	end
	.
