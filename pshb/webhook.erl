%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%% @doc
%%%	webhook - HTTPd for invoke callbacks
%%%
-module(webhook).
-export([start/0, stop/0, info/0, reload/1]).
-include("pshb.hrl").

start() ->
	ServiceConfig = [
		{port, ?SUBSCRIBERPORT},
		{server_name, "www.jin.gr.jp"},
		{server_root, "/home/kgbu/Erlang/erlandom/pshb/webhook"},
		{document_root, "/home/kgbu/Erlang/erlandom/pshb/webhook/htdocs"},
		{log_format, combined},
		{error_log_format, pretty},
		{error_log, "logs/error_log"},
		{security_log, "logs/security_log"},
		{transfer_log, "logs/access_log"},
		{modules, [mod_alias, mod_auth, mod_esi, mod_actions, mod_cgi, mod_dir, mod_get, mod_head, mod_log, mod_disk_log]},
		{erl_script_alias, {"/do", [socnode, cb]}}
	],
	{ok, Pid} = inets:start(httpd, ServiceConfig),
	% io:format("pids: ~p~n", [lists:map(fun(X) -> whereis(X) end, registered())]),
    % io:format("registered :~p~n", [registered()]),
    % io:format("sevice info:~p~n", [inets:services_info()]),
	% erlang:register(webhook, Pid),
	Pid
	.

stop() ->
	inets:stop()
	.

info() ->
	httpd:info(webhook)
	.

reload(Config) ->
    inets:reload_config(Config)
    .
