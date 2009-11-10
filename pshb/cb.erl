%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%%	callback CGI module for pubsubhubbub operations
%%%

-module(cb).
%-export([notice/3]).
-compile(export_all).

-include("pshb.hrl").
%-define(TEXTHEADER, "Content-type:text/html\r\n\r\n").

notice(SessionID, _Env, _Input) when not is_pid(SessionID) ->
	mod_esi:deliver(SessionID, ?TEXTHEADER ++ "hi")
	;

notice(SessionID, Env, Input) ->
	case proplists:get_value(request_method , Env) of
		get -> mod_esi:deliver(SessionID, response_verification(Env, Input));
		"GET" -> mod_esi:deliver(SessionID, response_verification(Env, Input));
		"get" -> mod_esi:deliver(SessionID, response_verification(Env, Input));
		post -> mod_esi:deliver(SessionID, response_notice(Env, Input));
		"POST" -> mod_esi:deliver(SessionID, response_notice(Env, Input));
		"post" -> mod_esi:deliver(SessionID, response_notice(Env, Input));
		_ -> false
	end
	.

response_verification(Env, GetData) ->
	% set response with hub.challenge and set (2xx) status

	QueryString = proplists:get_value(query_string, Env, ""),
	ParsedQuery = httpd:parse_query(QueryString),
	Topic = proplists:get_value("hub.topic", ParsedQuery, "no challange"),
	pshb_subscriber ! {push_verify, {Topic, Env, GetData}},
	?TEXTHEADER ++ 
%DEBUG		io_lib:format("env: ~p~n",[Env]) ++ 
%DEBUG		io_lib:format("query: ~p~n",[QueryString]) ++ 
		proplists:get_value("hub.challenge", ParsedQuery, "no challange")
	.

response_notice(Env, PostData) ->
	% Content_length = proplists:get_value(query_string, Env, []),
	% just store data, and return success (2xx) status

	% TODO
	%	signature check
	%	add aggregated contents signature check

	ParsedData = httpd:parse_query(PostData),
	Topic = proplists:get_value("hub.topic", ParsedData, no_topic),
	content_mgr ! {update_notice, Topic, {Env, PostData}},
	?TEXTHEADER ++ "roger."
	.
