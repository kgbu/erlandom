%%%	socnode - communication service 
%%%
%%% RPC API
%%%
%%%	webhook API
%%%    webhookURL/socnode/OP
%%%		OP: [login, list, entry, entry, comment, search]
%%%
%%%
-module(socnode).

-export([login/3,
		list/3,
		entry/3,
		search/3,
		notice/3]).

-include("pshb.hrl").


%%%
%%	mod_esi callback
%

login(SessionID, Env, Input) ->
	Method = proplists:get_value(request_method , Env),
    case is_getmethod(Method) of
		true -> 
       		mod_esi:deliver(SessionID, do_login(Env, Input));
		_ -> false
    end
    .

do_login(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.

list(SessionID, Env, Input) ->
	Method = proplists:get_value(request_method , Env),
    case is_getmethod(Method) of
		true ->
       		mod_esi:deliver(SessionID, view_list(Env, Input));
		_ -> false
    end
    .

view_list(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.

entry(SessionID, Env, Input) ->
	Method = method_of(proplists:get_value(request_method , Env)),
    case Method of
		get ->
        	mod_esi:deliver(SessionID, view_entry(Env, Input));
		post ->
        	mod_esi:deliver(SessionID, post_entry(Env, Input));
		delete ->
        	mod_esi:deliver(SessionID, delete_entry(Env, Input));
        true -> false
    end
	.

view_entry(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.

post_entry(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.

delete_entry(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.


search(SessionID, Env, Input) ->
	Method = proplists:get_value(request_method , Env),
    case is_getmethod(Method) of
		true ->
       		mod_esi:deliver(SessionID, view_searchresult(Env, Input));
		_ -> false
    end
    .

view_searchresult(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.

notice(SessionID, Env, Input) ->
	Method = proplists:get_value(request_method , Env),
    case is_getmethod(Method) of
		true ->
       		mod_esi:deliver(SessionID, get_notice(Env, Input));
		_ -> false
    end
    .

get_notice(Env, Input) ->
	{error, {fixme, {Env, Input}}}
	.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%%	utilities
%%

method_of(Value) ->
	case is_getmethod(Value) of
		true -> get;
		_ -> case is_putmethod(Value) of
			true -> put;
			_ -> case is_postmethod(Value) of
				true -> post;
				_ -> case is_deletemethod(Value) of
				true -> delete;
				_ -> undefined
				end
			end
		end
	end
	.

is_getmethod(Value) ->
	is_method(get, Value)
	.

is_postmethod(Value) ->
	is_method(post, Value)
	.

is_putmethod(Value) ->
	is_method(put, Value)
	.

is_deletemethod(Value) ->
	is_method(delete, Value)
	.

is_method(Method, Value) when is_atom(Method), is_list(Value) ->
	LowercaseValue = string:to_lower(Value),
	atom_to_list(Method) =:= LowercaseValue
	;
is_method(Method, Value) when is_atom(Method) ->
	Method =:= Value
	;
is_method(_,_) ->
	false
	.
