%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%%	PubSubHubbub client : subscriber services
%%% ref: http://pubsubhubbub.googlecode.com/svn/trunk/pubsubhubbub-core-0.2.html


-module(pshb_subscriber).
-export([start/0, stop/0,
		atom_to_subscribe/1, local_subscribe/1, raw_subscribe/2, 
		atom_to_unsubscribe/1, local_unsubscribe/1, raw_unsubscribe/2
		]).
-export([loop/1]).

-include("pshb.hrl").

%%%
%%	PubSubHubbub API for subscriber
%

%%
%	send subscription request
%

atom_to_subscribe(AtomUri) ->
	{ok, {Topic, Hub}} = pshb_utils:scrape(AtomUri),
	raw_subscribe(Topic, Hub)
	.

local_subscribe(Topic) ->
	send_subscription(?HUB, ?CALLBACKURL, "subscribe", Topic, "sync", [])
	.

raw_subscribe(Topic, Hub) ->
	send_subscription(Hub, ?CALLBACKURL, "subscribe", Topic, "sync", [])
	.

atom_to_unsubscribe(AtomUri) ->
	{ok, {Topic, Hub}} = pshb_utils:scrape(AtomUri),
	raw_unsubscribe(Topic, Hub)
	.

local_unsubscribe(Topic) ->
	send_subscription(?HUB, ?CALLBACKURL, "unsubscribe", Topic, "sync", [])
	.

raw_unsubscribe(Topic, Hub) ->
	send_subscription(Hub, ?CALLBACKURL, "unsubscribe", Topic, "sync", [])
	.

send_subscription(Hub, Callback, Mode, Topic, Verify, Options) ->
	ModeisOK = lists:member(Mode,["subscribe", "unsubscribe"]),
	VerifyisOK = lists:member(Verify, ["sync", "async"]),
	CallbackUrlisOK = pshb_utils:is_absoluteurl(Callback),
	TopicUrlisOK = pshb_utils:is_absoluteurl(Topic),
	if
		ModeisOK, VerifyisOK, CallbackUrlisOK, TopicUrlisOK ->
			PreParam =[
				{"hub.callback", Callback},
				{"hub.mode", Mode},
				{"hub.topic", Topic},
				{"hub.verify", Verify},
				{"hub.lease_seconds", proplists:get_value(lease_seconds, Options)},
				{"hub.secret", proplists:get_value(secret, Options)}, 
				{"hub.verify_token", proplists:get_value(verify_token, Options)}
			],
			ParamList = [ {X, Y} || {X, Y} <- PreParam, Y =/= undefined],  
			PreBody = lists:flatten(lists:map(fun({A,B}) -> [A, "=", B, "&"] end, ParamList)),
			Body = string:sub_string(PreBody, 1, length(PreBody) - 1),	
			ContentType = "application/x-www-form-urlencoded",
			{ok, _} = http:request(post,{Hub,[],ContentType,Body},[],[]),
			{ok, Result} = ask_verify(Topic),
			Result;
		true -> false
	end
	.

ask_verify(Topic) ->
	pshb_subscriber ! {ask_verify, self(), Topic},
	receive
		{ok, Result} -> {ok, Result};
		{error, Reason} -> {error, Reason}
	after 10000 ->
		{error, timeout}
	end
	.


%
%	subscriber service
%

%%	setup server to receive Callback
start() ->
	Pid = spawn(?MODULE, loop, [[]]),
	erlang:register(pshb_subscriber, Pid),
	Pid
	.

loop(StateStack) ->
	receive
		{stop} ->
			io:format("hihii",[]),
			exit(normal);
		{update_notice, {Topic, Data}} ->
			content_mgr ! {updated, {Topic, Data}},
			loop(StateStack);
		{push_verify, {Topic, Env, GetData}} ->
			loop([{verify, {Topic, Env, GetData}} | StateStack]);
		{ask_verify, Pid, Topic} ->
			case lists:member({verify, Topic}, StateStack) of
				true ->
					Pid ! {ok, Topic},
					loop(lists:delete({verify, Topic}, StateStack));
				false -> 
					Pid ! {error, verify_not_arrived},
					loop(StateStack)
			end;
		_Any ->
			loop(StateStack)
	end
	.


stop() ->
	pshb_subscriber ! {stop}
	.
