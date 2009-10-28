%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%%	PubSubHubbub client : publisher functionality
%%%	ref: http://pubsubhubbub.googlecode.com/svn/trunk/pubsubhubbub-core-0.2.html

-module(pshb_publisher).
-export([send_ping/2]).
-include("pshb.hrl").

% send POST to hub : New Content Notification
send_ping(Mode, Topic) ->
	ModeisOK = lists:member(Mode,["publish"]),
	TopicUrlisOK = pshb_utils:is_absoluteurl(Topic),
	if
		ModeisOK, TopicUrlisOK ->
			ContentType = "application/x-www-form-urlencoded",
            ParamList = [{"hub.mode", Mode}, {"hub.url", Topic}],
            PreBody = lists:flatten(lists:map(fun({A,B}) -> [A, "=", B, "&"] end, ParamList)),
            Body = string:sub_string(PreBody, 1, length(PreBody) - 1),
			Result = http:request(post,{?HUB, [], ContentType, Body}, [],[]),
			case Result of
				{ok, _} -> {ok, Result};
				true -> {error, Result}
			end;
		true -> {error, [{mode, ModeisOK}, {topic, TopicUrlisOK}]}
	end
	.
