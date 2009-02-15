-module(getatom).
-export([new/3]).

new(User, Password, Uri) ->
    RequestHeader = [{ "X-WSSE" , wsse:new(User, Password)}],

    inets:start(),
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} =
	http:request(get, {Uri, RequestHeader}, [], []), 
    Body.
