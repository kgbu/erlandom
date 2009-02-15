-module(wsse).
-export([new/2]).

-define(SHA1DIGESTLENGTH, 20).

new (User, Password) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    Nonce = nonce(?SHA1DIGESTLENGTH, ""),

%% create ISO 8601 compling datetime
%% $ ruby -e " require 'open-uri' ; p Time.now.iso8601"
%% => "2008-07-31T16:16:14+09:00"
%%
%%  ref) http://www.trapexit.org/Converting_Between_struct:time_and_ISO8601_Format
    {{Year, Month, Day}, {Hour, Min, Sec}} = erlang:localtime(),
    TZ = os:cmd("date +%:z"),
    Created = io_lib:format("~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~6s", [Year, Month, Day, Hour, Min, Sec, TZ]),
    crypto:start(),
    Digest = binary_to_list(crypto:sha(Nonce ++ Created ++ Password)),

    "UsernameToken Username=\"" ++ User ++ "\", " ++
    "PasswordDigest=\"" ++
    base64:encode_to_string(Digest) ++ "\", " ++
    "Nonce=\"" ++
    base64:encode_to_string(Nonce) ++ "\", " ++
    "Created=\"" ++ Created ++ "\"".

nonce(0,L) -> L ++ [random:uniform(255)];
nonce(N,L) -> nonce(N -1, L ++ [random:uniform(255)]).
