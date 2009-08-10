-module(rtm).
%%-export([start/0,login/0,getfrob/0,test/0]).
-compile(export_all).

-include("rtm.hrl").

%%%
%%% library routines
%%%

head({A,_B}) -> A.

joint({A,B}) -> list_to_binary([A, B]).
paramjoint({A,B}) -> A ++ "=" ++ B.

fillzero(32) -> $0;
fillzero(A) -> A.

repeatchar(Char, Num) ->
	repeatchar_(Char, Num, []).

repeatchar_(_, 0, Stor) -> lists:flatten(Stor);
repeatchar_(_, N, Stor) when N < 0 -> lists:fratten(Stor);
repeatchar_(Char, N, Stor) -> 
	repeatchar_(Char, N - 1, [Char | Stor]).

sortbykey(List) -> 
	lists:sort(fun(A,B) -> head(A) =< head(B) end, List).

mergetuple([], Str) -> list_to_binary(lists:reverse(Str));
mergetuple([H|L], Str) -> mergetuple(L, [joint(H) | Str]).

makeparam([], Str) -> lists:flatten(lists:reverse(Str));
makeparam([H|[]], Str) -> makeparam([], [paramjoint(H) | Str]);
makeparam([H|L], Str) -> makeparam(L, [paramjoint(H) ++ "&"| Str]).

hexdump(L) -> lists:map(fun(A) -> fillzero(A) end, hexdump_(L,[])).
hexdump_([],Stor) -> lists:flatten(lists:reverse(Stor));
hexdump_([H|T],Stor) -> hexdump_(T, [io_lib:format("~2.16x",[H, ""]) | Stor]).

signStr(ParamTuple) -> binary_to_list(crypto:md5(list_to_binary([?APISECRET, mergetuple(sortbykey(ParamTuple),[])]))).

signhexStr(ParamTuple) -> hexdump(signStr(ParamTuple)).

callapi(RawParam) ->
	callrtm_(RawParam, ?APIURL).

callweb(RawParam) ->
	callrtm_(RawParam, ?AUTHURL).

callrtm_(RawParams, BaseURL) ->
	Signature = signhexStr(RawParams),
	Params = [{"api_sig", Signature}|RawParams],
	URL = lists:flatten([BaseURL | makeparam(Params, [])]),
	io:format("URL = ~p~n",[URL]),

	receive
		cancel -> void
	after ?ACCESSWAITMSEC ->
		http:request(URL)
	end.

checkresponse(Result) ->
	case Result of
		{ok, {_Status, _Header, Body}} -> Body;
		_Response ->
			io:format("bad request : ~p~n",[_Response]),
			_Response
	end.

%%%
%%% Initialize
%%%
start() ->
	inets:start(),
	crypto:start().

%%%     %%%
%%% API %%%
%%%     %%%
	
%%%
%%% Auth
%%%
%%%    * rtm.auth.checkToken - Token validation

checkToken() ->
	checkresponse(checkToken_()).

checkToken_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.auth.checkToken"}
		]).

%%%    * rtm.auth.getFrob - get consumer Token(frob)

getfrob() ->
	checkresponse(getfrob_()).

getfrob_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.auth.getFrob"}
		]).

frob2string(Frob) -> Frob.

%%%    * rtm.auth.getToken - get Token

getToken(Frob) ->
	checkresponse(getToken_(Frob)).

getToken_(Frob) ->
	callapi([
		{"api_key", ?APIKEY},
		{"frob", Frob},
		{"method", "rtm.auth.getToken"}
		]).

%%%
%%% Contacts object
%%%
%%%    * rtm.contacts.add

contactsAdd(Timeline, Contact) ->
	checkresponse(contactsAdd_(Timeline, Contact)).

contactsAdd_(Timeline, Contact) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"contact", Contact},
		{"method", "rtm.contacts.add"}
		]).

%%%    * rtm.contacts.delete

contactsDelete(Timeline, ContactId) ->
	checkresponse(contactsDelete_(Timeline, ContactId)).

contactsDelete_(Timeline, ContactId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"contact_id", ContactId},
		{"method", "rtm.contacts.delete"}
		]).

%%%    * rtm.contacts.getList

contactsGetList() ->
	checkresponse(contactsGetList_()).

contactsGetList_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.contacts.getList"}
		]).

%%%
%%% Groups object 
%%%
%%%    * rtm.groups.add

groupsAdd(Timeline, Group) ->
	checkresponse(groupsAdd_(Timeline, Group)).

groupsAdd_(Timeline, Group) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"group", Group},
		{"method", "rtm.groups.add"}
		]).

%%%    * rtm.groups.delete

groupsDelete(Timeline, GroupId) ->
	checkresponse(groupsDelete_(Timeline, GroupId)).

groupsDelete_(Timeline, GroupId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"group_id", GroupId},
		{"method", "rtm.groups.delete"}
		]).

%%%    * rtm.groups.getList

groupsGetList() ->
	checkresponse(groupsGetList_()).

groupsGetList_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.groups.getList"}
		]).


%%%
%%% Usecase : Authentication
%%%


login() ->
	login(?APIKEY).

login(Key) ->
	login(Key, "read").

login(Key, Priv) ->
	case login_(Key, Priv) of
		{ok, {_Status, _Header, Body}} -> Body;
		_Response ->
			io:format("bad request : ~p~n",[_Response]),
			_Response
	end.

login_(Key, Priv) ->
	callweb([
		{"api_key", Key},
		{"perms", Priv}
		]). 
%%%
%%% TEST
%%%

testname(Str) ->
	Underline = repeatchar($=, length(Str ++ " test")),
	io:format("~n~n~s test~n~s~n",[Str, Underline]).


test() ->
	start(),
	logintest(),
	test_1(),
	test_2(),
	test_3(),
	test_4(),
	test_5().

logintest() ->
	testname("logintest"),
	getfrob().
	

test_1()->
	testname("signature"),
	A = signStr([{"ab", "cde"}, {"cd", "DEF"}]),
        B = "49f56ed8de703597d43d01d6c8bdde54", 
	C = hexdump(A),
	io:format("~p~n~p~n",[crypto:md5(B), crypto:md5(C)]),
	io:format("~s~n~s~n",[B, C]),
	case C == B of
		true -> ok;
		_ -> md5dumpfailure
	end.

test_2() -> 
	testname("web login"),
	login().

test_3() ->
	testname("getfrob API"),
	login(me, "read"),
	getfrob().

test_4() ->
	testname("contact API"),
	contactsGetList().

test_5() ->
	testname("groups API"),
	groupsGetList().
