%%
%%	Remember the Milk library
%%
-module(rtm).
%%
%%-export([start/0,login/0,getfrob/0,test/0]).
%%
%% just for unit-testing
%%
-compile(export_all).

-include("rtm.hrl").
%
% sample configuration
%
% -define(APIKEY, "dummyKEYae2fshkkdfs").
% -define(APISECRET, "dummySECRETfaskkwiewrwwerk").
% -define(AUTHURL, "http://www.rememberthemilk.com/services/auth/?").
% -define(APIURL, "http://api.rememberthemilk.com/services/rest/?").
% -define(ACCESSWAITMSEC, 1500).

%%%
%%% utility routines
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
%%% Authentication
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

%%%    * rtm.groups.addContact

groupsAddContact(Timeline, GroupId, ContactId) ->
	checkresponse(groupsAddContact_(Timeline, GroupId, ContactId)).

groupsAddContact_(Timeline, GroupId, ContactId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"group_id", GroupId},
		{"contact_id", ContactId},
		{"method", "rtm.groups.addContact"}
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

%%%    * rtm.groups.removeContact

groupsRemoveContact(Timeline, GroupId, ContactId) ->
	checkresponse(groupsRemoveContact_(Timeline, GroupId, ContactId)).

groupsRemoveContact_(Timeline, GroupId, ContactId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"group_id", GroupId},
		{"contact_id", ContactId},
		{"method", "rtm.groups.removeContact"}
		]).

%%%
%%% Lists object 
%%%
%%%    * rtm.lists.add

listsAdd(Timeline, Name) ->
	checkresponse(listsAdd_(Timeline, Name, "")).
listsAdd(Timeline, Name, Filter) ->
	checkresponse(listsAdd_(Timeline, Name, Filter)).

listsAdd_(Timeline, Name, Filter) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"name", Name},
		{"filter", Filter},
		{"method", "rtm.lists.add"}
		]).

%%%    * rtm.lists.archive

listsArchive(Timeline, ListId) ->
	checkresponse(listsArchive_(Timeline, ListId)).

listsArchive_(Timeline, ListId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"method", "rtm.lists.Archive"}
		]).

%%%    * rtm.lists.delete

listsDelete(Timeline, ListId) ->
	checkresponse(listsDelete_(Timeline, ListId)).

listsDelete_(Timeline, ListId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"method", "rtm.lists.delete"}
		]).

%%%    * rtm.lists.getList

listsGetList() ->
	checkresponse(listsGetList_()).

listsGetList_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.lists.getList"}
		]).

%%%    * rtm.lists.setDefaultList

listssetDefalutList(Timeline, ListId) ->
	checkresponse(listssetDefaultList(Timeline, ListId)).

listssetDefaultList(Timeline, ListId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"method", "rtm.lists.setDefaultList"}
		]).

%%%    * rtm.lists.setName

listsSetName(Timeline, ListId, Name) ->
	checkresponse(listsSetName_(Timeline, ListId, Name)).

listsSetName_(Timeline, ListId, Name) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"name", Name},
		{"method", "rtm.lists.setName"}
		]).

%%%    * rtm.lists.unarchive

listsUnarchive(Timeline, ListId) ->
	checkresponse(listsUnarchive_(Timeline, ListId)).

listsUnarchive_(Timeline, ListId) ->
	callapi([
		{"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"method", "rtm.lists.unarchive"}
		]).

%%%
%%% Locations object 
%%%

%%%    * rtm.locations.getList

locationsGetList() ->
	checkresponse(locationsGetList_()).

locationsGetList_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.locations.getList"}
		]).

%%%
%%% API Reflection
%%%

%%%    * rtm.reflection.getMethodInfo

reflectiongetMethodInfo(MethodName) ->
	checkresponse(reflectiongetMethodInfo_(MethodName)).

reflectiongetMethodInfo_(MethodName) ->
	callapi([
		{"api_key", ?APIKEY},
		{"method_name", MethodName},
		{"method", "rtm.reflaction.getMethodInfo"}
		]).

%%%    * rtm.reflection.getMethods

reflectiongetMethods() ->
	checkresponse(reflectiongetMethods_()).

reflectiongetMethods_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.reflaction.getMethods"}
		]).

%%%
%%% Settings
%%%

%%%    * rtm.settings.getList
%%%	timezone, dateformat(0|1), timeformat(0|1), defaultlist, language

settingsgetList() ->
	checkresponse(settingsgetList_()).

settingsgetList_() ->
	callapi([
		{"api_key", ?APIKEY},
		{"method", "rtm.settings.getList"}
		]).

%%%
%%% Tasks
%%%

%%%    * rtm.tasks.add

tasksAdd(Timeline, ListId, Name, Parse) ->
        checkresponse(tasksAdd_(Timeline, ListId, Name, Parse)).

tasksAdd_(Timeline, ListId, Name, Parse) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"name", Name},
		{"parse", Parse},
                {"method", "rtm.tasks.add"}
                ]).

%%%    * rtm.tasks.addTags

tasksAddTags(Timeline, ListId, TaskseriesId, TaskId, Tags) ->
        checkresponse(tasksAddTags_(Timeline, ListId, TaskseriesId, TaskId, Tags)).

tasksAddTags_(Timeline, ListId, TaskseriesId, TaskId, Tags) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"tags", Tags},
                {"method", "rtm.tasks.addTags"}
                ]).

%%%    * rtm.tasks.complete

tasksComplete(Timeline, ListId, TaskseriesId, TaskId) ->
        checkresponse(tasksComplete_(Timeline, ListId, TaskseriesId, TaskId)).

tasksComplete_(Timeline, ListId, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.complete"}
                ]).

%%%    * rtm.tasks.delete

tasksDelete(Timeline, ListId, TaskseriesId, TaskId) ->
        checkresponse(tasksDelete_(Timeline, ListId, TaskseriesId, TaskId)).

tasksDelete_(Timeline, ListId, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.delete"}
                ]).

%%%    * rtm.tasks.getList

tasksgetList(Timeline, ListId, Filter, LastSync) ->
        checkresponse(tasksgetList_(Timeline, ListId, Filter, LastSync)).

tasksgetList_(Timeline, ListId, Filter, LastSync) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"filter", Filter},
		{"last_sync", LastSync},
                {"method", "rtm.tasks.delete"}
		]).

%%%    * rtm.tasks.movePriority

tasksmovePriority(Timeline, ListId, TaskseriesId, TaskId, Direction) ->
        checkresponse(tasksmovePriority_(Timeline, ListId, TaskseriesId, TaskId, Direction)).

tasksmovePriority_(Timeline, ListId, TaskseriesId, TaskId, Direction) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"direction", Direction},
                {"method", "rtm.tasks.movePriority"}
                ]).

%%%    * rtm.tasks.moveTo

tasksmoveTo(Timeline, From, To, TaskseriesId, TaskId) ->
        checkresponse(tasksmoveTo_(Timeline, From, To, TaskseriesId, TaskId)).

tasksmoveTo_(Timeline, From, To, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"from_list_id", From},
		{"to_list_id", To},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.moveTo"}
                ]).

%%%    * rtm.tasks.postpone

tasksPostpone(Timeline, ListId, TaskseriesId, TaskId) ->
        checkresponse(tasksPostpone_(Timeline, ListId, TaskseriesId, TaskId)).

tasksPostpone_(Timeline, ListId, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.postpone"}
                ]).

%%%    * rtm.tasks.removeTags

tasksRemoveTags(Timeline, ListId, TaskseriesId, TaskId, Tags) ->
        checkresponse(tasksRemoveTags_(Timeline, ListId, TaskseriesId, TaskId, Tags)).

tasksRemoveTags_(Timeline, ListId, TaskseriesId, TaskId, Tags) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"tags", Tags},
                {"method", "rtm.tasks.removeTags"}
                ]).

%%%    * rtm.tasks.setDueDate

taskssetDueDate(Timeline, ListId, TaskseriesId, TaskId, Due, HasDueTime, Parse) ->
	checkresponse(taskssetDueDate_(Timeline, ListId, TaskseriesId, TaskId, Due, HasDueTime, Parse)).

taskssetDueDate_(Timeline, ListId, TaskseriesId, TaskId, Due, HasDueTime, Parse) ->
	callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"due", Due},
		{"has_due_time", HasDueTime},
		{"parse", Parse},
                {"method", "rtm.tasks.setDueDate"}
		]).

%%%    * rtm.tasks.setEstimate

taskssetEstimate(Timeline, ListId, TaskseriesId, TaskId) ->
	checkresponse(taskssetEstimate_(Timeline, ListId, TaskseriesId, TaskId, "")).
taskssetEstimate(Timeline, ListId, TaskseriesId, TaskId, Estimate) ->
	checkresponse(taskssetEstimate_(Timeline, ListId, TaskseriesId, TaskId, Estimate)).

taskssetEstimate_(Timeline, ListId, TaskseriesId, TaskId) ->
	callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.setEstimate"}
		]).

taskssetEstimate_(Timeline, ListId, TaskseriesId, TaskId, Estimate) ->
	callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"estimate", Estimate},
                {"method", "rtm.tasks.setEstimate"}
		]).

%%%    * rtm.tasks.setLocation

taskssetLocation(Timeline, ListId, TaskseriesId, TaskId) ->
        checkresponse(taskssetLocation_(Timeline, ListId, TaskseriesId, TaskId )).
taskssetLocation(Timeline, ListId, TaskseriesId, TaskId, LocationId) ->
        checkresponse(taskssetLocation_(Timeline, ListId, TaskseriesId, TaskId, LocationId)).

taskssetLocation_(Timeline, ListId, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.setLocation"}
		]).
taskssetLocation_(Timeline, ListId, TaskseriesId, TaskId, LocationId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"location_id", LocationId},
                {"method", "rtm.tasks.setLocation"}
                ]).

%%%    * rtm.tasks.setName

taskssetName(Timeline, ListId, TaskseriesId, TaskId, Name) ->
	checkresponse(taskssetName_(Timeline, ListId, TaskseriesId, TaskId, Name)).
taskssetName_(Timeline, ListId, TaskseriesId, TaskId, Name) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"name", Name},
                {"method", "rtm.tasks.setName"}
                ]).

%%%    * rtm.tasks.setPriority

taskssetPriority(Timeline, ListId, TaskseriesId, TaskId, Priority) ->
	checkresponse(taskssetPriority_(Timeline, ListId, TaskseriesId, TaskId, Priority)). 
taskssetPriority_(Timeline, ListId, TaskseriesId, TaskId, Priority) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"priority", Priority},
                {"method", "rtm.tasks.setPriority"}
                ]).

%%%    * rtm.tasks.setRecurrence

taskssetRecurrence(Timeline, ListId, TaskseriesId, TaskId) ->
	checkresponse(taskssetRecurrence_(Timeline, ListId, TaskseriesId, TaskId, "")). 
taskssetRecurrence(Timeline, ListId, TaskseriesId, TaskId, Recurrence) ->
	checkresponse(taskssetRecurrence_(Timeline, ListId, TaskseriesId, TaskId, Recurrence)). 
taskssetRecurrence_(Timeline, ListId, TaskseriesId, TaskId, Recurrence) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"recurrence", Recurrence},
                {"method", "rtm.tasks.setRecurrence"}
                ]).

%%%    * rtm.tasks.setTags

taskssetTags(Timeline, ListId, TaskseriesId, TaskId) ->
	checkresponse(taskssetTags_(Timeline, ListId, TaskseriesId, TaskId, "")). 
taskssetTags(Timeline, ListId, TaskseriesId, TaskId, Tags) ->
	checkresponse(taskssetTags_(Timeline, ListId, TaskseriesId, TaskId, Tags)). 
taskssetTags_(Timeline, ListId, TaskseriesId, TaskId, Tags) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"tags", Tags},
                {"method", "rtm.tasks.setTags"}
                ]).

%%%    * rtm.tasks.setURL

taskssetUrl(Timeline, ListId, TaskseriesId, TaskId) ->
	checkresponse(taskssetUrl_(Timeline, ListId, TaskseriesId, TaskId, "")). 
taskssetUrl(Timeline, ListId, TaskseriesId, TaskId, Url) ->
	checkresponse(taskssetUrl_(Timeline, ListId, TaskseriesId, TaskId, Url)). 
taskssetUrl_(Timeline, ListId, TaskseriesId, TaskId, Url) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"url", Url},
                {"method", "rtm.tasks.setUrl"}
                ]).

%%%    * rtm.tasks.uncomplete

tasksuncomplete(Timeline, ListId, TaskseriesId, TaskId) ->
	checkresponse(tasksuncomplete_(Timeline, ListId, TaskseriesId, TaskId)). 
tasksuncomplete_(Timeline, ListId, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.uncomplete"}
		]).

%%%
%%% Tasks Notes
%%%

%%%    * rtm.tasks.notes.add

tasksnotesAdd(Timeline, ListId, TaskseriesId, TaskId, NoteTitle, NoteText) ->
	checkresponse(tasksnotesAdd_(Timeline, ListId, TaskseriesId, TaskId, NoteTitle, NoteText)). 

tasksnotesAdd_(Timeline, ListId, TaskseriesId, TaskId, NoteTitle, NoteText) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"note_title", NoteTitle},
		{"note_text", NoteText},
                {"method", "rtm.tasks.notes.add"}
		]).

%%%    * rtm.tasks.notes.delete

tasksnotesDelete(Timeline, ListId, TaskseriesId, TaskId) ->
	checkresponse(tasksnotesDelete_(Timeline, ListId, TaskseriesId, TaskId)). 

tasksnotesDelete_(Timeline, ListId, TaskseriesId, TaskId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
                {"method", "rtm.tasks.notes.delete"}
		]).

%%%    * rtm.tasks.notes.edit

tasksnotesEdit(Timeline, ListId, TaskseriesId, TaskId, NoteTitle, NoteText) ->
	checkresponse(tasksnotesEdit_(Timeline, ListId, TaskseriesId, TaskId, NoteTitle, NoteText)). 

tasksnotesEdit_(Timeline, ListId, TaskseriesId, TaskId, NoteTitle, NoteText) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"list_id", ListId},
		{"taskseries_id", TaskseriesId},
		{"task_id", TaskId},
		{"note_title", NoteTitle},
		{"note_text", NoteText},
                {"method", "rtm.tasks.notes.edit"}
		]).
%%%
%%% test
%%%

%%%    * rtm.test.echo

testecho() ->
	checkresponse(testecho_()).

testecho_() ->
        callapi([
                {"api_key", ?APIKEY},
                {"method", "rtm.test.echo"}
		]).

%%%    * rtm.test.login

testlogin() ->
	checkresponse(testlogin_()).

testlogin_() ->
        callapi([
                {"api_key", ?APIKEY},
                {"method", "rtm.test.login"}
		]).

%%%
%%% time
%%%

%%%    * rtm.time.convert
%%%	ISO 8601 format Time. default is now

timeconvert(FromTimezone, ToTimezone) ->
	checkresponse(timeconvert_(FromTimezone, ToTimezone)).
timeconvert_(FromTimezone, ToTimezone) ->
        callapi([
                {"api_key", ?APIKEY},
		{"from_timezone", FromTimezone},
		{"to_timezone", ToTimezone},
                {"method", "rtm.time.convert"}
		]).

timeconvert(FromTimezone, ToTimezone, Time) ->
	checkresponse(timeconvert_(FromTimezone, ToTimezone, Time)).

timeconvert_(FromTimezone, ToTimezone, Time) ->
        callapi([
                {"api_key", ?APIKEY},
		{"from_timezone", FromTimezone},
		{"to_timezone", ToTimezone},
		{"time", Time},
                {"method", "rtm.time.convert"}
		]).

%%%    * rtm.time.parse

timeparse(Text) ->
	checkresponse(timeparse_(Text)).
timeparse_(Text) ->
        callapi([
                {"api_key", ?APIKEY},
		{"text", Text},
                {"method", "rtm.time.parse"}
		]).

timeparse(Text, Timezone) ->
	checkresponse(timeparse_(Text, Timezone)).

timeparse_(Text, Timezone) ->
        callapi([
                {"api_key", ?APIKEY},
		{"text", Text},
		{"timezone", Timezone},
                {"method", "rtm.time.parse"}
		]).

timeparse(Text, Timezone, Dateformat) ->
	checkresponse(timeparse_(Text, Timezone, Dateformat)).

timeparse_(Text, Timezone, Dateformat) ->
        callapi([
                {"api_key", ?APIKEY},
		{"text", Text},
		{"timezone", Timezone},
		{"dateformat", Dateformat},
                {"method", "rtm.time.parse"}
		]).

%%%
%%% timelines
%%%

%%%    * rtm.timelines.create

timelinesCreate() ->
	checkresponse(timelinesCreate_()).

timelinesCreate_() ->
        callapi([
                {"api_key", ?APIKEY},
                {"method", "rtm.timelines.create"}
		]).

%%%
%%% timezones
%%%

%%%    * rtm.timezones.getList

timezonesgetList() ->
	checkresponse(timezonesgetList_()).

timezonesgetList_() ->
        callapi([
                {"api_key", ?APIKEY},
                {"method", "rtm.timezones.getList"}
		]).

%%%
%%% transactions
%%%

%%%    * rtm.transactions.undo

transactionsUndo(Timeline, TransactionsId) ->
	checkresponse(transactionsUndo_(Timeline, TransactionsId)).

transactionsUndo_(Timeline, TransactionsId) ->
        callapi([
                {"api_key", ?APIKEY},
		{"timeline", Timeline},
		{"transactions_id", TransactionsId},
                {"method", "rtm.transactions.undo"}
		]).
