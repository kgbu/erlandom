%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%%	utilities
%%%
-module(pshb_utils).

-compile(export_all).

%%	Atom handling

-include_lib("xmerl/include/xmerl.hrl").

-define(Val(X),
(fun() ->
[#xmlElement{name = N, content = [#xmlText{value = V}|_]}] = X, {N,V} end)())
.

-define(Attrib(X),
(fun() ->
[#xmlAttribute{name = N, value = V}] = X, {N,V} end)())
.

%   scrape Atom feed of target content
scrape_from_atom(AtomUri) ->
    {ok, {_Status, _Headers, Body}} = http:request(AtomUri),
    TmpFile = mktemp(),
%FIXME: use try-catch, or Io for File
    file:write_file(TmpFile,Body),
    HubURL = getrelhub(TmpFile),
    SelfURL = getrelself(TmpFile),
    file:delete(TmpFile),
    {ok, {HubURL, SelfURL}}
    .

getrelhub(File) ->
	getrel(File, "//atom:feed/link/@href[../@rel=\"hub\"]")
	.

getrelself(File) ->
	getrel(File, "//atom:feed/link/@href[../@rel=\"self\"]")
	.

getrel(File, Path) when is_binary(Path) ->
	getrel(File, binary_to_list(Path))
	;
getrel(File, Path) ->
	Result = xmerl_scan:file(File),
	case Result of
		{Xml, _} ->
			Rel = try ?Attrib(xmerl_xpath:string(Path, Xml)) 
			catch
				throw:X -> {Xml, thrown, X};
				error:X -> {Xml, error, X};
				exit:X -> {Xml, exit, X}
			end,
			case Rel of
				{href, Y} -> {ok, Y};
				Other -> {error, Other}
			end;
		true -> Result
	end
	.


%%  pshb_utils:getrel("target.atom","/atom:feed/link/@href[../@rel=\"self\"]").
%%  rel : [{xmlAttribute,href,[],[],[],[],2,[],
%%	http://publisher.example.com/topic.xml",false}]


get_subscriberlist() ->
	[]
	.

%%% Atom handling
%%%
%%%
json2atom(Str) when is_binary(Str) ->
	json2atom(binary_to_list(Str))
	;
json2atom(Str) when is_list(Str) ->
	{ok, Str}
	;
json2atom(_Str) ->
	{error, badtypearg}
	.

atom_full_new (_FullContentsList) ->
	{error, fixme}
	.

atom_truncated_new(_TruncatedContentsList) ->
	{error, fixme}
	.

atom_notify_new(_NotificationList) ->
	%Atom = atom_container_new(),
	{error, fixme}
	.

atom_container_new(_LinkHub, _LinkSelf, _UpdatedTime) ->
	{error, fixme}
	.


%%%	Validators

is_absoluteurl(Url) when is_binary(Url) ->
	is_absoluteurl(binary_to_list(Url))
	;
is_absoluteurl(Url) ->
	io:format("~p~n",[Url]),
	Tokens = string:tokens(Url, "/"),
	Proto = string:tokens(Url, ":"),
	case Tokens of 
		["http:", _Hostname] ->
			case lists:reverse(Url) of
				['/'|_] -> is_http(Proto);
				true -> false
			end;
		["http:", _Hostname | _Path] -> is_http(Proto);
		true -> false
	end
	.

is_http(Proto) ->
	case Proto of 
		["http", Other] ->
			case Other of 
				["//"|_Remains] -> true;
				true -> false
			end;
		true -> false
	end
	.

is_validgid(Id) when is_binary(Id) ->
	is_validgid(binary_to_list(Id)) 
	;
is_validgid(Id) when is_list(Id)->
	true
	;
is_validgid(_Id) ->
	false
	.

is_validdatetime(DateTime) when is_binary(DateTime) ->
	is_validdatetime(binary_to_list(DateTime))
	;
is_validdatetime(DateTime) when is_list(DateTime) ->
	% FIXME
	true
	;
is_validdatetime(_DateTime) ->
	false
	.

is_fullcontentslist(Contents) ->
	{ok, Contents}
	.

is_truncatedlist(Truncated) ->
	{ok, Truncated}
	.
is_notificationlist(Notifications) ->
	{ok, Notifications}
	.


%%%
%%	misc.
%

mktemp() ->
	string:strip(os:cmd("mktemp"),both,$\n)
	.
	
writetmp(Str) ->
	File = ?MODULE:mktmp(),
	file:write_file(File, Str),
	{ok, File}
	.

proptupleval(Key, PropList) ->
	case proplists:get_value(Key, PropList) of
		undefined ->
			[];
		Value ->
			{Key, Value}
	end
	.
