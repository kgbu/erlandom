%%%	@author OGAKI, Kazutaka <ogakikz@jin.gr.jp>
%%%	@copyright please refer http://github.com/kgbu/erlandom/blob/master/README
%%%
%%%	@doc
%%%	salmon protocol related codes
%%%	ref: http://www.salmon-protocol.org/salmon-protocol-summary
%%%
%%%	other references
%%%	Atom: RFC4287
%%%	Id format: RFC3987 (IRI:Internationalized Resource Identifiers)
%%%	Updated format: RFC3339 (Date and Time on the Internet: Timestamps)
%%%
-module(salmon).

%	protocol usecases 
-export([ping/2, validate_ping/2, new_entry_atom/3, new_comment_atom/4]).

%	public utilities
-export([validate_atom/1, sign/1, sign/2, sign/3,
		 now_RFC3339_Z/0, localtime_to_RFC3339_Z/1]).

%	external library
%	uuid :	http://github.com/akreiling/erlang-uuid


%	includes
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("pshb.hrl").
-include_lib("salmon.hrl").


%%%
%%%	Protocol usecases
%%%

%	comment ping 
ping(Ref, CommentAtom) ->
	http:request(post, {Ref, [], [{"Content-type", ?SALMONTYPE}], CommentAtom}, [], [])
	.


validate_ping(_Source, _CommentAtom) ->
	true
	.

%	generate entry Atom
%

new_entry_atom(Author, Content, Id) ->
    {Root, _Misc} = xmerl_scan:string("<entry></entry>"),
    SimpleBase = [{author, [], [Author]},
                     {content,[], [Content]},
                     {id,[], [Id]}
                    ],
    NewBaseContent = lists:flatten([SimpleBase]),
    NewRoot = Root#xmlElement{content=NewBaseContent},
    xmerl:export_simple([NewRoot], xmerl_xml)
    .

%	generate comment Atom
%
new_comment_atom(Refer, Author, Content, Id) ->
	Updated = now_RFC3339_Z(),
    {Root, _Misc} = xmerl_scan:string("<entry xmlns='http://www.w3.org/2005/Atom'></entry>"),
    SimpleBase = [{author, [], [Author]},
                     {content,[], [Content]},
                     {id,[], [Id]},
					 {updated, [], [Updated]},
					 {"thr:in-reply-to", [
							{"xmlns:thr", "http://purl.org/syndication/thread/1.0"},
       						{ref, "tag:example.org,1999:id-22717401685551851865"}
							],[]},
					 {"sal:signature", [
						{"xmlns:sal","http://salmonprotocol.org/ns/1.0" }
						], ?MODULE:sign([?SECRET, Refer, Updated])}
                    ],
    NewBaseContent = lists:flatten([SimpleBase]),
    NewRoot = Root#xmlElement{content=NewBaseContent},
    BaseAtom = xmerl:export_simple([NewRoot], xmerl_xml),
    ?MODULE:sign(BaseAtom, ?SECRET)
	.



%%
%%	public utilities
%%

validate_atom(Atom) when is_binary(Atom) ->
	validate_atom(binary_to_list(Atom))
	;
validate_atom(Atom) when is_list(Atom) ->
%%
% Must maintain the following fields byte-for-byte identical to the originals:
%
%    * Contents of atom:author/uri element (a URI) - used for provenance
%    * Contents of the entry/atom:id element -- either retained as atom:id, or if not, recorded as xpost:source/id element (a URI) - used for de-duping
%    * Contents of atom:updated element (a timestamp)
%    * The thr:in-reply-to@ref attribute's content (a URI)
%    * Contents of sal:signature element (a base64 encoded text string)
%%%
%%%	FIXME : DTD required
%%%
	{Xml, _Rest} = xmerl_scan:string(Atom),
	{uri, Uri} = ?XpathV("//author/uri", Xml),
	{id, Id} = ?XpathV("//id", Xml),
	{updated, Updated} = ?XpathV("//updated", Xml),
	{ref, Refer} = ?XpathA("//thr:in-reply-to/@ref", Xml),
	Validate_list = [
	is_validuri(Uri),
	pshb_utils:is_validgid(Id),
	is_validtimestamp(Updated),
	is_validrefer(Refer),
	is_validsignature(?XpathV("//sal:signature", Xml))
	],
	{lists:all(fun(X) -> X =/= true end, Validate_list), Validate_list}
	;
validate_atom(_Atom) ->
	false
	.

is_validuri(Str) when is_binary(Str) ->
	is_validuri(binary_to_list(Str))
	;
is_validuri(Str) when is_list(Str) ->
	true
	;
is_validuri(_) ->
	false
	.

is_validrefer(Str) when is_binary(Str) ->
	is_validrefer(binary_to_list(Str))
	;
is_validrefer(Str) when is_list(Str) ->
	true
	;
is_validrefer(_) ->
	false
	.

is_validtimestamp(Str) when is_binary(Str) ->
	is_validtimestamp(binary_to_list(Str))
	;
is_validtimestamp(Str) when is_list(Str) ->
	true
	;
is_validtimestamp(_) ->
	false
	.

is_validsignature(Str) when is_binary(Str) ->
	List = binary_to_list(Str),
	if
		length(List) == 32 ->
			try base64:decode(Str) of
				_Val -> true
			catch
				error:_ -> false
			end;
		true ->
			false
	end
	;
is_validsignature(Str) when is_list(Str) ->
	is_validsignature(list_to_binary(Str))
	;
is_validsignature(_) ->
	false
	.

%%
%	signature
%

sign(Array) when is_list(Array) ->
	Str = lists:flatten(Array),
    base64:encode(erlang:md5(Str))
	;
sign(_) ->
	{error}
	.
sign(Atom, Secret) ->
	sign(Atom, Secret, fun(X) -> erlang:md5(X) end)
	.
	
sign(Xml, Secret, DigestFunc) when is_binary(Secret) ->
	sign(Xml, binary_to_list(Secret), DigestFunc)
	;
sign(Xml, Secret, DigestFunc) ->
	crypto_start(),
	{uri,Uri} = ?XpathV("//author/uri", Xml),
	{ref, Refer} = ?XpathA("//thr:in-reply-to/@ref", Xml),
    {updated, Updated} = ?XpathV("//updated", Xml),
	
	%% if any
	%% {Source} = ?XpathV("//xpost:source", Xml),

	ConcatinatedStr =
	Secret ++
	Uri ++
    %% if any
	%%  Source ++
	Refer ++
	Updated,
%debug io:format("~s~n",[ConcatinatedStr]),
	base64:encode(DigestFunc(ConcatinatedStr))
	.

%%%
%%	misc
%

crypto_start() ->
	try crypto:start()
	catch
		{error, {already_started,crypto}} -> true
	end
	.

%%%
%%	datetime string generator
%%	refer RFC3339 (for Atom timestamp)
%
now_RFC3339_Z() ->
	localtime_to_RFC3339_Z(erlang:localtime())
	.
	
localtime_to_RFC3339_Z(DateTime) ->
	{{Y, M, D}, {H, Min, S}} = erlang:localtime_to_universaltime(DateTime),
	[M2, D2, H2, Min2, S2] = lists:map(fun(X) -> to_2digits(X) end,
								[M,D,H,Min,S]),
	lists:flatten(io_lib:format("~p-~s-~sT~s:~s:~sZ",[Y,M2,D2, H2,Min2,S2]))
	.

to_2digits(N) ->
	lists:flatten([integer_to_list(N div 10), integer_to_list(N rem 10)])
	.
