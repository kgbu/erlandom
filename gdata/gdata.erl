%%%	convert XML to JSON like tuple in gdata flavor
%%%	FIXME! not completed
%%%	on to_tuple
%%%	on UTF8 handling (or, just use mochijson2)
%%%
%%%	ogaki@keynauts.com (kgbu)

-module(gdata).
-export([file/1, to_json/1, to_tuple/1]).

file(File) ->
	Option = [
		{event_fun, fun eventfun/3},
		{event_state, _StackFrame = []}
		],
	case xmerl_sax_parser:file(File, Option) of
		{ok,{StackFrame}} -> StackFrame;
		{ok, Bin} -> Bin;
		{Other} -> Other
	end
	.

%
%	parser callback
%
eventfun({ignorableWhitespace, _},
		_,
		State) ->
	State
	;

eventfun({startElement, _A, Tag, _B, []}, 
		_Location,
		StackFrame) ->
	[{asbin(Tag), []} | StackFrame]
	;

eventfun({startElement, _T1, Tag, {_T3, _T4}, Attribs},
		_Location,
		[]) ->
	[{asbin(Tag), lists:map(fun accattribs/1,Attribs)}]
	;

eventfun({startElement, _T1, Tag, {_T3, _T4}, Attribs},
		_Location,
		[{PTag, PTmpAcc}| SFRemains]) ->
	[{asbin(PTag),
		[{asbin(Tag), lists:map(fun accattribs/1,Attribs)}|PTmpAcc]} | SFRemains]
	;

eventfun({characters, BinValue},
		_Location,
		[{Tag, TmpAcc}|SFRemains]) when is_binary(BinValue)->
	eventfun({characters, binary_to_list(BinValue)},
			_Location,
		[{Tag, TmpAcc}|SFRemains]) 
	;

eventfun({characters, Value},
		_Location,
		[{Tag, TmpAcc}|SFRemains]) ->
	[{asbin(Tag), [{text, asbin(Value)}|TmpAcc]}|SFRemains]
	;

eventfun({endElement, _A, _B, _C},
		_Location,
		[{Tag, TmpAcc}]) ->
	[{Tag, reducelonearray(TmpAcc)}]
	;

eventfun({endElement, _A, _B, _C},
		_Location,
		[{Tag, TmpAcc}, {PTag, PTmpAcc} |SFRemains]) ->
	[{PTag, [{Tag, reducelonearray(TmpAcc)} | PTmpAcc]} | SFRemains]
	;

eventfun(_Ev,_Loc,State) ->
	State
	.

accattribs({_,_,Name, Value}) ->
	{asbin(Name), asbin(Value)}
	.

%
%	JSON encoding traverser
%
to_json([Data]) ->
	to_json(Data, [])
	.

to_json([], Acc) ->
	lists:reverse(Acc)
	;
to_json({LeafLabel, BinStr}, Acc) when is_binary(BinStr) ->
	[ asbin(io_lib:format("\"~s\": \"~s\"",[LeafLabel, BinStr])) | Acc ]
	;
to_json({NodeLabel, {LeafLabel, BinStr}}, Acc) ->
	[ asbin(io_lib:format("~s:",[NodeLabel])) , to_json([{LeafLabel, BinStr}]) | Acc]
	;
to_json({NodeLabel, Children}, Acc) ->
	[ asbin(io_lib:format("~s:",[NodeLabel])) , lists:map(fun(H) -> to_json(H,[]) end, Children) | Acc]
	;
to_json(A, Acc) ->
	io:format("data:~n~p~nacc~p~n", [A, Acc])
	.

%
%	utilities
%
asbin(Str) when is_binary(Str) ->
	Str
	;
asbin(Str) when is_list(Str) ->
	list_to_binary(Str)
	;
asbin(Str) ->
	Str
	.

reducelonearray([H|[]]) ->
	H
	;
reducelonearray(A) ->
	lists:reverse(A)
	.

to_tuple(L) when is_list(L) ->
	list_to_tuple(lists:map(fun(X) -> to_tuple(X) end, L))
	;
to_tuple(Any) ->
	Any
	.
