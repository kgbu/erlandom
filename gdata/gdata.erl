%%%	convert XML to JSON like tuple in gdata flavor
%%%	FIXME! not completed
%%%	ogaki@keynauts.com (kgbu)

-module(gdata).
-export([file/1]).

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

eventfun({ignorableWhitespace, _},
		_,
		State) ->
	State
	;

eventfun({startElement, _A, Tag, _B, []}, 
		_Location,
		StackFrame) ->
	[{Tag, []} | StackFrame]
	;

eventfun({startElement, _T1, Tag, {_T3, _T4}, Attribs},
		_Location,
		[]) ->
	[{Tag, lists:map(fun accattribs/1,Attribs)}]
	;

eventfun({startElement, _T1, Tag, {_T3, _T4}, Attribs},
		_Location,
		[{PTag, PTmpAcc}| SFRemains]) ->
	[{PTag, [{Tag, lists:map(fun accattribs/1,Attribs)}|PTmpAcc]} | SFRemains]
	;

eventfun({characters, Value},
		_Location,
		[{Tag, TmpAcc}|SFRemains]) ->
	[{Tag, [{text, Value}|TmpAcc]}|SFRemains]
	;

eventfun({endElement, _A, _B, _C},
		_Location,
		[{Tag, TmpAcc}]) ->
	[{Tag, TmpAcc}]
	;

eventfun({endElement, _A, _B, _C},
		_Location,
		[{Tag, TmpAcc}, {PTag, PTmpAcc} |SFRemains]) ->
	[{PTag, lists:reverse([{Tag, TmpAcc} | PTmpAcc])} | SFRemains]
	;

eventfun(_Ev,_Loc,State) ->
	State
	.


accattribs({_,_,Name, Value}) ->
	{Name, Value}
	.

