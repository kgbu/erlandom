%%
%% LogFormat "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"" combined
%% LogFormat "%h %l %u %t \"%r\" %>s %b" common

-module(logreader).
-compile(export_all).

new(Pid, File, Host)->
	{ok, IO} = file:open(File, read),
	Pid ! {done, loop(Host, IO, [])}.
	
loop(Host, IO, Acc)->
	case io:get_line(IO, '') of
	eof -> Acc;
	Line -> 
		case lists:nth(6, string:tokens(Line, " \"")) of
			"GET" ->
				URL = "http://" ++ Host ++ lists:nth(7, string:tokens(Line, " \"")),
				loop(Host, IO,  [{URL}] ++  Acc );
			"POST"  ->
				io:format("POST record dropped~n", []),
				loop(Host, IO, Acc);
			_A ->
				io:format("unmatch record dropped~n", []),
				loop(Host, IO, Acc)
		end
	end.
