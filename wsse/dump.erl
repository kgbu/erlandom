#!/usr/bin/env escript

main(_) -> 
  Latest = getatom:new("kgbu", "ab8b885c35Q", "http://b.hatena.ne.jp/dump"),
  {ok,FH} = file:open("hatebudump.dat", write),
  io:format(FH, "~s", [Latest]).
