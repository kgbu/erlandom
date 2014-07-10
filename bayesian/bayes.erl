-module(bayes).
-compile(export_all).

turn(List, [], Count) ->
  {ok, List, Count}; 
turn({L1, _}, [R|Rtail], Count) when R > 0 -> 
    Lok = L1 + (1.0 / (Count + 1)),
    Lng = 1 - L1,
    turn({Lok, Lng}, Rtail, Count + 1);
turn({_, L2}, [_|Rtail], Count) -> 
    Lng = L2 + (1.0 / (Count + 1)),
    Lok = 1 - L2,
    turn({Lok, Lng}, Rtail, Count + 1).

testTurn() ->
    turn({0.5, 0.5}, [0, 1, 0, 1, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1], 0). 

setone(0, Heads, [R|Rtail]) ->
    lists:flatten([Heads ++ [R + 1 | Rtail]]); 
setone(N, Heads, [R|Rtail]) ->
    setone(N - 1, Heads ++ [R], Rtail). 

listturn(List, [], Count) ->
    {ok, [X / Count  || X <- List ], Count};
listturn(List, [R|Rtail], Count) ->
    listturn(setone(R, [], List), Rtail, Count + 1).
    
testListTurn() ->
    listturn([1/3, 1/3, 1/3], [0, 1, 2, 1, 0, 1, 2, 1, 1, 0, 1, 2, 1, 1, 1], 0). 
