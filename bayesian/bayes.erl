-module(bayes).
-export([turn/3, testTurn/0]).

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
