-module(multibroker).
-behaviour(application).
-export([start/0, init/0]).



init(SizeN) -> 
    Status = pre_operation,
    Q = gen_queue(SizeN).

start() ->


stop() ->
    

queue() ->
    queue(0).

queue(Status) -> 
    N = Status + 1,
    queue(N).
