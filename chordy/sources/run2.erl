-module(run2).
-export([start/0, start/1, stop/1]).


start() ->
    start(4).

start(Nbnodes) ->
    Key = key:generate(),
    StarterPid = node2:start(Key, nil, []),
    start(Nbnodes-1,StarterPid, [{Key, StarterPid}]).

start(N,StarterPid, Nodes) when N > 0 ->
    Key = key:generate(),
    Pid = node2:start(Key,StarterPid, []),
    start(N-1, StarterPid, [ {Key,Pid}|Nodes ] );

start(_,_,Nodes) -> Nodes.

stop(Nodes) ->
    lists:foreach(
        fun({_, Pid}) -> Pid ! stop end,
        Nodes
    ).
