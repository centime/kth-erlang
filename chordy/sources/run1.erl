-module(run1).
-export([start/0, startStar/1, startLine/1, stop/1]).


start() ->
    startStar(5).

startStar(N) ->
    Spid = node1:start(key:generate(),nil),
    startStar(N-1,Spid).

startStar(N,Spid) when N > 0 ->
    node1:start(key:generate(),Spid),
    startStar(N-1,Spid);

startStar(_,Pid) -> Pid.
    
startLine(N) ->
    Spid = node1:start(key:generate(),nil),
    startLine(N-1,Spid).

startLine(N,PreviousPid) when N > 0 ->
    Pid = node1:start(key:generate(),PreviousPid),
    startLine(N-1,Pid);

startLine(_,Pid) -> Pid.

stop(Nodes) ->
    lists:foreach(
        fun(Node) -> Node ! stop end,
        Nodes
    ).