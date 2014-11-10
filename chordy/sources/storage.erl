-module(storage).
-export([create/0, add/3, lookup/2, split/2, merge/2]).

create() -> [].

add(Key, Value, L) ->
    lists:keystore(Key, 1, L, {Key, Value}).

lookup(Key, L) ->
    lists:keyfind(Key, 1, L).

split(Key, L) ->
    lists:partition(fun({K,_}) -> K =< Key end, L).

merge(L1, L2) ->
    lists:keymerge(1, L1, L2).


