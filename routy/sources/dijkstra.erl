-module(dijkstra).
-export([entry/2, replace/4, update/4, iterate/3, table/2, route/2]). % Use when you want to run the tests.
%-export([table/2, route/2]).


% Returns the length of the shortest path to the node or 0 if the node is not found.
%%% Why return 0 when not found ? at what distance is a node from itself then ?  why not handle it properly ? 
entry(Node, Sorted)->
    case lists:keyfind(Node, 1, Sorted) of
        false -> 0;
        {_, Dist, _} -> Dist
    end.

% Replaces the entry for Node in Sorted with a new entry having a new length N and Gateway. 
% The resulting list should of course be sorted.
replace(Node, D, Gateway, Sorted)->
    lists:keymerge(2,[{Node, D, Gateway}],lists:keydelete(Node, 1, Sorted)). %TODO : node|list or list|node ?


% Update the list Sorted given the information that Node can be reached in N hops using Gateway.
% If no entry is found then no new entry is added.
% Only if we have a better (shorter) path should we replace the existing entry.
update(Node, D, Gateway, Sorted)->
    Known_distance = entry(Node, Sorted),
    if 
        Known_distance > D -> replace(Node, D, Gateway, Sorted) ;
        true -> Sorted
    end.

% Construct a table given a sorted list of nodes, a map and a table constructed so far.
iterate(Sorted, Map, Table) ->
    case Sorted of
        % If there are no more entries in the sorted list then we are done and the given routing table is complete
        [] -> Table ;
        % If the first entry is a dummy entry with an infinite path to a city we know that the rest of the sorted list is also of infite length and the given routing table is complet
        [{_,inf,_}|_] -> Table ;
        %Otherwise, take the first entry in the sorted list.
        [{Node,Dist,Gateway}|T] ->
            % Find the nodes in the map reachable from this entry and for each of these nodes update the Sorted list. 
            Sorted_ = lists:foldl(
                fun(N, S)->update(N, Dist+1, Gateway, S) end, % TO_CHECK
                T,
                map:reachable(Node, Map)
            ),
            % The entry that you took from the sorted list is added to the routing table
            iterate(Sorted_,Map,[{Node,Gateway}|Table])

    end.

% Construct a routing table given the gateways and a map.
table(Gateways, Map) ->
    % List the nodes of the map and construct a initial sorted list.
    % This list should have dummy entries for all nodes with the length set to infinity and the gateway to unknown.
    Non_gateway_nodes = lists:map(
        fun(N) -> {N,inf,unknown} end,
        lists:filter(
            fun(N) -> not lists:member(N,Gateways) end,
            map:all_nodes(Map)
        )
    ),
    % The entries of the gateways should have length zero and gateway set to itself.
    Gateway_nodes = lists:map(
        fun(N) -> {N,0,N} end,
        Gateways
    ),
    % When you have constructed this list you can call iterate with an empty table. 
    iterate(Gateway_nodes ++ Non_gateway_nodes, Map, []).

% Search the routing table and return the gateway suitable to route messages to a node. 
% If a gateway is found we should return {ok, Gateway} otherwise we return notfound.
route(Node, Table) ->
    case lists:keysearch(Node, 1, Table) of
        {value, {_,Gateway} } ->  {ok, Gateway};
        false -> notfound
    end.