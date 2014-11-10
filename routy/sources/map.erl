-module(map).
-export([new/0, update/3, reachable/2, all_nodes/1]).

new()->
    [].

% Updates the Map to reflect that Node has directional links to all nodes in the list Links. 
% The old entry is removed.
update(Node, Links, Map) ->
    Map_ = lists:keydelete(Node, 1, Map),
    [{Node, Links} | Map_ ].

% Returns the list of nodes directly reachable from Node.
reachable(Node, Map) ->
    case lists:keysearch(Node, 1, Map) of
        {value, {_,Links} } ->  Links;
        false -> []
    end.


% Returns a list of all nodes in the map, also the ones without outgoing links. 
% So if berlin is linked to london but london does not have any outgoing links (and thus no entry in the list), 
% berlin should still be in the returned list.

all_nodes(Map) ->
    lists:usort(lists:foldl(fun({Node,Links},A)-> [Node|Links++A] end, [], Map)).