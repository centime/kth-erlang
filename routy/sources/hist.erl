-module(hist).
-export([new/1,update/3]).

% Return a new history, where messages from Name will always be seen as old.
new(Name) ->
    [{Name,inf}].

% Check if message number N from the Node is old or new. 
% If it is old then return old but if it new return {new, Updated} where Updated is the updated history
update(Node, N, History) ->
    case lists:keyfind(Node, 1, History) of
        % If the node has never been seen yet, the message is new.
        false -> {new, [{Node,N}|History]};
        {_, Count} -> 
            if Count < N -> {new, [{Node,N}|lists:keydelete(Node,1,History)]};
                true -> old
            end
    end.