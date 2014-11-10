-module(logger).
-export([start/1, stop/1, received_before_sent/1, sublist_until/2]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes) ->
    % Initialisation of the stage ith dummy entries.
    Stage = lists:map(
            fun(X)-> {X, []}end,
            Nodes
        ),
    loop(Stage,[]).

loop(Stage,Msgs) ->
    receive
        {log, From, Time, Msg} ->
            % Stage the new message.
            Updated_stage = stage_message({From, Time, Msg},Stage),
            % Process the stage to get the new stage and the unstaged messages
            {Unstaged_messages, New_stage} = process_stage(Updated_stage, []),
            % For each unstaged message, log it and prepend it to the list of already received messages.
            New_Msgs = lists:foldl(
                fun(X,M)-> log(X), [X|M] end, 
                Msgs, lists:reverse(Unstaged_messages) 
            ),
            loop(New_stage, New_Msgs);
        stop ->
            Msgs_r = lists:reverse(Msgs),
            io:format("~n
                **********************************************~n
                Still staged :~n~n~p~n~n
                **********************************************~n
                Messages received before sent (if any) :~p~n
                Are messages logically sorted ? : ~p~n~n~n",[
                    Stage,
                    received_before_sent(Msgs_r),
                    logical_order(Msgs_r)
                ])
    end.

stage_message({From, Time, Msg}, Stage) ->
    lists:map(
        fun(X)-> case X of
                    % Append the message in the stage of it's node.
                    {From, Substage} -> {From, Substage++[{From,Time,Msg}]};
                    Any -> Any
                end end,
        Stage
    ).

process_stage(Stage, Messages) ->
    % If we have at least one message staged for every node.
    None_empty = no_substage_empty(Stage),
    if None_empty ->
        % Get the time of the oldest message we have staged.
        Min_time = min_time_from_stage(Stage),
        % Unstage the messages from this time.
        {New_messages, New_stage} = unstage_messages(Min_time, Stage, Messages),
        % Repeat until one of the stages is empty.
        process_stage(New_stage, New_messages);
    true ->
        {Messages, Stage}
    end.

no_substage_empty(Stage) ->
    Substages = lists:map(fun({_,L})-> L end, Stage),
    not lists:member([], Substages).

% Before calling this function, make sure none of the substages is empty
min_time_from_stage(Stage)->
    lists:min(lists:map(
        % Get the logical time of the oldest (first) message of the substage for each node.
        fun({_,[{_,T,_}|_]}) -> T end,
        Stage
    )).

unstage_messages(Time, Stage, Messages) ->
    M = get_messages_from_stage(Time, Stage, Messages),
    S = remove_messages_from_stage(M, Stage),
    {M,S}.

% Returns a list of the messages to unstage.
get_messages_from_stage(Time, Stage, Messages)->
    lists:foldl(
        fun({_,Substage},A)-> 
            % We look at the stage for Node.
            case Substage of
                % If it is empty, do nothing.
                []-> A;
                % We look at it's first (oldest) message.
                [First|_] -> 
                    {_,T,_} = First,
                    % If it from the time in which the logger is currently in, we unstage it
                    if T==Time -> [First|A];
                    % Else, we do nothing.
                    true -> A
                    end
            end
        end,
        Messages,Stage
        ).

% Removes from the substages the message which are to be unstaged. 
remove_messages_from_stage(Unstaged_messages, Stage) ->
    lists:map(
        fun({Node,Substage})-> 
            {Node,lists:filter(
                fun(X)-> not lists:member(X, Unstaged_messages) end,
                Substage
            )}
        end,
        Stage
        ).

log({From, Time, Msg}) ->
    io:format("log: ~w ~w ~p~n", [From, Time, Msg]).

% Check that for every message, the logger receives the 'sent' event before the 'received' event.
received_before_sent(Msgs) ->
    % Get all the messages labeled as 'received'
    Received = lists:filter(fun({_,_,{Type,_}})-> Type==received end,Msgs),
    Was_sent_before = lists:map(
        fun(X)->
            {_,_,{_,{Message,Token}}} = X,
            case lists:keysearch({sending,{Message,Token}},3, sublist_until(X, Msgs)) of
                % Why the curly brackets ? Because fucking [57,77,90,58] -> "9MZ:" !!!!
                % So I have to go with [{57},{77},{90},{58}]...
                false -> {Token};
                _ -> true
            end
        end,
        Received),
    lists:filter(fun(X)-> not (X==true) end, Was_sent_before).
    
% Return a sublist of List, from start to Elem.
sublist_until(Elem, List) ->
    % string:str/2 is used as a quick and dirty hack for an indexOf-like function.
    % /!\ starts at index 1.
    Index = string:str(List, [Elem]),
    {Previous_elems,_} = lists:split(Index-1,List),
    Previous_elems.

% Check the logical order of the logged messages.
% Returns a boolean.
logical_order(Msgs) ->
    Msgs == lists:keysort(2,Msgs).
