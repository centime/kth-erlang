-module(logger).
-export([start/1, stop/2, received_before_sent/1, sublist_until/2]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

% Type is here just so we can hook several loggers and distinguish between their output (like one without jitter nor networ).
stop(Logger, Type) ->
    Logger ! {stop,Type}.

init(Nodes) ->
    % Initialisation of the stage with dummy entries.
    Stage = lists:map(
            % For each node : {Node, Last_time, Pre_substage, Substage}.
            fun(X)-> {X, 0, [], []}end,
            Nodes
        ),
    loop(Stage,[]).

loop(Stage, Msgs) ->
    receive
        {log, From, Time, Previous_time, Msg} ->
            % Stage the new message.
            Updated_stage = stage_message({From, Time, Previous_time, Msg},Stage),
            % Process the stage to get the new stage and the unstaged messages
            {Unstaged_messages, New_stage} = process_stage(Updated_stage, []),
            % For each unstaged message, log it and prepend it to the list of already received messages.
            New_Msgs = lists:foldl(
                fun(X,M)-> log(X), [X|M] end, 
                Msgs, lists:reverse(Unstaged_messages) 
            ),
            % Re-loop with the updated stage, and list of received messages.
            loop(New_stage, New_Msgs);
        {stop, Type} ->
            % Print everything we want to know about how it went for our dear logger.
            % Type is here just so we can hook several loggers and distinguish between their output (like one without jitter nor networ).
            print(Msgs, Stage, Type)
    end.

stage_message({From, Time, Previous_time, Msg}, Stage) ->
    % The global stage is a structure with one entry per node:
    % [{Node, Last_time, Pre_substage, Substage}, ...]
    % The Pre_substage is there to allow reordering the messages from the node, using Last_time to know the time value of the last "un-pre-sub-staged".
    % In Substage we find a coherent list of the messages of the node, and we wait until we know enough about the other nodes 
    lists:map(
        % We loop through the different Nodes to pass the message to stage in the right substage/pre-substage.
        fun(X)-> case X of
                    % If it is the right node and the previous time matches..
                    {From, Previous_time, Pre_substage, Substage} -> 
                        % We look in the pre-substage if any message can now go to the substage
                        {Un_prestaged_messages, New_time, New_pre_substage} = un_prestage(Time, Pre_substage),
                        % We pass all of this to the substage.
                        {From, New_time, New_pre_substage, Substage++[{From,Time,Previous_time,Msg}]++Un_prestaged_messages};
                    % If it is the right node but the previous time doesn't matche, it goes to the pre-substage.
                    {From, Other_time, Pre_substage, Substage} ->
                        % We want to keep the pre_substage sorted.
                        {From, Other_time, lists:keymerge(2,[{From,Time,Previous_time,Msg}],Pre_substage), Substage};
                    % if it doesn't concern this node, pass.
                    Any -> Any
                end end,
        Stage
    ).

un_prestage(Time, Pre_substage) ->
    {M,T} = get_messages_from_pre_substage(Time, Pre_substage, []),
    S = lists:filter(
            fun(X) -> not lists:member(X,M) end,
            Pre_substage
        ),
    {M,T,S}.

% get all the messages from the substage, when we can construct between them the chain of logial time (using Time, Previous_time)
get_messages_from_pre_substage(Previous_time, Pre_substage, Un_prestaged_messages) ->
    case Pre_substage of
        [{From, Time, Previous_time, Msg}|T] -> get_messages_from_pre_substage(Time, T, Un_prestaged_messages++[{From, Time, Previous_time, Msg}]) ;
        % if we're at the end of the list, or the Previous_time doesn't match.
        %_-> {lists:keysort(2,Un_prestaged_messages),Previous_time}
        _-> {Un_prestaged_messages,Previous_time}
    end.

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
    Substages = lists:map(fun({_,_,_,L})-> L end, Stage),
    not lists:member([], Substages).

% Before calling this function, make sure none of the substages is empty
min_time_from_stage(Stage)->
    lists:min(lists:map(
        % Get the logical time of the oldest (first) message of the substage for each node.
        fun({_,_,_,[{_,T,_,_}|_]}) -> T end,
        Stage
    )).

unstage_messages(Time, Stage, Messages) ->
    M = get_messages_from_stage(Time, Stage, Messages),
    S = remove_messages_from_stage(M, Stage),
    {M,S}.

% Returns a list of the messages to unstage.
get_messages_from_stage(Time, Stage, Messages)->
    lists:foldl(
        fun({_,_,_,Substage},A)-> 
            % We look at the stage for Node.
            case Substage of
                % If it is empty, do nothing.
                []-> A;
                % We look at it's first (oldest) message.
                [First|_] -> 
                    {_,T,_,_} = First,
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
        fun({Node,Previous_time,Pre_substage,Substage})-> 
            {Node,Previous_time,Pre_substage,lists:filter(
                fun(X)-> not lists:member(X, Unstaged_messages) end,
                Substage
            )}
        end,
        Stage
        ).

%%% OUTPUT %%%

log({From, Time, Previous_time, Msg}) ->
    io:format("log: ~w ~w ~w ~p~n", [From, Time, Previous_time, Msg]).

print(Msgs, Stage, Type) ->
    % io:format("~n~p~nMsgs:~n~p~n
    %     Stage:~n~p
    %     Received before sent : ~p~n~n
    %     Logical order respected : ~p~n",[
    %         Type, 
    %         lists:reverse(Msgs),
    %         Stage,
    %         received_before_sent(lists:reverse(Msgs)),
    %         logical_order(lists:reverse(Msgs))
    %     ]).
    io:format("~n~p~nStage:~n~p~n~n
        Received before sent : ~p~n~n
        Logical order respected : ~p~n",[
            Type, 
            Stage,
            received_before_sent(lists:reverse(Msgs)),
            logical_order(lists:reverse(Msgs))
        ]).

%%% TESTS %%%

logical_order(Msgs) ->
    Msgs == lists:keysort(2,Msgs).

% Check that for every message, the logger receives the 'sent' event before the 'received' event.
received_before_sent(Msgs) ->
    % Get all the messages labeled as 'received'
    Received = lists:filter(fun({_,_,_,{Type,_}})-> Type==received end,Msgs),
    Was_sent_before = lists:map(
        fun(X)->
            {_,_,_,{_,{Message,Token}}} = X,
            case lists:keysearch({sending,{Message,Token}},4, sublist_until(X, Msgs)) of
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