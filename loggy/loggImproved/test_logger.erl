-module(test_logger).
-export([start/1, stop/2, received_before_sent/1, sublist_until/2]).

start(Nodes) ->
    spawn_link(fun() ->init(Nodes) end).

stop(Logger, Type) ->
    Logger ! {stop,Type}.

init(_) ->
    loop([]).

loop(Msgs) ->
    receive
        {log, From, Time, Msg} ->
            %log(From, Time, Msg),
            loop([{From,Time,Msg}|Msgs]);
        {stop, Type} ->
            % io:format("~n~p~nReal:~n~p
            %     Real time order errors : ~p~nLogical:~n~p
            %     Logical time order errors : ~p~n~n~n",[
            %         Type, 
            %         lists:reverse(Msgs), 
            %         received_before_sent(lists:reverse(Msgs)),
            %         lists:keysort(2,Msgs),
            %         received_before_sent(lists:keysort(2,Msgs))
            %     ]);
            ok;
        {status, From} ->
            if not Msgs==[] ->
                [Last|_] = Msgs,
                From ! [{last,Last},{received_before_sent,received_before_sent(lists:reverse(Msgs))}];
            true ->
                ok
            end,
            loop(Msgs) 
    end.

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