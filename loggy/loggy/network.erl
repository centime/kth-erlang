-module(network).
-export([start/2, start_improved/2, stop/1]).

start(Latency, Logger) ->
    spawn_link(fun() ->loop(Latency, Logger, 0, []) end).

loop(Latency, Logger, N_packets, Packets) ->
    % If we have no packet, just wait longer so we have at least one.
    if 
        N_packets == 0 -> Wait = 1000000000;
        true -> Wait = random:uniform(Latency)
    end,
    receive
        {log, Name, Time, Message} ->
            New_packets = [{Name,Time,Message}|Packets],
            loop(Latency, Logger, N_packets+1, New_packets);
        stop ->
            io:format("
                *****************************************~n
                Packets still in the network : ~p~n",[Packets]),
            ok
    % Or 
    after Wait ->
        if 
            N_packets > 0 ->
                Select = random:uniform(N_packets), % Okay, this shouldn't be uniform. But, meh !
                Packet = lists:nth(Select,Packets),
                {Name, Time, Message} = Packet,
                Logger ! {log, Name, Time, Message},
                % remove packet, need to use a lists: method
                New_packets = lists:filter(
                        fun(X) -> not (X==Packet) end,
                        Packets
                    ),
                %io:format("~p~n",[Packets==New_packets]),
                loop(Latency, Logger, N_packets-1, New_packets);
            true ->
                loop(Latency, Logger, N_packets, Packets)
        end
    end.



%%%%%%%%%% The same, with only a slight modification on the 'log' message signature : include a 'last time' field.


start_improved(Latency, Logger) ->
    spawn_link(fun() ->loop_improved(Latency, Logger, 0, []) end).

loop_improved(Latency, Logger, N_packets, Packets) ->
    % If we have no packet, just wait longer so we have at least one.
    if 
        N_packets == 0 -> Wait = 1000000000;
        true -> Wait = random:uniform(Latency)
    end,
    receive
        {log, Name, Time, Previous_time, Message} ->
            New_packets = [{Name,Time,Previous_time,Message}|Packets],
            loop_improved(Latency, Logger, N_packets+1, New_packets);
        stop ->
            io:format("
                *****************************************~n
                Packets still in the network : ~p~n",[Packets]),
            ok
    % Or 
    after Wait ->
        if 
            N_packets > 0 ->
                Select = random:uniform(N_packets),
                Packet = lists:nth(Select,Packets),
                {Name, Time, Previous_time, Message} = Packet,
                Logger ! {log, Name, Time, Previous_time, Message},
                % remove packet, need to use a lists: method
                New_packets = lists:filter(
                        fun(X) -> not (X==Packet) end,
                        Packets
                    ),
                loop_improved(Latency, Logger, N_packets-1, New_packets);
            true ->
                loop_improved(Latency, Logger, N_packets, Packets)
        end
    end.

stop(Network) ->
    Network ! stop.
