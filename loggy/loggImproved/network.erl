-module(network).
-export([start/2, stop/1]).

start(Latency, Logger) ->
    spawn_link(fun() ->loop(Latency, Logger, 0, []) end).

loop(Latency, Logger, N_packets, Packets) ->
    %io:format("NETWK : ~p~n~n~n",[Packets]),
    % If we have no packet, just wait longer so we have at least one.
    if 
        N_packets == 0 -> Wait = 1000000000;
        true -> Wait = random:uniform(Latency)
    end,
    receive
        {log, Name, Time, Previous_time, Message} ->
            %io:format("NETWK RCV~n",[]),
            New_packets = [{Name,Time,Previous_time,Message}|Packets],
            %io:format("NETWK RCV : ~p~n",[{Name,Time,Message}]),
            loop(Latency, Logger, N_packets+1, New_packets);
        stop ->
            io:format("PACKETS STUCK IN NETWORK : ~p~n",[Packets]),
            ok
    % Or 
    after Wait ->
        %io:format("NETWK SND ? : ~p~n",[N_packets]),
        if 
            N_packets > 0 ->
                Select = random:uniform(N_packets),
                Packet = lists:nth(Select,Packets),
                {Name, Time, Previous_time, Message} = Packet,
                Logger ! {log, Name, Time, Previous_time, Message},
                %io:format("NETWK SND : ~p~n",[Packet]),
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

stop(Network) ->
    Network ! stop.
