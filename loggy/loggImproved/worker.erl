-module(worker).
-export([start/6, stop/1, peers/2]).


stop(Worker) ->
    Worker ! stop.

% To actually start the worker, we need to also call the peers/2 method.
start(Name, Logger, No_jitter_logger, Seed, Sleep, Jitter) ->
io:format("~p~n",[Name]),
    spawn_link(fun() -> init(Name, Logger, No_jitter_logger, Seed, Sleep, Jitter) end).

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

init(Name, Log, No_jitter_log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, No_jitter_log, Peers, Sleep, Jitter, 0);
        stop ->
            ok
    end.

% Worker's main loop.
loop(Name, Log, No_jitter_log, Peers, Sleep, Jitter, Local_time)->
    % Random winting between messages.
    Wait = random:uniform(Sleep),
    % Either receive a message from a peer...
    receive
        {msg, Remote_time, Msg} ->
            Time = erlang:max(Remote_time,Local_time)+1,
            Previous_time = Local_time,
            % And tell the logger about it.
            Log ! {log, Name, Time, Previous_time, {received, Msg}},
            No_jitter_log ! {log, Name, Time, Previous_time, {received, Msg}},
            loop(Name, Log, No_jitter_log, Peers, Sleep, Jitter, Time);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, Local_time, {error, Error}},
            No_jitter_log ! {log, Name, Local_time, Local_time, {error, Error}}
    % Or send a message to a peer.
    after Wait ->
        Selected = select(Peers),
        Time = Local_time+1,
        Previous_time = Local_time,
        Message = {hello, random:uniform(100000000000)},
        Selected ! {msg, Time, Message},
        % And tell the logger about it
        % After waiting a Jitter, so the receiver potentially will inform the logger
        % of the message transmission before the actual sender.
        
        No_jitter_log ! {log, Name, Time, Previous_time, {sending, Message}},
        jitter(Jitter),
        Log ! {log, Name, Time, Previous_time, {sending, Message}},
        loop(Name, Log, No_jitter_log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
