-module(worker).
-export([start/5, stop/1, peers/2]).


stop(Worker) ->
    Worker ! stop.

% To actually start the worker, we need to also call the peers/2 method.
start(Name, Logger, Seed, Sleep, Jitter) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

peers(Wrk, Peers) ->
    Wrk ! {peers, Peers}.

init(Name, Log, Seed, Sleep, Jitter) ->
    random:seed(Seed, Seed, Seed),
    receive
        {peers, Peers} ->
            loop(Name, Log, Peers, Sleep, Jitter, 0);
        stop ->
            ok
    end.

% Worker's main loop.
loop(Name, Log, Peers, Sleep, Jitter, Local_time)->
    % Random winting between messages.
    Wait = random:uniform(Sleep),
    % Either receive a message from a peer...
    receive
        {msg, Remote_time, Msg} ->
            Time = erlang:max(Remote_time,Local_time)+1,
            % And tell the logger about it.
            Log ! {log, Name, Time, {received, Msg}},
            loop(Name, Log, Peers, Sleep, Jitter, Time);
        stop ->
            ok;
        Error ->
            Log ! {log, Name, Local_time, {error, Error}}
    % Or send a message to a peer.
    after Wait ->
        Selected = select(Peers),
        Time = Local_time+1,
        Message = {hello, random:uniform(100000000000)},
        Selected ! {msg, Time, Message},
        % And tell the logger about it
        % After waiting a Jitter, so the receiver potentially will inform the logger
        % of the message transmission before the actual sender.
        
        jitter(Jitter),
        Log ! {log, Name, Time, {sending, Message}},
        loop(Name, Log, Peers, Sleep, Jitter, Time)
    end.

select(Peers) ->
    lists:nth(random:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(random:uniform(Jitter)).
