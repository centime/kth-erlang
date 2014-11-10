-module(benchmark_parallel).
-export([start/1,start/2,start/3]).

start(Port) ->
    start("localhost", Port,750).

start(Host, Port) ->
    start(Host, Port,750).


start(Host, Port,N) ->
    P = self(),
    Receiver = spawn(fun()->receiver(N,P,0,N) end),
    Start = now(),
    run(N, Receiver,Host, Port),
    receive
        {all_done, Errors} -> ok
    end,
    Finish = now(),
    io:format("Errors : ~p/~p~n",[Errors,N]),
    timer:now_diff(Finish, Start).

receiver(N, Pid, Errors, Init) ->
    io:format("Errors : ~p/~p~n",[Errors,Init-N]),
    if N == 0 ->
        Pid ! {all_done, Errors};
    true ->
        receive
            done -> receiver(N-1,Pid,Errors,Init);
            error -> receiver(N-1,Pid, Errors+1,Init)
        end
    end.

run(N, Receiver,Host, Port) ->
    if
        N == 0 ->
            ok;
        true ->
            spawn(fun()->request(Receiver,Host, Port)end),
            run(N-1, Receiver,Host, Port)
    end.

request(Receiver,Host, Port) ->
    Opt = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:connect(Host, Port, Opt) of
        {ok, Server} ->
            gen_tcp:send(Server, http:get("foo")),
            Recv = gen_tcp:recv(Server, 0),
            case Recv of
                {ok, _} ->
                    Receiver ! done;
                {error, _} ->
                    Receiver ! error
            end;
        {error, _} -> 
            Receiver ! error
    end.