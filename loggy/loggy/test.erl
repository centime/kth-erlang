-module(test).
-export([run/2, run/0, run_network/3, unit/0]).

unit()->
    % Compile the sources.
    SOURCES = [logger],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),

    test("logger:sublist_until/2",logger:sublist_until(elem,[first,second,last_of_sublist,elem,garbage])==[first,second,last_of_sublist]),
    test("logger:received_before_sent/1 (small)",logger:received_before_sent(
        [{ringo,2,{received,{hello,57}}},
         {john,1,{sending,{hello,00}}},
         {john,1,{received,{hello,00}}}
        ]) == [{57}]),
    test("logger:received_before_sent/1 (big)",logger:received_before_sent(
        [{ringo,2,{received,{hello,57}}},
         {john,1,{sending,{hello,57}}},
         {john,4,{received,{hello,77}}},
         {paul,1,{sending,{hello,68}}},
         {paul,6,{received,{hello,90}}},
         {ringo,3,{sending,{hello,77}}},
         {ringo,4,{received,{hello,68}}},
         {ringo,5,{received,{hello,58}}},
         {paul,7,{sending,{hello,40}}},
         {john,5,{sending,{hello,90}}},
         {john,8,{received,{hello,40}}},
         {george,1,{sending,{hello,58}}},
         {john,9,{received,{hello,42}}}]
         )==[{57},{77},{90},{58},{42}]),
    ok.

run()->
    run(3000,2000).
run(Sleep, Jitter) ->
    % Compile the sources.
    SOURCES = [logger, worker],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),
    unit(),
    % Run the test.
    Log = logger:start([john, paul, ringo, george]),
    A = worker:start(john, Log, 13, Sleep, Jitter),
    B = worker:start(paul, Log, 23, Sleep, Jitter),
    C = worker:start(ringo, Log, 36, Sleep, Jitter),
    D = worker:start(george, Log, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(5000),
    logger:stop(Log),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).

run_network(Sleep, Jitter, Latency) ->
     % Compile the sources.
    SOURCES = [logger, worker, network],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),
    unit(),
    % Run the test.
    Log = logger:start([john, paul, ringo, george]),
    Network = network:start(Latency,Log),
    A = worker:start(john, Network, 13, Sleep, Jitter),
    B = worker:start(paul, Network, 23, Sleep, Jitter),
    C = worker:start(ringo, Network, 36, Sleep, Jitter),
    D = worker:start(george, Network, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    %Log ! status,
    timer:sleep(5000),
    logger:stop(Log),
    network:stop(Network),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D).

test(Label,Expr)->
    case Expr of
        true ->
            io:format("~p : pass~n",[Label]);
        false ->
            io:format("~n                    ~p : FAIL~n~n",[Label])
    end.
