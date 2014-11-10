-module(test).
-export([run/3, run/0, unit/0]).

unit()->
    % Compile the sources.
    SOURCES = [logger],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),

    test("logger:sublist_until/2",logger:sublist_until(elem,[first,second,last_of_sublist,elem,garbage])==[first,second,last_of_sublist]),
    test("logger:received_before_sent/1 (small)",logger:received_before_sent(
        [{ringo,2,0,{received,{hello,57}}},
         {john,1,0,{sending,{hello,00}}},
         {john,1,0,{received,{hello,00}}}
        ]) == [{57}]),
    test("logger:received_before_sent/1 (big)",logger:received_before_sent(
        [{ringo,2,0,{received,{hello,57}}},
         {john,1,0,{sending,{hello,57}}},
         {john,4,0,{received,{hello,77}}},
         {paul,1,0,{sending,{hello,68}}},
         {paul,6,0,{received,{hello,90}}},
         {ringo,3,0,{sending,{hello,77}}},
         {ringo,4,0,{received,{hello,68}}},
         {ringo,5,0,{received,{hello,58}}},
         {paul,7,0,{sending,{hello,40}}},
         {john,5,0,{sending,{hello,90}}},
         {john,8,0,{received,{hello,40}}},
         {george,1,0,{sending,{hello,58}}},
         {john,9,0,{received,{hello,42}}}]
         )==[{57},{77},{90},{58},{42}]),
    ok.

run()->
    run(3000,2000, 500).
run(Sleep, Jitter, Latency) ->
    % Compile the sources.
    SOURCES = [logger, worker, test_logger, network],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),
    unit(),
    % Run the test.
    Log = logger:start([john, paul, ringo, george]),
    Network = network:start(Latency,Log),
    Test_log = test_logger:start([john, paul, ringo, george]),
    A = worker:start(john, Network, Test_log, 13, Sleep, Jitter),
    B = worker:start(paul, Network, Test_log, 23, Sleep, Jitter),
    C = worker:start(ringo, Network, Test_log, 36, Sleep, Jitter),
    D = worker:start(george, Network, Test_log, 49, Sleep, Jitter),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    %Log ! status,
    timer:sleep(5000),
    logger:stop(Log,"Log"),
    test_logger:stop(Test_log,"Test_log"),
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
