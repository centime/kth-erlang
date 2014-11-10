-module(tests).
-export([run/0]).

run()->

    % Compile the sources.
    SOURCES = [map, dijkstra, intf, hist],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),

    % Map.
    test("map:new",map:new()==[]),
    test("map:update",map:update(berlin, [london, paris], [])==[{berlin,[london,paris]}]),
    test("map:reachable",map:reachable(berlin, [{berlin,[london,paris]}])==[london,paris]),
    test("map:reachable2",map:reachable(london, [{berlin,[london,paris]}])==[]),
    test("map:all_nodes",map:all_nodes([{berlin,[london,paris]}])==lists:usort([paris,london,berlin])),
    test("map:update2",map:update(berlin, [madrid], [{berlin,[london,paris]}]) == [{berlin, [madrid]}]),
    
    % Djikstra.
    test("dijkstra:entry absent",dijkstra:entry(berlin,[{z,1,e},{p,2,u}]) == 0),
    test("dijkstra:entry here",dijkstra:entry(berlin,[{z,1,e},{berlin,2,z},{p,2,u}]) == 2),
    test("dijkstra:replace",dijkstra:replace(berlin,7,z,[{berlin,2,z},{p,3,u}])==[{p,3,u},{berlin,7,z}]),
    test("dijkstra:update absent",dijkstra:update(london, 2, amsterdam, [])==[]),
    test("dijkstra:update too long",dijkstra:update(london, 2, amsterdam, [{london, 2, paris}])==[{london,2,paris}]),
    test("dijkstra:update valid",dijkstra:update(london, 1, stockholm,[{berlin, 2, paris}, {london, 3, paris}])==[{london,1,stockholm}, {berlin, 2, paris}]),
    test("dijkstra:iterate",dijkstra:iterate([{paris, 0, paris}, {berlin, inf, unknown}],[{paris, [berlin]}], [])==lists:usort([{paris, paris},{berlin,paris}])),
    test("dijkstra:iterate advanced",dijkstra:iterate([{paris, 0, paris},{barcelona,0,barcelona}, {berlin, inf, unknown},{rome, inf, unknown},{stockholm, inf, unknown}],[{paris, [berlin,rome]},{rome,[stockholm]},{barcelona,[stockholm]}], []) ==
        [{berlin,paris},
         {rome,paris},
         {stockholm,barcelona},
         {barcelona,barcelona},
         {paris,paris}]
    ),
    test("dijkstra:table",lists:usort(dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]))==lists:usort([{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}])),
    test("dijkstra:route",dijkstra:route(rome,dijkstra:table([paris, madrid], [{madrid,[berlin]}, {paris, [rome,madrid]}]))=={ok,paris}),

    % intf.
    test("intf:new",intf:new()==[]),
    test("intf:add",intf:add(n1,r1,p1,[{n0,r0,p0}])==[{n1,r1,p1},{n0,r0,p0}]),
    test("intf:remove",intf:remove(n0,[{n1,r1,p1},{n0,r0,p0}])==[{n1,r1,p1}]),
    test("intf:lookup",intf:lookup(n1,[{n1,r1,p1},{n0,r0,p0}])=={ok,p1}),
    test("intf:ref",intf:ref(n1,[{n1,r1,p1},{n0,r0,p0}])=={ok,r1}),
    test("intf:name",intf:name(r0,[{n1,r1,p1},{n0,r0,p0}])=={ok,n0}),
    test("intf:list",lists:sort(intf:list([{n1,r1,p1},{n0,r0,p0}]))==lists:sort([n1,n0])),
    test("intf:broadcast : TEST NOT IMPLEMENTED.",true),

    % hist.
    test("hist:new",hist:new(self)==[{self,inf}]),
    test("hist:update new node",hist:update(n2, 20, [{n1,10}])=={new,[{n2,20},{n1,10}]}),
    test("hist:update new message, known node",hist:update(n2, 30, [{n2,20},{n1,10}])=={new,[{n2,30},{n1,10}]}),
    test("hist:update old message, known node",hist:update(n2, 10, [{n2,20},{n1,10}])==old),

    ok.

test(Label,Expr)->
    case Expr of
        true ->
            io:format("~p : pass~n",[Label]);
        false ->
            io:format("~n                    ~p : FAIL~n~n",[Label])
    end.
