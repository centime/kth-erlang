-module(node2).
-export([start/1, start/3, addEntry/4, getEntry/3]).

-define(Stabilize, 1000).
-define(Timeout, 3000).

start(Id) ->
    start(Id, nil,[]).

start(Id, Peer, Store) ->
    spawn(fun() -> init(Id, Peer, Store) end).

init(Id, Peer, Store) ->
    % io:format("****************~nNode (~p), peer :~p ~n",[Id, Peer]),
    Predecessor = nil,
    {ok, Successor} = connect(Id, Peer),
    schedule_stabilize(),
    node(Id, Predecessor, Successor, Store).

schedule_stabilize() ->
    timer:send_interval(?Stabilize, self(), stabilize).

connect(Id, nil) ->
    {ok, {Id, self()}};

connect(_, Peer) ->
    Qref = make_ref(),
    Peer ! {key, Qref, self()},
    receive
        {Qref, Skey} -> 
            {ok, {Skey,Peer}}
    after ?Timeout ->
        io:format("Time out: no response~n",[])
    end.


node(Id, Predecessor, Successor, Store) ->
    receive
        % A peer needs to know our key.
        {key, Qref, Peer} ->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor, Store);
        % A new node informs us of its existence
        {notify, New} ->
            {Pred, Keep} = notify(New, Id, Predecessor, Store),
            node(Id, Pred, Successor, Keep);
        % A predecessor needs to know our predecessor
        {request, Peer} ->
            request(Peer, Predecessor),
            node(Id, Predecessor, Successor, Store);
        % Our successor informs us about its predecessor
        {status, Pred} ->
            Succ = stabilize(Pred, Id, Successor),
            node(Id, Predecessor, Succ, Store);
        stabilize -> 
        %io:format("STABILI ~p~n",[{Id,self()}]),
            {_, Spid} = Successor,
            Spid ! {request, self()},
            node(Id, Predecessor, Successor, Store) ;
        % TODO
        % handlers to print out some state information, to terminate and a catch all clause in case some strange messages are sent.
        probe ->
            create_probe(Id, Successor, Store),
            node(Id, Predecessor, Successor, Store);
        {probe, Id, Nodes, T} ->
            remove_probe(T, Nodes),
            node(Id, Predecessor, Successor, Store);
        {probe, Ref, Nodes, T} ->
            forward_probe(Ref, T, Nodes, Id, Successor, Store),
            node(Id, Predecessor, Successor, Store) ;

        {add, Key, Value, Qref, Client} ->
            Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Added);

        {lookup, Key, Qref, Client} ->
            lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
            node(Id, Predecessor, Successor, Store);

        {handover, Elements} ->
            Merged = storage:merge(Elements, Store),
            node(Id, Predecessor, Successor, Merged)
    end.

% The Pred argument is ours successors current predecessor.
stabilize(Pred, Id, Successor) ->
    {Skey, Spid} = Successor,
    case Pred of
        % If this i nil we should of course inform it about our existence. 
        nil -> 
            Spid ! {notify, {Id, self()}},
            Successor;
        % If it is pointing back to us we don’t have to do anything. 
        {Id, _} -> 
            Successor;
        % If it is pointing to itself we should of course notify it about our existence.
        {Skey, _} -> 
            Spid ! {notify, {Id, self()}},
            Successor;
        % If it’s pointing to another node we need to be careful.
        {Xkey, Xpid} ->
            % The question is if we are to slide in between the two nodes or if we should place ourselves
            % behind the predecessor.  If we should be in between the nodes
            % we inform our successor of our existence.

            case key:between(Id, Xkey, Skey) of
                % If the key of the predecessor of our successor (Xkey)
                % is between us and our successor we should of course adopt this node as our
                % successor and run stabilization again.
                %% TODO
                false ->
                    Xpid ! {request, self()},
                    %io:format("~w : updates successor from ~w to ~w ~n",[Id, Skey, Xkey]),
                    Pred;
                true -> 
                    Spid ! {notify, {Id, self()}},
                    %o:format("~w : sending notify to ~w (pred : ~w )~n",[Id, Skey, Xkey]),
                    Successor
            end
    end.

request(Peer, Predecessor) ->
    case Predecessor of
        nil ->
            Peer ! {status, nil};
        {Pkey, Ppid} ->
            Peer ! {status, {Pkey, Ppid}}
    end.

% A way for a node to make a friendly proposal that it might be our proper predecessor. 
notify({Nkey, Npid}, Id, Predecessor, Store) ->

    case Predecessor of
        nil ->  
            Keep = handover(Store, Nkey, Npid),
            {{Nkey, Npid}, Keep};
        {Pkey, _} ->
            % We can not take their word for it, so we have to do our own investigation.
            case key:between(Nkey, Pkey, Id) of
                true -> 
                    Keep = handover(Store, Nkey, Npid),
                    {{Nkey, Npid}, Keep};
                false ->  
                    {Predecessor, Store}
            end
    end.



create_probe(Id, Successor, Store) ->
    {_, Spid} = Successor,
    Spid ! {probe, Id, [{Id, self(), Store}], erlang:now()}.

remove_probe(T, Nodes) ->
    io:format("Probe : ~n",[]),
    lists:foreach(
      fun({Key, _, Store}) -> 
              io:format("    Node ~w :~n", [Key]),
              io:format("        Store:~n",[]), 
              lists:foreach(
                fun({K,V}) ->
                    io:format("        [~w] -> [~w]~n", [K, V]) 
                end,
                Store)
      end,
      Nodes),
    io:format("Took ~w ms.~n",[timer:now_diff(erlang:now(), T)]).

forward_probe(Ref, T, Nodes, Id, Successor, Store) ->
    {_, Spid} = Successor,
    Spid ! {probe, Ref, [{Id, self(), Store}|Nodes], T}.

add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Client ! {Qref, ok} ,
            storage:add(Key, Value, Store);            
        false ->
            Spid ! {add, Key, Value, Qref, Client},
            Store
    end.

lookup(Key, Qref, Client, Id, {Pkey, _}, Successor, Store) ->
    case key:between(Key, Pkey, Id) of
        true ->
            Result = storage:lookup(Key, Store),
            Client ! {Qref, Result};
        false ->
            {_, Spid} = Successor,
            Spid ! {lookup, Key, Qref, Client} 
        end.

handover(Store, Nkey, Npid) ->
    {Leave, Keep} = storage:split(Nkey, Store),
    Npid ! {handover, Leave},
    Keep.

addEntry(Key, Value, NodePid, Client) ->
    Qref = make_ref(),
    NodePid ! {add, Key, Value, Qref, Client},
    Qref.

getEntry(Key, NodePid, Client) ->
    Qref = make_ref(),
    NodePid ! {lookup, Key, Qref, Client},
    Qref.