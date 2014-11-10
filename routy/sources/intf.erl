-module(intf).
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).

% Return an empty set of interfaces.
new() ->
    [].

% Add a new entry to the set and return the new set of interfaces.
add(Name, Ref, Pid, Intf) ->
    [{Name,Ref,Pid}|Intf].

% Remove an entry given a name of an interface, return a new set of interfaces.
remove(Name, Intf) ->
    lists:keydelete(Name,1,Intf).

% Find the process identifier given a name, return {ok, Pid} if found otherwise notfound.
lookup(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false -> notfound;
        {_, _, Pid} -> {ok, Pid}
    end.

% Find the reference given a name and return {ok,Ref} or notfound
ref(Name, Intf) ->
    case lists:keyfind(Name, 1, Intf) of
        false -> notfound;
        {_, Ref, _} -> {ok, Ref}
    end.    

% Find the name of an entry given a reference and return {ok, Name} or notfound.
name(Ref, Intf) ->
    case lists:keyfind(Ref, 2, Intf) of
        false -> notfound;
        {Name, _, _} -> {ok, Name}
    end.        

% Return a list with all names.
list(Intf) ->
    lists:foldl(fun({Name,_,_},A)-> [Name|A] end, [], Intf).

% Send the message to all interface processes.
broadcast(Message, Intf)->
    lists:foreach(fun({_,_,Pid})->Pid ! Message end,Intf).
