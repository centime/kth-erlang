-module(key).
-export([generate/0,between/3]).

% The function generate/0 will simply return a random number from 1 to 1.000.000.000 (30-bits),
% Using a hash function such as SHA-1 would give us 160 bits and allow us to have ***human readable names*** 
%         Oh REALLY ?
generate() ->
    random:uniform(100000).

% From == To -> complete ring
between(_, From, From) ->
    true;
% From < To --> between is the inner set
between(Key, From, To) when From < To ->
    ((From < Key) and (Key < To));
% From >= To --> between is the outer set
between(Key, From, To) when From > To->
    ((From < Key) or (Key < To)).