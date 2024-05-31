-module(gleam@pair).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first/1, second/1, swap/1, map_first/2, map_second/2, new/2]).

-spec first({YN, any()}) -> YN.
first(Pair) ->
    {A, _} = Pair,
    A.

-spec second({any(), YQ}) -> YQ.
second(Pair) ->
    {_, A} = Pair,
    A.

-spec swap({YR, YS}) -> {YS, YR}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({YT, YU}, fun((YT) -> YV)) -> {YV, YU}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({YW, YX}, fun((YX) -> YY)) -> {YW, YY}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.

-spec new(YZ, AAA) -> {YZ, AAA}.
new(First, Second) ->
    {First, Second}.
