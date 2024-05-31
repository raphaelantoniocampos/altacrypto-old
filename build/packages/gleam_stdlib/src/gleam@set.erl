-module(gleam@set).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/0, size/1, contains/2, delete/2, to_list/1, fold/3, filter/2, drop/2, take/2, intersection/2, difference/2, is_subset/2, is_disjoint/2, insert/2, from_list/1, union/2, symmetric_difference/2]).
-export_type([set/1]).

-opaque set(FCJ) :: {set, gleam@dict:dict(FCJ, list(nil))}.

-spec new() -> set(any()).
new() ->
    {set, gleam@dict:new()}.

-spec size(set(any())) -> integer().
size(Set) ->
    maps:size(erlang:element(2, Set)).

-spec contains(set(FCS), FCS) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@dict:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-spec delete(set(FCU), FCU) -> set(FCU).
delete(Set, Member) ->
    {set, gleam@dict:delete(erlang:element(2, Set), Member)}.

-spec to_list(set(FCX)) -> list(FCX).
to_list(Set) ->
    gleam@dict:keys(erlang:element(2, Set)).

-spec fold(set(FDD), FDF, fun((FDF, FDD) -> FDF)) -> FDF.
fold(Set, Initial, Reducer) ->
    gleam@dict:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-spec filter(set(FDG), fun((FDG) -> boolean())) -> set(FDG).
filter(Set, Predicate) ->
    {set,
        gleam@dict:filter(erlang:element(2, Set), fun(M, _) -> Predicate(M) end)}.

-spec drop(set(FDJ), list(FDJ)) -> set(FDJ).
drop(Set, Disallowed) ->
    gleam@list:fold(Disallowed, Set, fun delete/2).

-spec take(set(FDN), list(FDN)) -> set(FDN).
take(Set, Desired) ->
    {set, gleam@dict:take(erlang:element(2, Set), Desired)}.

-spec order(set(FDR), set(FDR)) -> {set(FDR), set(FDR)}.
order(First, Second) ->
    case maps:size(erlang:element(2, First)) > maps:size(
        erlang:element(2, Second)
    ) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-spec intersection(set(FEA), set(FEA)) -> set(FEA).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).

-spec difference(set(FEE), set(FEE)) -> set(FEE).
difference(First, Second) ->
    drop(First, to_list(Second)).

-spec is_subset(set(FEI), set(FEI)) -> boolean().
is_subset(First, Second) ->
    intersection(First, Second) =:= First.

-spec is_disjoint(set(FEL), set(FEL)) -> boolean().
is_disjoint(First, Second) ->
    intersection(First, Second) =:= new().

-spec insert(set(FCP), FCP) -> set(FCP).
insert(Set, Member) ->
    {set, gleam@dict:insert(erlang:element(2, Set), Member, [])}.

-spec from_list(list(FDA)) -> set(FDA).
from_list(Members) ->
    Dict = gleam@list:fold(
        Members,
        gleam@dict:new(),
        fun(M, K) -> gleam@dict:insert(M, K, []) end
    ),
    {set, Dict}.

-spec union(set(FDW), set(FDW)) -> set(FDW).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-spec symmetric_difference(set(FEO), set(FEO)) -> set(FEO).
symmetric_difference(First, Second) ->
    difference(union(First, Second), intersection(First, Second)).
