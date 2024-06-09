-module(gleam@result).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, 'try'/2, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, partition/1, replace/2, replace_error/2, values/1, try_recover/2]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _} ->
            false;

        {ok, _} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _} ->
            false;

        {error, _} ->
            true
    end.

-spec map({ok, BMR} | {error, BMS}, fun((BMR) -> BMV)) -> {ok, BMV} |
    {error, BMS}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BMY} | {error, BMZ}, fun((BMZ) -> BNC)) -> {ok, BMY} |
    {error, BNC}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BNF} | {error, BNG}} | {error, BNG}) -> {ok, BNF} |
    {error, BNG}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec 'try'({ok, BNN} | {error, BNO}, fun((BNN) -> {ok, BNR} | {error, BNO})) -> {ok,
        BNR} |
    {error, BNO}.
'try'(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec then({ok, BNW} | {error, BNX}, fun((BNW) -> {ok, BOA} | {error, BNX})) -> {ok,
        BOA} |
    {error, BNX}.
then(Result, Fun) ->
    'try'(Result, Fun).

-spec unwrap({ok, BOF} | {error, any()}, BOF) -> BOF.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default
    end.

-spec lazy_unwrap({ok, BOJ} | {error, any()}, fun(() -> BOJ)) -> BOJ.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BOO}, BOO) -> BOO.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BOR} | {error, BOR}) -> BOR.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BOU} | {error, any()}) -> {ok, BOU} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, BPA} | {error, BPB}, {ok, BPA} | {error, BPB}) -> {ok, BPA} |
    {error, BPB}.
'or'(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second
    end.

-spec lazy_or({ok, BPI} | {error, BPJ}, fun(() -> {ok, BPI} | {error, BPJ})) -> {ok,
        BPI} |
    {error, BPJ}.
lazy_or(First, Second) ->
    case First of
        {ok, _} ->
            First;

        {error, _} ->
            Second()
    end.

-spec all(list({ok, BPQ} | {error, BPR})) -> {ok, list(BPQ)} | {error, BPR}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec do_partition(list({ok, BQF} | {error, BQG}), list(BQF), list(BQG)) -> {list(BQF),
    list(BQG)}.
do_partition(Results, Oks, Errors) ->
    case Results of
        [] ->
            {Oks, Errors};

        [{ok, A} | Rest] ->
            do_partition(Rest, [A | Oks], Errors);

        [{error, E} | Rest@1] ->
            do_partition(Rest@1, Oks, [E | Errors])
    end.

-spec partition(list({ok, BPY} | {error, BPZ})) -> {list(BPY), list(BPZ)}.
partition(Results) ->
    do_partition(Results, [], []).

-spec replace({ok, any()} | {error, BQO}, BQR) -> {ok, BQR} | {error, BQO}.
replace(Result, Value) ->
    case Result of
        {ok, _} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, BQU} | {error, any()}, BQY) -> {ok, BQU} | {error, BQY}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _} ->
            {error, Error}
    end.

-spec values(list({ok, BRB} | {error, any()})) -> list(BRB).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).

-spec try_recover(
    {ok, BRH} | {error, BRI},
    fun((BRI) -> {ok, BRH} | {error, BRL})
) -> {ok, BRH} | {error, BRL}.
try_recover(Result, Fun) ->
    case Result of
        {ok, Value} ->
            {ok, Value};

        {error, Error} ->
            Fun(Error)
    end.
