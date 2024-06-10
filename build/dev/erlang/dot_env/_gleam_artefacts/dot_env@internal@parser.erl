-module(dot_env@internal@parser).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([parse/1]).

-spec parse_comment(list(binary()), fun((list(binary())) -> FZY)) -> FZY.
parse_comment(Text, Next) ->
    case Text of
        [<<"\r"/utf8>>, <<"\n"/utf8>> | _] ->
            Next(Text);

        [<<"\n"/utf8>> | _] ->
            Next(Text);

        [_ | Rest] ->
            parse_comment(Rest, Next);

        [] ->
            Next(Text)
    end.

-spec join(list(binary())) -> binary().
join(Strings) ->
    _pipe = Strings,
    _pipe@1 = gleam@list:reverse(_pipe),
    gleam@string:join(_pipe@1, <<""/utf8>>).

-spec parse_key(list(binary()), list(binary())) -> {ok,
        {binary(), list(binary())}} |
    {error, binary()}.
parse_key(Text, Acc) ->
    case Text of
        [<<"="/utf8>> | Rest] ->
            {ok, {gleam@string:trim(join(Acc)), Rest}};

        [C | Rest@1] ->
            parse_key(Rest@1, [C | Acc]);

        [] ->
            {error, <<"unexpected end of input"/utf8>>}
    end.

-spec parse_value_unquoted(list(binary()), list(binary())) -> {ok,
        {binary(), list(binary())}} |
    {error, binary()}.
parse_value_unquoted(Text, Acc) ->
    case Text of
        [<<"\r"/utf8>>, <<"\n"/utf8>> | Rest] ->
            {ok, {gleam@string:trim(join(Acc)), Rest}};

        [<<"\n"/utf8>> | Rest] ->
            {ok, {gleam@string:trim(join(Acc)), Rest}};

        [<<"#"/utf8>> | Rest@1] ->
            parse_comment(Rest@1, fun(R) -> parse_value_unquoted(R, Acc) end);

        [C | Rest@2] ->
            parse_value_unquoted(Rest@2, [C | Acc]);

        [] ->
            {error, <<"unclosed double quote"/utf8>>}
    end.

-spec parse_value_double_quoted(list(binary()), list(binary())) -> {ok,
        {binary(), list(binary())}} |
    {error, binary()}.
parse_value_double_quoted(Text, Acc) ->
    case Text of
        [<<"\""/utf8>> | Rest] ->
            {ok, {join(Acc), Rest}};

        [<<"\\"/utf8>>, <<"\""/utf8>> = C | Rest@1] ->
            parse_value_double_quoted(Rest@1, [C | Acc]);

        [<<"\\"/utf8>>, <<"n"/utf8>> | Rest@2] ->
            parse_value_double_quoted(Rest@2, [<<"\n"/utf8>> | Acc]);

        [C@1 | Rest@3] ->
            parse_value_double_quoted(Rest@3, [C@1 | Acc]);

        [] ->
            {error, <<"unclosed double quote"/utf8>>}
    end.

-spec parse_value_single_quoted(list(binary()), list(binary())) -> {ok,
        {binary(), list(binary())}} |
    {error, binary()}.
parse_value_single_quoted(Text, Acc) ->
    case Text of
        [<<"'"/utf8>> | Rest] ->
            {ok, {join(Acc), Rest}};

        [<<"\\"/utf8>>, <<"'"/utf8>> = C | Rest@1] ->
            parse_value_single_quoted(Rest@1, [C | Acc]);

        [C@1 | Rest@2] ->
            parse_value_single_quoted(Rest@2, [C@1 | Acc]);

        [] ->
            {error, <<"unclosed single quote"/utf8>>}
    end.

-spec parse_value_backtick_quoted(list(binary()), list(binary())) -> {ok,
        {binary(), list(binary())}} |
    {error, binary()}.
parse_value_backtick_quoted(Text, Acc) ->
    case Text of
        [<<"`"/utf8>> | Rest] ->
            {ok, {join(Acc), Rest}};

        [<<"\\"/utf8>>, <<"`"/utf8>> = Char | Rest@1] ->
            parse_value_backtick_quoted(Rest@1, [Char | Acc]);

        [Char@1 | Rest@2] ->
            parse_value_backtick_quoted(Rest@2, [Char@1 | Acc]);

        [] ->
            {error, <<"unclosed backtick quote"/utf8>>}
    end.

-spec parse_value(list(binary())) -> {ok, {binary(), list(binary())}} |
    {error, binary()}.
parse_value(Text) ->
    case Text of
        [<<"\n"/utf8>> | Rest] ->
            {ok, {<<""/utf8>>, Rest}};

        [<<"\r"/utf8>>, <<"\n"/utf8>> | Rest] ->
            {ok, {<<""/utf8>>, Rest}};

        [<<"\""/utf8>> | Rest@1] ->
            parse_value_double_quoted(Rest@1, []);

        [<<"'"/utf8>> | Rest@2] ->
            parse_value_single_quoted(Rest@2, []);

        [<<"`"/utf8>> | Rest@3] ->
            parse_value_backtick_quoted(Rest@3, []);

        [<<"#"/utf8>> | Rest@4] ->
            parse_comment(Rest@4, fun(R) -> parse_value(R) end);

        [C | Rest@5] ->
            parse_value_unquoted(Rest@5, [C]);

        [] ->
            {error, <<"unexpected end of input"/utf8>>}
    end.

-spec parse_kv(list(binary())) -> {ok, {{binary(), binary()}, list(binary())}} |
    {error, binary()}.
parse_kv(Text) ->
    gleam@result:'try'(
        parse_key(Text, []),
        fun(_use0) ->
            {Key, Rest} = _use0,
            gleam@result:'try'(
                parse_value(Rest),
                fun(_use0@1) ->
                    {Value, Rest@1} = _use0@1,
                    {ok, {{Key, Value}, Rest@1}}
                end
            )
        end
    ).

-spec parse_kvs(list(binary()), list({binary(), binary()})) -> {ok,
        list({binary(), binary()})} |
    {error, binary()}.
parse_kvs(Text, Acc) ->
    case Text of
        [] ->
            {ok, gleam@list:reverse(Acc)};

        [<<"\r"/utf8>>, <<"\n"/utf8>> | Rest] ->
            parse_kvs(Rest, Acc);

        [<<"\n"/utf8>> | Rest] ->
            parse_kvs(Rest, Acc);

        [<<" "/utf8>> | Rest] ->
            parse_kvs(Rest, Acc);

        [<<"#"/utf8>> | Rest@1] ->
            parse_comment(Rest@1, fun(R) -> parse_kvs(R, Acc) end);

        [<<"e"/utf8>>,
            <<"x"/utf8>>,
            <<"p"/utf8>>,
            <<"o"/utf8>>,
            <<"r"/utf8>>,
            <<"t"/utf8>>,
            <<" "/utf8>> |
            Rest@2] ->
            parse_kvs(Rest@2, Acc);

        _ ->
            gleam@result:'try'(
                parse_kv(Text),
                fun(_use0) ->
                    {Pair, Rest@3} = _use0,
                    parse_kvs(Rest@3, [Pair | Acc])
                end
            )
    end.

-spec parse(binary()) -> {ok, list({binary(), binary()})} | {error, binary()}.
parse(Text) ->
    _pipe = Text,
    _pipe@1 = gleam@string:to_graphemes(_pipe),
    parse_kvs(_pipe@1, []).
