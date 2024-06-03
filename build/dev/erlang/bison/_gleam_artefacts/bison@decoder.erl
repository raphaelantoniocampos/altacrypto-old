-module(bison@decoder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([decode/1]).

-spec consume_till_zero(bitstring(), bitstring()) -> {ok,
        {bitstring(), bitstring()}} |
    {error, nil}.
consume_till_zero(Binary, Storage) ->
    case Binary of
        <<Ch:8, Rest/bitstring>> ->
            case Ch of
                0 ->
                    {ok, {Storage, Rest}};

                _ ->
                    consume_till_zero(
                        Rest,
                        gleam@bit_array:append(Storage, <<Ch>>)
                    )
            end;

        _ ->
            {error, nil}
    end.

-spec decode_boolean(integer(), fun((boolean()) -> {ok, HYQ} | {error, nil})) -> {ok,
        HYQ} |
    {error, nil}.
decode_boolean(Value, Rest) ->
    case Value of
        0 ->
            Rest(false);

        1 ->
            Rest(true);

        _ ->
            {error, nil}
    end.

-spec decode_body(bitstring(), gleam@dict:dict(binary(), bison@bson:value())) -> {ok,
        gleam@dict:dict(binary(), bison@bson:value())} |
    {error, nil}.
decode_body(Binary, Storage) ->
    gleam@bool:guard(
        erlang:byte_size(Binary) =:= 0,
        {ok, Storage},
        fun() -> case Binary of
                <<Code:8, Binary@1/bitstring>> ->
                    gleam@result:then(
                        consume_till_zero(Binary@1, <<>>),
                        fun(_use0) ->
                            {Key, Rest} = _use0,
                            gleam@result:then(
                                gleam@bit_array:to_string(Key),
                                fun(Key@1) -> case {kind, <<Code>>} of
                                        K when K =:= {kind, <<16#FF>>} ->
                                            recurse_with_new_kv(
                                                Rest,
                                                Storage,
                                                Key@1,
                                                min
                                            );

                                        K@1 when K@1 =:= {kind, <<16#7F>>} ->
                                            recurse_with_new_kv(
                                                Rest,
                                                Storage,
                                                Key@1,
                                                max
                                            );

                                        K@2 when K@2 =:= {kind, <<16#0A>>} ->
                                            recurse_with_new_kv(
                                                Rest,
                                                Storage,
                                                Key@1,
                                                null
                                            );

                                        K@3 when K@3 =:= {kind, <<16#10>>} ->
                                            case Rest of
                                                <<Value:32/little-signed,
                                                    Rest@1/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@1,
                                                        Storage,
                                                        Key@1,
                                                        {int32, Value}
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@4 when K@4 =:= {kind, <<16#12>>} ->
                                            case Rest of
                                                <<Value@1:64/little-signed,
                                                    Rest@2/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@2,
                                                        Storage,
                                                        Key@1,
                                                        {int64, Value@1}
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@5 when K@5 =:= {kind, <<16#01>>} ->
                                            case Rest of
                                                <<"NaN"/utf8, Rest@3/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@3,
                                                        Storage,
                                                        Key@1,
                                                        na_n
                                                    );

                                                <<"Infinity"/utf8,
                                                    Rest@4/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@4,
                                                        Storage,
                                                        Key@1,
                                                        infinity
                                                    );

                                                <<"-Infinity"/utf8,
                                                    Rest@5/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@5,
                                                        Storage,
                                                        Key@1,
                                                        negative_infinity
                                                    );

                                                <<Value@2/little-float,
                                                    Rest@6/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@6,
                                                        Storage,
                                                        Key@1,
                                                        {double, Value@2}
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@6 when K@6 =:= {kind, <<16#11>>} ->
                                            case Rest of
                                                <<Counter:32/little-unsigned,
                                                    Stamp:32/little-unsigned,
                                                    Rest@7/bitstring>> ->
                                                    recurse_with_new_kv(
                                                        Rest@7,
                                                        Storage,
                                                        Key@1,
                                                        {timestamp,
                                                            Stamp,
                                                            Counter}
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@7 when K@7 =:= {kind, <<16#07>>} ->
                                            case Rest of
                                                <<Value@3:96/bitstring,
                                                    Rest@8/bitstring>> ->
                                                    gleam@result:then(
                                                        bison@object_id:from_bit_array(
                                                            Value@3
                                                        ),
                                                        fun(Oid) ->
                                                            recurse_with_new_kv(
                                                                Rest@8,
                                                                Storage,
                                                                Key@1,
                                                                {object_id, Oid}
                                                            )
                                                        end
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@8 when K@8 =:= {kind, <<16#08>>} ->
                                            case Rest of
                                                <<Value@4:8, Rest@9/bitstring>> ->
                                                    decode_boolean(
                                                        Value@4,
                                                        fun(Value@5) ->
                                                            recurse_with_new_kv(
                                                                Rest@9,
                                                                Storage,
                                                                Key@1,
                                                                {boolean,
                                                                    Value@5}
                                                            )
                                                        end
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@9 when K@9 =:= {kind, <<16#09>>} ->
                                            case Rest of
                                                <<Value@6:64/little-signed,
                                                    Rest@10/bitstring>> ->
                                                    Value@7 = birl:add(
                                                        {time, 0, 0, none, none},
                                                        birl@duration:accurate_new(
                                                            [{Value@6,
                                                                    milli_second}]
                                                        )
                                                    ),
                                                    recurse_with_new_kv(
                                                        Rest@10,
                                                        Storage,
                                                        Key@1,
                                                        {date_time, Value@7}
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@10 when K@10 =:= {kind, <<16#0B>>} ->
                                            gleam@result:then(
                                                consume_till_zero(Rest, <<>>),
                                                fun(_use0@1) ->
                                                    {Pattern_bytes, Rest@11} = _use0@1,
                                                    gleam@result:then(
                                                        consume_till_zero(
                                                            Rest@11,
                                                            <<>>
                                                        ),
                                                        fun(_use0@2) ->
                                                            {Options_bytes,
                                                                Rest@12} = _use0@2,
                                                            gleam@result:then(
                                                                gleam@bit_array:to_string(
                                                                    Pattern_bytes
                                                                ),
                                                                fun(Pattern) ->
                                                                    gleam@result:then(
                                                                        gleam@bit_array:to_string(
                                                                            Options_bytes
                                                                        ),
                                                                        fun(
                                                                            Options
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@12,
                                                                                Storage,
                                                                                Key@1,
                                                                                {regex,
                                                                                    Pattern,
                                                                                    Options}
                                                                            )
                                                                        end
                                                                    )
                                                                end
                                                            )
                                                        end
                                                    )
                                                end
                                            );

                                        K@11 when K@11 =:= {kind, <<16#02>>} ->
                                            case Rest of
                                                <<Given_size:32/little-integer,
                                                    Rest@13/bitstring>> ->
                                                    gleam@result:then(
                                                        consume_till_zero(
                                                            Rest@13,
                                                            <<>>
                                                        ),
                                                        fun(_use0@3) ->
                                                            {Str, Rest@14} = _use0@3,
                                                            Str_size = erlang:byte_size(
                                                                Str
                                                            ),
                                                            case Given_size =:= (Str_size
                                                            + 1) of
                                                                true ->
                                                                    gleam@result:then(
                                                                        gleam@bit_array:to_string(
                                                                            Str
                                                                        ),
                                                                        fun(
                                                                            Str@1
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@14,
                                                                                Storage,
                                                                                Key@1,
                                                                                {string,
                                                                                    Str@1}
                                                                            )
                                                                        end
                                                                    );

                                                                false ->
                                                                    {error, nil}
                                                            end
                                                        end
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@12 when K@12 =:= {kind, <<16#0D>>} ->
                                            case Rest of
                                                <<Given_size@1:32/little-integer,
                                                    Rest@15/bitstring>> ->
                                                    gleam@result:then(
                                                        consume_till_zero(
                                                            Rest@15,
                                                            <<>>
                                                        ),
                                                        fun(_use0@4) ->
                                                            {Str@2, Rest@16} = _use0@4,
                                                            Str_size@1 = erlang:byte_size(
                                                                Str@2
                                                            ),
                                                            case Given_size@1
                                                            =:= (Str_size@1 + 1) of
                                                                true ->
                                                                    gleam@result:then(
                                                                        gleam@bit_array:to_string(
                                                                            Str@2
                                                                        ),
                                                                        fun(
                                                                            Str@3
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@16,
                                                                                Storage,
                                                                                Key@1,
                                                                                {js,
                                                                                    Str@3}
                                                                            )
                                                                        end
                                                                    );

                                                                false ->
                                                                    {error, nil}
                                                            end
                                                        end
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@13 when (K@13 =:= {kind, <<16#03>>}) orelse (K@13 =:= {kind,
                                            <<16#04>>}) ->
                                            case Rest of
                                                <<Doc_size:32/little-integer,
                                                    _/bitstring>> ->
                                                    gleam@result:then(
                                                        gleam_stdlib:bit_array_slice(
                                                            Rest,
                                                            0,
                                                            Doc_size
                                                        ),
                                                        fun(Doc) ->
                                                            gleam@result:then(
                                                                decode_document(
                                                                    Doc
                                                                ),
                                                                fun(Doc@1) ->
                                                                    case Doc@1 of
                                                                        {document,
                                                                            Doc@2} ->
                                                                            gleam@result:then(
                                                                                case K@13 of
                                                                                    K@14 when K@14 =:= {kind,
                                                                                        <<16#03>>} ->
                                                                                        _pipe = Doc@2,
                                                                                        _pipe@1 = {document,
                                                                                            _pipe},
                                                                                        {ok,
                                                                                            _pipe@1};

                                                                                    K@15 when K@15 =:= {kind,
                                                                                        <<16#04>>} ->
                                                                                        gleam@result:then(
                                                                                            gleam@list:try_map(
                                                                                                maps:to_list(
                                                                                                    Doc@2
                                                                                                ),
                                                                                                fun(
                                                                                                    Item
                                                                                                ) ->
                                                                                                    gleam@result:then(
                                                                                                        gleam@int:parse(
                                                                                                            erlang:element(
                                                                                                                1,
                                                                                                                Item
                                                                                                            )
                                                                                                        ),
                                                                                                        fun(
                                                                                                            First
                                                                                                        ) ->
                                                                                                            {ok,
                                                                                                                {First,
                                                                                                                    erlang:element(
                                                                                                                        2,
                                                                                                                        Item
                                                                                                                    )}}
                                                                                                        end
                                                                                                    )
                                                                                                end
                                                                                            ),
                                                                                            fun(
                                                                                                Doc@3
                                                                                            ) ->
                                                                                                _pipe@2 = Doc@3,
                                                                                                _pipe@3 = gleam@list:sort(
                                                                                                    _pipe@2,
                                                                                                    fun(
                                                                                                        A,
                                                                                                        B
                                                                                                    ) ->
                                                                                                        gleam@int:compare(
                                                                                                            erlang:element(
                                                                                                                1,
                                                                                                                A
                                                                                                            ),
                                                                                                            erlang:element(
                                                                                                                1,
                                                                                                                B
                                                                                                            )
                                                                                                        )
                                                                                                    end
                                                                                                ),
                                                                                                _pipe@4 = gleam@list:map(
                                                                                                    _pipe@3,
                                                                                                    fun gleam@pair:second/1
                                                                                                ),
                                                                                                _pipe@5 = {array,
                                                                                                    _pipe@4},
                                                                                                {ok,
                                                                                                    _pipe@5}
                                                                                            end
                                                                                        );

                                                                                    _ ->
                                                                                        {error,
                                                                                            nil}
                                                                                end,
                                                                                fun(
                                                                                    Doc@4
                                                                                ) ->
                                                                                    case gleam_stdlib:bit_array_slice(
                                                                                        Rest,
                                                                                        Doc_size,
                                                                                        erlang:byte_size(
                                                                                            Rest
                                                                                        )
                                                                                        - Doc_size
                                                                                    ) of
                                                                                        {ok,
                                                                                            Rest@17} ->
                                                                                            recurse_with_new_kv(
                                                                                                Rest@17,
                                                                                                Storage,
                                                                                                Key@1,
                                                                                                Doc@4
                                                                                            );

                                                                                        {error,
                                                                                            nil} ->
                                                                                            {error,
                                                                                                nil}
                                                                                    end
                                                                                end
                                                                            );

                                                                        _ ->
                                                                            {error,
                                                                                nil}
                                                                    end
                                                                end
                                                            )
                                                        end
                                                    );

                                                _ ->
                                                    {error, nil}
                                            end;

                                        K@16 when K@16 =:= {kind, <<16#05>>} ->
                                            case Rest of
                                                <<Byte_size:32/little-integer,
                                                    Sub_code:8,
                                                    Rest@18/bitstring>> ->
                                                    Given_size@2 = Byte_size * 8,
                                                    case Rest@18 of
                                                        <<Value@8:Given_size@2/bitstring,
                                                            Rest@19/bitstring>> ->
                                                            case {sub_kind,
                                                                <<Sub_code>>} of
                                                                Sub_kind when Sub_kind =:= {sub_kind,
                                                                    <<16#0>>} ->
                                                                    gleam@result:then(
                                                                        bison@generic:from_bit_array(
                                                                            Value@8
                                                                        ),
                                                                        fun(
                                                                            Value@9
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@19,
                                                                                Storage,
                                                                                Key@1,
                                                                                begin
                                                                                    _pipe@6 = Value@9,
                                                                                    _pipe@7 = {generic,
                                                                                        _pipe@6},
                                                                                    {binary,
                                                                                        _pipe@7}
                                                                                end
                                                                            )
                                                                        end
                                                                    );

                                                                Sub_kind@1 when Sub_kind@1 =:= {sub_kind,
                                                                    <<16#5>>} ->
                                                                    gleam@result:then(
                                                                        bison@md5:from_bit_array(
                                                                            Value@8
                                                                        ),
                                                                        fun(
                                                                            Value@10
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@19,
                                                                                Storage,
                                                                                Key@1,
                                                                                begin
                                                                                    _pipe@8 = Value@10,
                                                                                    _pipe@9 = {md5,
                                                                                        _pipe@8},
                                                                                    {binary,
                                                                                        _pipe@9}
                                                                                end
                                                                            )
                                                                        end
                                                                    );

                                                                Sub_kind@2 when Sub_kind@2 =:= {sub_kind,
                                                                    <<16#4>>} ->
                                                                    gleam@result:then(
                                                                        bison@uuid:from_bit_array(
                                                                            Value@8
                                                                        ),
                                                                        fun(
                                                                            Value@11
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@19,
                                                                                Storage,
                                                                                Key@1,
                                                                                begin
                                                                                    _pipe@10 = Value@11,
                                                                                    _pipe@11 = {uuid,
                                                                                        _pipe@10},
                                                                                    {binary,
                                                                                        _pipe@11}
                                                                                end
                                                                            )
                                                                        end
                                                                    );

                                                                _ when Sub_code >= 16#80 ->
                                                                    gleam@result:then(
                                                                        bison@custom:from_bit_array_with_code(
                                                                            Sub_code,
                                                                            Value@8
                                                                        ),
                                                                        fun(
                                                                            Value@12
                                                                        ) ->
                                                                            recurse_with_new_kv(
                                                                                Rest@19,
                                                                                Storage,
                                                                                Key@1,
                                                                                begin
                                                                                    _pipe@12 = Value@12,
                                                                                    _pipe@13 = {custom,
                                                                                        _pipe@12},
                                                                                    {binary,
                                                                                        _pipe@13}
                                                                                end
                                                                            )
                                                                        end
                                                                    );

                                                                _ ->
                                                                    {error, nil}
                                                            end;

                                                        _ ->
                                                            {error, nil}
                                                    end;

                                                _ ->
                                                    {error, nil}
                                            end;

                                        _ ->
                                            {error, nil}
                                    end end
                            )
                        end
                    );

                _ ->
                    {error, nil}
            end end
    ).

-spec recurse_with_new_kv(
    bitstring(),
    gleam@dict:dict(binary(), bison@bson:value()),
    binary(),
    bison@bson:value()
) -> {ok, gleam@dict:dict(binary(), bison@bson:value())} | {error, nil}.
recurse_with_new_kv(Rest, Storage, Key, Value) ->
    decode_body(Rest, gleam@dict:insert(Storage, Key, Value)).

-spec decode_document(bitstring()) -> {ok, bison@bson:value()} | {error, nil}.
decode_document(Binary) ->
    Total_size = erlang:byte_size(Binary),
    Last_byte = gleam_stdlib:bit_array_slice(Binary, Total_size, -1),
    case Last_byte of
        {ok, <<0>>} ->
            case Binary of
                <<Given_size:32/little-integer, Rest/bitstring>> ->
                    case Total_size =:= Given_size of
                        true ->
                            gleam@result:then(
                                gleam_stdlib:bit_array_slice(
                                    Rest,
                                    0,
                                    (Total_size - 4) - 1
                                ),
                                fun(Body) ->
                                    gleam@result:then(
                                        decode_body(Body, gleam@dict:new()),
                                        fun(Body@1) -> _pipe = Body@1,
                                            _pipe@1 = {document, _pipe},
                                            {ok, _pipe@1} end
                                    )
                                end
                            );

                        false ->
                            {error, nil}
                    end;

                _ ->
                    {error, nil}
            end;

        _ ->
            {error, nil}
    end.

-spec decode(bitstring()) -> {ok, gleam@dict:dict(binary(), bison@bson:value())} |
    {error, nil}.
decode(Binary) ->
    case decode_document(Binary) of
        {ok, {document, Doc}} ->
            {ok, Doc};

        _ ->
            {error, nil}
    end.
