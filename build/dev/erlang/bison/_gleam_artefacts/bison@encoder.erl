-module(bison@encoder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([encode/1, encode_list/1]).

-spec null() -> {bison@kind:kind(), bitstring()}.
null() ->
    {{kind, <<16#0A>>}, <<>>}.

-spec nan() -> {bison@kind:kind(), bitstring()}.
nan() ->
    {{kind, <<16#01>>}, <<"NaN"/utf8>>}.

-spec infinity() -> {bison@kind:kind(), bitstring()}.
infinity() ->
    {{kind, <<16#01>>}, <<"Infinity"/utf8>>}.

-spec negative_infinity() -> {bison@kind:kind(), bitstring()}.
negative_infinity() ->
    {{kind, <<16#01>>}, <<"-Infinity"/utf8>>}.

-spec min() -> {bison@kind:kind(), bitstring()}.
min() ->
    {{kind, <<16#FF>>}, <<>>}.

-spec max() -> {bison@kind:kind(), bitstring()}.
max() ->
    {{kind, <<16#7F>>}, <<>>}.

-spec js(binary()) -> {bison@kind:kind(), bitstring()}.
js(Value) ->
    Length = erlang:byte_size(<<Value/binary>>) + 1,
    {{kind, <<16#0D>>}, <<Length:32/little, Value/binary, 0>>}.

-spec string(binary()) -> {bison@kind:kind(), bitstring()}.
string(Value) ->
    Length = erlang:byte_size(<<Value/binary>>) + 1,
    {{kind, <<16#02>>}, <<Length:32/little, Value/binary, 0>>}.

-spec boolean(boolean()) -> {bison@kind:kind(), bitstring()}.
boolean(Value) ->
    case Value of
        true ->
            {{kind, <<16#08>>}, <<1>>};

        false ->
            {{kind, <<16#08>>}, <<0>>}
    end.

-spec double(float()) -> {bison@kind:kind(), bitstring()}.
double(Value) ->
    {{kind, <<16#01>>}, <<Value/little-float>>}.

-spec int64(integer()) -> {bison@kind:kind(), bitstring()}.
int64(Value) ->
    case (Value >= -9223372036854775808) andalso (Value =< 9223372036854775807) of
        true ->
            {{kind, <<16#12>>}, <<Value:64/little>>};

        false ->
            _pipe = Value,
            _pipe@1 = gleam@int:to_float(_pipe),
            double(_pipe@1)
    end.

-spec int32(integer()) -> {bison@kind:kind(), bitstring()}.
int32(Value) ->
    case (Value >= -2147483648) andalso (Value =< 2147483647) of
        true ->
            {{kind, <<16#10>>}, <<Value:32/little>>};

        false ->
            int64(Value)
    end.

-spec datetime(birl:time()) -> {bison@kind:kind(), bitstring()}.
datetime(Value) ->
    {duration, Value@1} = birl:difference(Value, {time, 0, 0, none, none}),
    Value@2 = Value@1 div 1000,
    {{kind, <<16#09>>}, <<Value@2:64/little>>}.

-spec object_id(bison@object_id:object_id()) -> {bison@kind:kind(), bitstring()}.
object_id(Value) ->
    {{kind, <<16#07>>}, bison@object_id:to_bit_array(Value)}.

-spec timestamp(integer(), integer()) -> {bison@kind:kind(), bitstring()}.
timestamp(Stamp, Counter) ->
    {{kind, <<16#11>>}, <<Counter:32/little, Stamp:32/little>>}.

-spec md5(bison@md5:md5()) -> {bison@kind:kind(), bitstring()}.
md5(Value) ->
    Value@1 = bison@md5:to_bit_array(Value),
    Length = erlang:byte_size(Value@1),
    {{kind, <<16#05>>},
        gleam_stdlib:bit_array_concat(
            [<<Length:32/little>>,
                erlang:element(2, {sub_kind, <<16#5>>}),
                Value@1]
        )}.

-spec uuid(bison@uuid:uuid()) -> {bison@kind:kind(), bitstring()}.
uuid(Value) ->
    Value@1 = bison@uuid:to_bit_array(Value),
    Length = erlang:byte_size(Value@1),
    {{kind, <<16#05>>},
        gleam_stdlib:bit_array_concat(
            [<<Length:32/little>>,
                erlang:element(2, {sub_kind, <<16#4>>}),
                Value@1]
        )}.

-spec custom(bison@custom:custom()) -> {bison@kind:kind(), bitstring()}.
custom(Value) ->
    {Code, Value@1} = bison@custom:to_bit_array_with_code(Value),
    Length = erlang:byte_size(Value@1),
    {{kind, <<16#05>>},
        gleam_stdlib:bit_array_concat([<<Length:32/little>>, <<Code>>, Value@1])}.

-spec generic(bison@generic:generic()) -> {bison@kind:kind(), bitstring()}.
generic(Value) ->
    Value@1 = bison@generic:to_bit_array(Value),
    Length = erlang:byte_size(Value@1),
    {{kind, <<16#05>>},
        gleam_stdlib:bit_array_concat(
            [<<Length:32/little>>,
                erlang:element(2, {sub_kind, <<16#0>>}),
                Value@1]
        )}.

-spec regex(binary(), binary()) -> {bison@kind:kind(), bitstring()}.
regex(Pattern, Options) ->
    {{kind, <<16#0B>>},
        gleam_stdlib:bit_array_concat(
            [<<Pattern/binary, 0, Options/binary, 0>>]
        )}.

-spec encode_kv({binary(), bison@bson:value()}) -> bitstring().
encode_kv(Pair) ->
    Key = <<(erlang:element(1, Pair))/binary, 0>>,
    {Kind, Value@14} = case erlang:element(2, Pair) of
        na_n ->
            nan();

        min ->
            min();

        max ->
            max();

        null ->
            null();

        infinity ->
            infinity();

        {js, Value} ->
            js(Value);

        {int32, Value@1} ->
            int32(Value@1);

        {int64, Value@2} ->
            int64(Value@2);

        {array, Value@3} ->
            array(Value@3);

        {double, Value@4} ->
            double(Value@4);

        {string, Value@5} ->
            string(Value@5);

        {boolean, Value@6} ->
            boolean(Value@6);

        {document, Value@7} ->
            document(Value@7);

        {date_time, Value@8} ->
            datetime(Value@8);

        {object_id, Value@9} ->
            object_id(Value@9);

        {binary, {md5, Value@10}} ->
            md5(Value@10);

        negative_infinity ->
            negative_infinity();

        {binary, {uuid, Value@11}} ->
            uuid(Value@11);

        {binary, {custom, Value@12}} ->
            custom(Value@12);

        {binary, {generic, Value@13}} ->
            generic(Value@13);

        {regex, Pattern, Options} ->
            regex(Pattern, Options);

        {timestamp, Stamp, Counter} ->
            timestamp(Stamp, Counter)
    end,
    gleam_stdlib:bit_array_concat([erlang:element(2, Kind), Key, Value@14]).

-spec document(gleam@dict:dict(binary(), bison@bson:value())) -> {bison@kind:kind(),
    bitstring()}.
document(Doc) ->
    Doc@1 = gleam@dict:fold(
        Doc,
        <<>>,
        fun(Acc, Key, Value) ->
            gleam@bit_array:append(Acc, encode_kv({Key, Value}))
        end
    ),
    Size = erlang:byte_size(Doc@1) + 5,
    {{kind, <<16#03>>},
        gleam_stdlib:bit_array_concat([<<Size:32/little>>, Doc@1, <<0>>])}.

-spec encode(gleam@dict:dict(binary(), bison@bson:value())) -> bitstring().
encode(Doc) ->
    case document(Doc) of
        {_, Value} ->
            Value
    end.

-spec document_from_list(list({binary(), bison@bson:value()})) -> {bison@kind:kind(),
    bitstring()}.
document_from_list(Doc) ->
    Doc@1 = gleam@list:fold(
        Doc,
        <<>>,
        fun(Acc, Kv) ->
            gleam@bit_array:append(
                Acc,
                encode_kv({erlang:element(1, Kv), erlang:element(2, Kv)})
            )
        end
    ),
    Size = erlang:byte_size(Doc@1) + 5,
    {{kind, <<16#03>>},
        gleam_stdlib:bit_array_concat([<<Size:32/little>>, Doc@1, <<0>>])}.

-spec encode_list(list({binary(), bison@bson:value()})) -> bitstring().
encode_list(Doc) ->
    case document_from_list(Doc) of
        {_, Value} ->
            Value
    end.

-spec array(list(bison@bson:value())) -> {bison@kind:kind(), bitstring()}.
array(Value) ->
    case begin
        _pipe = gleam@list:index_map(
            Value,
            fun(Item, Index) -> {gleam@int:to_string(Index), Item} end
        ),
        _pipe@1 = maps:from_list(_pipe),
        document(_pipe@1)
    end of
        {_, Value@1} ->
            {{kind, <<16#04>>}, Value@1}
    end.
