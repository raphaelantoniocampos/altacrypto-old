-module(bison@ejson@decoder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([from_canonical/1]).

-spec map_value(juno:value(bison@bson:value())) -> bison@bson:value().
map_value(Value) ->
    case Value of
        null ->
            null;

        {custom, Value@1} ->
            Value@1;

        {int, Value@2} ->
            {int64, Value@2};

        {bool, Value@3} ->
            {boolean, Value@3};

        {float, Value@4} ->
            {double, Value@4};

        {string, Value@5} ->
            {string, Value@5};

        {array, Values} ->
            {array, gleam@list:map(Values, fun map_value/1)};

        {object, Value@6} ->
            {document,
                maps:from_list(
                    gleam@list:map(
                        maps:to_list(Value@6),
                        fun(Kv) ->
                            {erlang:element(1, Kv),
                                map_value(erlang:element(2, Kv))}
                        end
                    )
                )}
    end.

-spec code(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
code(Dyn) ->
    (gleam@dynamic:decode1(
        fun(Field@0) -> {js, Field@0} end,
        gleam@dynamic:field(<<"$code"/utf8>>, fun gleam@dynamic:string/1)
    ))(Dyn).

-spec oid(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
oid(Dyn) ->
    (gleam@dynamic:decode1(
        fun(Field@0) -> {object_id, Field@0} end,
        gleam@dynamic:field(<<"$oid"/utf8>>, fun(V) -> _pipe = V,
                _pipe@1 = gleam@dynamic:string(_pipe),
                _pipe@3 = gleam@result:map(
                    _pipe@1,
                    fun(S) -> _pipe@2 = bison@object_id:from_string(S),
                        gleam@result:replace_error(
                            _pipe@2,
                            [{decode_error,
                                    <<"object id"/utf8>>,
                                    <<"not object id"/utf8>>,
                                    []}]
                        ) end
                ),
                gleam@result:flatten(_pipe@3) end)
    ))(Dyn).

-spec timestamp(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
timestamp(Dyn) ->
    (gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {timestamp, Field@0, Field@1} end,
        gleam@dynamic:field(
            <<"$timestamp"/utf8>>,
            gleam@dynamic:field(<<"t"/utf8>>, fun gleam@dynamic:int/1)
        ),
        gleam@dynamic:field(
            <<"$timestamp"/utf8>>,
            gleam@dynamic:field(<<"i"/utf8>>, fun gleam@dynamic:int/1)
        )
    ))(Dyn).

-spec regex(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
regex(Dyn) ->
    (gleam@dynamic:decode2(
        fun(Field@0, Field@1) -> {regex, Field@0, Field@1} end,
        gleam@dynamic:field(
            <<"$regularExpression"/utf8>>,
            gleam@dynamic:field(<<"pattern"/utf8>>, fun gleam@dynamic:string/1)
        ),
        gleam@dynamic:field(
            <<"$regularExpression"/utf8>>,
            gleam@dynamic:field(<<"options"/utf8>>, fun gleam@dynamic:string/1)
        )
    ))(Dyn).

-spec min(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
min(Dyn) ->
    _pipe = Dyn,
    _pipe@1 = (gleam@dynamic:field(<<"$minKey"/utf8>>, fun gleam@dynamic:int/1))(
        _pipe
    ),
    _pipe@2 = gleam@result:map(_pipe@1, fun(S) -> case S of
                1 ->
                    {ok, min};

                _ ->
                    {error,
                        [{decode_error,
                                <<"1"/utf8>>,
                                gleam@int:to_string(S),
                                []}]}
            end end),
    gleam@result:flatten(_pipe@2).

-spec max(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
max(Dyn) ->
    _pipe = Dyn,
    _pipe@1 = (gleam@dynamic:field(<<"$maxKey"/utf8>>, fun gleam@dynamic:int/1))(
        _pipe
    ),
    _pipe@2 = gleam@result:map(_pipe@1, fun(S) -> case S of
                1 ->
                    {ok, max};

                _ ->
                    {error,
                        [{decode_error,
                                <<"1"/utf8>>,
                                gleam@int:to_string(S),
                                []}]}
            end end),
    gleam@result:flatten(_pipe@2).

-spec typed_int(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
typed_int(Dyn) ->
    _pipe = Dyn,
    _pipe@1 = (gleam@dynamic:field(
        <<"$numberInt"/utf8>>,
        fun gleam@dynamic:string/1
    ))(_pipe),
    _pipe@2 = gleam@result:map(_pipe@1, fun(S) -> case gleam@int:parse(S) of
                {ok, N} ->
                    {ok, {int32, N}};

                _ ->
                    {error, [{decode_error, <<"Integer"/utf8>>, S, []}]}
            end end),
    gleam@result:flatten(_pipe@2).

-spec typed_long(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
typed_long(Dyn) ->
    _pipe = Dyn,
    _pipe@1 = (gleam@dynamic:field(
        <<"$numberLong"/utf8>>,
        fun gleam@dynamic:string/1
    ))(_pipe),
    _pipe@2 = gleam@result:map(_pipe@1, fun(S) -> case gleam@int:parse(S) of
                {ok, N} ->
                    {ok, {int64, N}};

                _ ->
                    {error, [{decode_error, <<"Integer"/utf8>>, S, []}]}
            end end),
    gleam@result:flatten(_pipe@2).

-spec date(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
date(Dyn) ->
    (gleam@dynamic:decode1(
        fun(Field@0) -> {date_time, Field@0} end,
        gleam@dynamic:field(<<"$date"/utf8>>, fun(V) -> _pipe = V,
                _pipe@1 = typed_long(_pipe),
                _pipe@2 = gleam@result:map(_pipe@1, fun(N) -> case N of
                            {int64, N@1} ->
                                {ok,
                                    birl:add(
                                        {time, 0, 0, none, none},
                                        {duration, N@1 * 1000}
                                    )};

                            _ ->
                                {error,
                                    [{decode_error,
                                            <<"long"/utf8>>,
                                            <<"not long"/utf8>>,
                                            []}]}
                        end end),
                gleam@result:flatten(_pipe@2) end)
    ))(Dyn).

-spec typed_double(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
typed_double(Dyn) ->
    _pipe = Dyn,
    _pipe@1 = (gleam@dynamic:field(
        <<"$numberDouble"/utf8>>,
        fun gleam@dynamic:string/1
    ))(_pipe),
    _pipe@2 = gleam@result:map(_pipe@1, fun(S) -> case S of
                <<"NaN"/utf8>> ->
                    {ok, na_n};

                <<"infinity"/utf8>> ->
                    {ok, infinity};

                <<"-infinity"/utf8>> ->
                    {ok, negative_infinity};

                _ ->
                    case gleam@float:parse(S) of
                        {ok, F} ->
                            {ok, {double, F}};

                        {error, nil} ->
                            {error,
                                [{decode_error,
                                        <<"Floating point number, NaN, Infinity or -Infinity"/utf8>>,
                                        S,
                                        []}]}
                    end
            end end),
    gleam@result:flatten(_pipe@2).

-spec binary(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
binary(Dyn) ->
    (gleam@dynamic:decode1(
        fun(Field@0) -> {binary, Field@0} end,
        gleam@dynamic:field(<<"$binary"/utf8>>, fun(V) -> _pipe = V,
                _pipe@1 = (gleam@dynamic:dict(
                    fun gleam@dynamic:string/1,
                    fun gleam@dynamic:string/1
                ))(_pipe),
                _pipe@2 = gleam@result:map(
                    _pipe@1,
                    fun(Bin_doc) ->
                        case {gleam@dict:get(Bin_doc, <<"base64"/utf8>>),
                            gleam@dict:get(Bin_doc, <<"subType"/utf8>>)} of
                            {{ok, Base64}, {ok, Sub_type}} ->
                                case gleam@bit_array:base64_decode(Base64) of
                                    {ok, Decoded} ->
                                        case Sub_type of
                                            <<"00"/utf8>> ->
                                                case bison@generic:from_bit_array(
                                                    Decoded
                                                ) of
                                                    {ok, Generic} ->
                                                        {ok, {generic, Generic}};

                                                    {error, nil} ->
                                                        {error,
                                                            [{decode_error,
                                                                    <<"generic binary"/utf8>>,
                                                                    <<"not generic binary"/utf8>>,
                                                                    []}]}
                                                end;

                                            <<"04"/utf8>> ->
                                                case bison@uuid:from_bit_array(
                                                    Decoded
                                                ) of
                                                    {ok, Uuid} ->
                                                        {ok, {uuid, Uuid}};

                                                    {error, nil} ->
                                                        {error,
                                                            [{decode_error,
                                                                    <<"uuid binary"/utf8>>,
                                                                    <<"not uuid binary"/utf8>>,
                                                                    []}]}
                                                end;

                                            <<"05"/utf8>> ->
                                                case bison@md5:from_bit_array(
                                                    Decoded
                                                ) of
                                                    {ok, Md5} ->
                                                        {ok, {md5, Md5}};

                                                    {error, nil} ->
                                                        {error,
                                                            [{decode_error,
                                                                    <<"md5 binary"/utf8>>,
                                                                    <<"not md5 binary"/utf8>>,
                                                                    []}]}
                                                end;

                                            _ ->
                                                case gleam@int:parse(Sub_type) of
                                                    {ok, Code} ->
                                                        case bison@custom:from_bit_array_with_code(
                                                            Code,
                                                            Decoded
                                                        ) of
                                                            {ok, Custom} ->
                                                                {ok,
                                                                    {custom,
                                                                        Custom}};

                                                            {error, nil} ->
                                                                {error,
                                                                    [{decode_error,
                                                                            <<"valid custom binary code"/utf8>>,
                                                                            <<"invalid custom binary code"/utf8>>,
                                                                            []}]}
                                                        end;

                                                    {error, nil} ->
                                                        {error,
                                                            [{decode_error,
                                                                    <<"valid custom binary code"/utf8>>,
                                                                    <<"invalid custom binary code"/utf8>>,
                                                                    []}]}
                                                end
                                        end;

                                    _ ->
                                        {error,
                                            [{decode_error,
                                                    <<"binary"/utf8>>,
                                                    <<"not binary"/utf8>>,
                                                    []}]}
                                end;

                            {_, _} ->
                                {error,
                                    [{decode_error,
                                            <<"binary"/utf8>>,
                                            <<"not binary"/utf8>>,
                                            []}]}
                        end
                    end
                ),
                gleam@result:flatten(_pipe@2) end)
    ))(Dyn).

-spec from_canonical(binary()) -> {ok,
        gleam@dict:dict(binary(), bison@bson:value())} |
    {error, gleam@json:decode_error()}.
from_canonical(Doc) ->
    gleam@result:then(
        juno:decode_object(
            Doc,
            [fun oid/1,
                fun date/1,
                fun timestamp/1,
                fun regex/1,
                fun min/1,
                fun max/1,
                fun typed_int/1,
                fun typed_long/1,
                fun typed_double/1,
                fun binary/1,
                fun code/1]
        ),
        fun(Doc@1) ->
            _assert_subject = map_value(Doc@1),
            {document, Doc@2} = case _assert_subject of
                {document, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"bison/ejson/decoder"/utf8>>,
                                function => <<"from_canonical"/utf8>>,
                                line => 36})
            end,
            {ok, Doc@2}
        end
    ).
