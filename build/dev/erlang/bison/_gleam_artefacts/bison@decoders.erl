-module(bison@decoders).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([min/1, max/1, nan/1, nil/1, infinity/1, negative_infinity/1, timestamp/1, regex/1, wrap/1, js/1, string/1, int/1, float/1, bool/1, bit_array/1, time/1, object_id/1, list/1, dict/1]).

-spec min(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
min(Dyn) ->
    case gleam@dynamic:from(min) =:= Dyn of
        true ->
            {ok, min};

        false ->
            {error,
                [{decode_error, <<"Min"/utf8>>, gleam@dynamic:classify(Dyn), []}]}
    end.

-spec max(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
max(Dyn) ->
    case gleam@dynamic:from(max) =:= Dyn of
        true ->
            {ok, max};

        false ->
            {error,
                [{decode_error, <<"Max"/utf8>>, gleam@dynamic:classify(Dyn), []}]}
    end.

-spec nan(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
nan(Dyn) ->
    case gleam@dynamic:from(na_n) =:= Dyn of
        true ->
            {ok, na_n};

        false ->
            {error,
                [{decode_error, <<"NaN"/utf8>>, gleam@dynamic:classify(Dyn), []}]}
    end.

-spec nil(gleam@dynamic:dynamic_()) -> {ok, nil} |
    {error, list(gleam@dynamic:decode_error())}.
nil(Dyn) ->
    case gleam@dynamic:from(null) =:= Dyn of
        true ->
            {ok, nil};

        false ->
            {error,
                [{decode_error,
                        <<"Null"/utf8>>,
                        gleam@dynamic:classify(Dyn),
                        []}]}
    end.

-spec infinity(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
infinity(Dyn) ->
    case gleam@dynamic:from(infinity) =:= Dyn of
        true ->
            {ok, infinity};

        false ->
            {error,
                [{decode_error,
                        <<"Infinity"/utf8>>,
                        gleam@dynamic:classify(Dyn),
                        []}]}
    end.

-spec negative_infinity(gleam@dynamic:dynamic_()) -> {ok, bison@bson:value()} |
    {error, list(gleam@dynamic:decode_error())}.
negative_infinity(Dyn) ->
    case gleam@dynamic:from(negative_infinity) =:= Dyn of
        true ->
            {ok, negative_infinity};

        false ->
            {error,
                [{decode_error,
                        <<"Negative Infinity"/utf8>>,
                        gleam@dynamic:classify(Dyn),
                        []}]}
    end.

-spec timestamp(gleam@dynamic:dynamic_()) -> {ok, {integer(), integer()}} |
    {error, list(gleam@dynamic:decode_error())}.
timestamp(Dyn) ->
    gleam@result:then(
        (gleam@dynamic:element(1, fun gleam@dynamic:int/1))(Dyn),
        fun(Timestamp) ->
            gleam@result:then(
                (gleam@dynamic:element(2, fun gleam@dynamic:int/1))(Dyn),
                fun(Counter) -> {ok, {Timestamp, Counter}} end
            )
        end
    ).

-spec regex(gleam@dynamic:dynamic_()) -> {ok, {binary(), binary()}} |
    {error, list(gleam@dynamic:decode_error())}.
regex(Dyn) ->
    gleam@result:then(
        (gleam@dynamic:element(1, fun gleam@dynamic:string/1))(Dyn),
        fun(Pattern) ->
            gleam@result:then(
                (gleam@dynamic:element(2, fun gleam@dynamic:string/1))(Dyn),
                fun(Options) -> {ok, {Pattern, Options}} end
            )
        end
    ).

-spec wrap(
    fun((gleam@dynamic:dynamic_()) -> {ok, INL} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, INL} |
    {error, list(gleam@dynamic:decode_error())}).
wrap(Decoder) ->
    gleam@dynamic:element(1, Decoder).

-spec js(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, list(gleam@dynamic:decode_error())}.
js(Dyn) ->
    (wrap(fun gleam@dynamic:string/1))(Dyn).

-spec string(gleam@dynamic:dynamic_()) -> {ok, binary()} |
    {error, list(gleam@dynamic:decode_error())}.
string(Dyn) ->
    (wrap(fun gleam@dynamic:string/1))(Dyn).

-spec int(gleam@dynamic:dynamic_()) -> {ok, integer()} |
    {error, list(gleam@dynamic:decode_error())}.
int(Dyn) ->
    (wrap(fun gleam@dynamic:int/1))(Dyn).

-spec float(gleam@dynamic:dynamic_()) -> {ok, float()} |
    {error, list(gleam@dynamic:decode_error())}.
float(Dyn) ->
    (wrap(fun gleam@dynamic:float/1))(Dyn).

-spec bool(gleam@dynamic:dynamic_()) -> {ok, boolean()} |
    {error, list(gleam@dynamic:decode_error())}.
bool(Dyn) ->
    (wrap(fun gleam@dynamic:bool/1))(Dyn).

-spec bit_array(gleam@dynamic:dynamic_()) -> {ok, bitstring()} |
    {error, list(gleam@dynamic:decode_error())}.
bit_array(Dyn) ->
    begin
        _pipe = gleam@dynamic:any(
            [gleam@dynamic:element(2, fun gleam@dynamic:bit_array/1),
                wrap(fun gleam@dynamic:bit_array/1)]
        ),
        _pipe@1 = wrap(_pipe),
        wrap(_pipe@1)
    end(Dyn).

-spec time(gleam@dynamic:dynamic_()) -> {ok, birl:time()} |
    {error, list(gleam@dynamic:decode_error())}.
time(Dyn) ->
    gleam@result:then(
        (wrap(gleam@dynamic:element(1, fun gleam@dynamic:int/1)))(Dyn),
        fun(Value) ->
            {ok,
                birl:add(
                    {time, 0, 0, none, none},
                    birl@duration:accurate_new([{Value, micro_second}])
                )}
        end
    ).

-spec object_id(gleam@dynamic:dynamic_()) -> {ok, bison@object_id:object_id()} |
    {error, list(gleam@dynamic:decode_error())}.
object_id(Dyn) ->
    gleam@result:then(
        (wrap(gleam@dynamic:element(1, fun gleam@dynamic:bit_array/1)))(Dyn),
        fun(Value) ->
            gleam@result:replace_error(
                bison@object_id:from_bit_array(Value),
                [{decode_error, <<"object id"/utf8>>, <<"bit array"/utf8>>, []}]
            )
        end
    ).

-spec list(
    fun((gleam@dynamic:dynamic_()) -> {ok, IOU} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, list(IOU)} |
    {error, list(gleam@dynamic:decode_error())}).
list(Value_decoder) ->
    wrap(gleam@dynamic:list(Value_decoder)).

-spec dict(
    fun((gleam@dynamic:dynamic_()) -> {ok, IOX} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, gleam@dict:dict(binary(), IOX)} |
    {error, list(gleam@dynamic:decode_error())}).
dict(Value_decoder) ->
    wrap(gleam@dynamic:dict(fun gleam@dynamic:string/1, Value_decoder)).
