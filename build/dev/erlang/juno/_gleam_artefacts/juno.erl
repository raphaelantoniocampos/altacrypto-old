-module(juno).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([decode/2, decode_object/2]).
-export_type([value/1]).

-type value(HEK) :: null |
    {int, integer()} |
    {custom, HEK} |
    {bool, boolean()} |
    {float, float()} |
    {string, binary()} |
    {array, list(value(HEK))} |
    {object, gleam@dict:dict(binary(), value(HEK))}.

-spec wrap(
    fun((gleam@dynamic:dynamic_()) -> {ok, HFO} |
        {error, list(gleam@dynamic:decode_error())})
) -> fun((gleam@dynamic:dynamic_()) -> {ok, value(HFO)} |
    {error, list(gleam@dynamic:decode_error())}).
wrap(Decoder) ->
    fun(_capture) ->
        (gleam@dynamic:decode1(fun(Field@0) -> {custom, Field@0} end, Decoder))(
            _capture
        )
    end.

-spec int() -> fun((gleam@dynamic:dynamic_()) -> {ok, value(any())} |
    {error, list(gleam@dynamic:decode_error())}).
int() ->
    fun(_capture) ->
        (gleam@dynamic:decode1(
            fun(Field@0) -> {int, Field@0} end,
            fun gleam@dynamic:int/1
        ))(_capture)
    end.

-spec float() -> fun((gleam@dynamic:dynamic_()) -> {ok, value(any())} |
    {error, list(gleam@dynamic:decode_error())}).
float() ->
    fun(_capture) ->
        (gleam@dynamic:decode1(
            fun(Field@0) -> {float, Field@0} end,
            fun gleam@dynamic:float/1
        ))(_capture)
    end.

-spec bool() -> fun((gleam@dynamic:dynamic_()) -> {ok, value(any())} |
    {error, list(gleam@dynamic:decode_error())}).
bool() ->
    fun(_capture) ->
        (gleam@dynamic:decode1(
            fun(Field@0) -> {bool, Field@0} end,
            fun gleam@dynamic:bool/1
        ))(_capture)
    end.

-spec string() -> fun((gleam@dynamic:dynamic_()) -> {ok, value(any())} |
    {error, list(gleam@dynamic:decode_error())}).
string() ->
    fun(_capture) ->
        (gleam@dynamic:decode1(
            fun(Field@0) -> {string, Field@0} end,
            fun gleam@dynamic:string/1
        ))(_capture)
    end.

-spec object(
    list(fun((gleam@dynamic:dynamic_()) -> {ok, value(HHF)} |
        {error, list(gleam@dynamic:decode_error())}))
) -> fun((gleam@dynamic:dynamic_()) -> {ok, value(HHF)} |
    {error, list(gleam@dynamic:decode_error())}).
object(Custom_decoders) ->
    fun(_capture) ->
        (gleam@dynamic:decode1(
            fun(Field@0) -> {object, Field@0} end,
            gleam@dynamic:dict(
                fun gleam@dynamic:string/1,
                fun(_capture@1) -> value(_capture@1, Custom_decoders) end
            )
        ))(_capture)
    end.

-spec value(
    gleam@dynamic:dynamic_(),
    list(fun((gleam@dynamic:dynamic_()) -> {ok, value(HHF)} |
        {error, list(gleam@dynamic:decode_error())}))
) -> {ok, value(HHF)} | {error, list(gleam@dynamic:decode_error())}.
value(Dyn, Custom_decoders) ->
    Value_decoders = lists:append(
        Custom_decoders,
        [int(),
            bool(),
            float(),
            string(),
            array(Custom_decoders),
            object(Custom_decoders)]
    ),
    case (gleam@dynamic:optional(gleam@dynamic:any(Value_decoders)))(Dyn) of
        {ok, {some, Value}} ->
            {ok, Value};

        {ok, none} ->
            {ok, null};

        {error, Error} ->
            {error, Error}
    end.

-spec decode(
    binary(),
    list(fun((gleam@dynamic:dynamic_()) -> {ok, HEL} |
        {error, list(gleam@dynamic:decode_error())}))
) -> {ok, value(HEL)} | {error, gleam@json:decode_error()}.
decode(Json, Custom_decoders) ->
    gleam@json:decode(
        Json,
        fun(_capture) ->
            value(_capture, gleam@list:map(Custom_decoders, fun wrap/1))
        end
    ).

-spec decode_object(
    binary(),
    list(fun((gleam@dynamic:dynamic_()) -> {ok, HEP} |
        {error, list(gleam@dynamic:decode_error())}))
) -> {ok, value(HEP)} | {error, gleam@json:decode_error()}.
decode_object(Json, Custom_decoders) ->
    gleam@json:decode(Json, object(gleam@list:map(Custom_decoders, fun wrap/1))).

-spec array(
    list(fun((gleam@dynamic:dynamic_()) -> {ok, value(HHF)} |
        {error, list(gleam@dynamic:decode_error())}))
) -> fun((gleam@dynamic:dynamic_()) -> {ok, value(HHF)} |
    {error, list(gleam@dynamic:decode_error())}).
array(Custom_decoders) ->
    fun(_capture) ->
        (gleam@dynamic:decode1(
            fun(Field@0) -> {array, Field@0} end,
            gleam@dynamic:list(
                fun(_capture@1) -> value(_capture@1, Custom_decoders) end
            )
        ))(_capture)
    end.
