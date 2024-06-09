-module(bison).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([encode/1, encode_list/1, decode/1, to_custom_type/2]).

-spec encode(gleam@dict:dict(binary(), bison@bson:value())) -> bitstring().
encode(Doc) ->
    bison@encoder:encode(Doc).

-spec encode_list(list({binary(), bison@bson:value()})) -> bitstring().
encode_list(Doc) ->
    bison@encoder:encode_list(Doc).

-spec decode(bitstring()) -> {ok, gleam@dict:dict(binary(), bison@bson:value())} |
    {error, nil}.
decode(Binary) ->
    bison@decoder:decode(Binary).

-spec to_custom_type(
    gleam@dict:dict(binary(), bison@bson:value()),
    fun((gleam@dynamic:dynamic_()) -> {ok, IDL} |
        {error, list(gleam@dynamic:decode_error())})
) -> {ok, IDL} | {error, list(gleam@dynamic:decode_error())}.
to_custom_type(Doc, Decoder) ->
    _pipe = Doc,
    _pipe@1 = gleam@dynamic:from(_pipe),
    Decoder(_pipe@1).
