-module(bison@ejson).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_canonical/1, from_canonical/1]).

-spec to_canonical(gleam@dict:dict(binary(), bison@bson:value())) -> binary().
to_canonical(Doc) ->
    bison@ejson@encoder:to_canonical(Doc).

-spec from_canonical(binary()) -> {ok,
        gleam@dict:dict(binary(), bison@bson:value())} |
    {error, gleam@json:decode_error()}.
from_canonical(Doc) ->
    bison@ejson@decoder:from_canonical(Doc).
