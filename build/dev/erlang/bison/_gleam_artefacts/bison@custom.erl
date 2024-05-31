-module(bison@custom).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_bit_array_with_code/1, from_bit_array_with_code/2]).
-export_type([custom/0]).

-opaque custom() :: {custom, bitstring(), bitstring()}.

-spec to_bit_array_with_code(custom()) -> {integer(), bitstring()}.
to_bit_array_with_code(Custom) ->
    {custom, <<Code>>, Value} = case Custom of
        {custom, <<_>>, _} -> Custom;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/custom"/utf8>>,
                        function => <<"to_bit_array_with_code"/utf8>>,
                        line => 10})
    end,
    {Code, Value}.

-spec from_bit_array_with_code(integer(), bitstring()) -> {ok, custom()} |
    {error, nil}.
from_bit_array_with_code(Code, Value) ->
    case (Code =< 16#ff) andalso (Code >= 16#80) of
        true ->
            {ok, {custom, <<Code>>, Value}};

        false ->
            {error, nil}
    end.
