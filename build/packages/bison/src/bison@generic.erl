-module(bison@generic).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_string/1, to_bit_array/1, from_string/1, from_int_list/1, to_int_list/1, from_bit_array/1]).
-export_type([generic/0]).

-opaque generic() :: {generic, bitstring()}.

-spec to_string(generic()) -> {ok, binary()} | {error, nil}.
to_string(Generic) ->
    case Generic of
        {generic, Binary} ->
            gleam@bit_array:to_string(Binary)
    end.

-spec to_bit_array(generic()) -> bitstring().
to_bit_array(Generic) ->
    case Generic of
        {generic, Binary} ->
            Binary
    end.

-spec from_string(binary()) -> generic().
from_string(Binary) ->
    {generic, gleam_stdlib:identity(Binary)}.

-spec from_int_list(list(integer())) -> generic().
from_int_list(Binary) ->
    _pipe = Binary,
    _pipe@1 = gleam@list:fold(
        _pipe,
        <<>>,
        fun(Acc, Code) -> gleam@bit_array:append(Acc, <<Code>>) end
    ),
    {generic, _pipe@1}.

-spec to_int_list_internal(bitstring(), gleam@queue:queue(integer())) -> list(integer()).
to_int_list_internal(Remaining, Storage) ->
    <<Num:8, Remaining@1/binary>> = case Remaining of
        <<_:8, _/binary>> -> Remaining;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/generic"/utf8>>,
                        function => <<"to_int_list_internal"/utf8>>,
                        line => 48})
    end,
    New_storage = gleam@queue:push_back(Storage, Num),
    case erlang:byte_size(Remaining@1) =:= 0 of
        true ->
            gleam@queue:to_list(New_storage);

        false ->
            to_int_list_internal(Remaining@1, New_storage)
    end.

-spec to_int_list(generic()) -> list(integer()).
to_int_list(Generic) ->
    case Generic of
        {generic, Binary} ->
            to_int_list_internal(Binary, gleam@queue:new())
    end.

-spec from_bit_array(bitstring()) -> {ok, generic()} | {error, nil}.
from_bit_array(Binary) ->
    case erlang:bit_size(Binary) rem 8 of
        0 ->
            {ok, {generic, Binary}};

        _ ->
            {error, nil}
    end.
