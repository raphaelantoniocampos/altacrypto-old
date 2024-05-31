-module(bison@object_id).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_datetime/1, compare/2, to_bit_array/1, from_int_list/1, from_bit_array/1, to_int_list/1, from_string/1, to_string/1, from_datetime/1, new/0, range/3]).
-export_type([object_id/0]).

-opaque object_id() :: {object_id, bitstring()}.

-spec to_datetime(object_id()) -> birl:time().
to_datetime(Id) ->
    {object_id, <<Timestamp:32/big, _/bitstring>>} = case Id of
        {object_id, <<_:32/big, _/bitstring>>} -> Id;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"to_datetime"/utf8>>,
                        line => 41})
    end,
    birl:from_unix(Timestamp).

-spec compare(object_id(), object_id()) -> gleam@order:order().
compare(A, B) ->
    {object_id, <<Moment_a:32/big, _:24/big, _:16/big, Counter_a:24/big>>} = case A of
        {object_id, <<_:32/big, _:24/big, _:16/big, _:24/big>>} -> A;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"compare"/utf8>>,
                        line => 60})
    end,
    {object_id, <<Moment_b:32/big, _:24/big, _:16/big, Counter_b:24/big>>} = case B of
        {object_id, <<_:32/big, _:24/big, _:16/big, _:24/big>>} -> B;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"compare"/utf8>>,
                        line => 62})
    end,
    case Moment_a =:= Moment_b of
        true ->
            case Counter_a =:= Counter_b of
                true ->
                    eq;

                false ->
                    case Counter_a < Counter_b of
                        true ->
                            lt;

                        false ->
                            gt
                    end
            end;

        false ->
            case Moment_a < Moment_b of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

-spec to_bit_array(object_id()) -> bitstring().
to_bit_array(Id) ->
    case Id of
        {object_id, Value} ->
            Value
    end.

-spec from_int_list(list(integer())) -> {ok, object_id()} | {error, nil}.
from_int_list(Id) ->
    case erlang:length(Id) of
        12 ->
            case gleam@list:try_fold(
                Id,
                <<>>,
                fun(Acc, Code) -> case (Code >= 0) andalso (Code =< 255) of
                        true ->
                            {ok, gleam@bit_array:append(Acc, <<Code>>)};

                        false ->
                            {error, nil}
                    end end
            ) of
                {ok, Id@1} ->
                    {ok, {object_id, Id@1}};

                {error, nil} ->
                    {error, nil}
            end;

        24 ->
            case gleam@list:try_map(
                Id,
                fun(Code@1) -> case (Code@1 >= 0) andalso (Code@1 =< 15) of
                        true ->
                            {ok, Code@1};

                        false ->
                            {error, nil}
                    end end
            ) of
                {ok, Codes} ->
                    _pipe = Codes,
                    _pipe@1 = gleam@list:sized_chunk(_pipe, 2),
                    _pipe@2 = gleam@list:map(
                        _pipe@1,
                        fun(Pair) ->
                            [High, Low] = case Pair of
                                [_, _] -> Pair;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail,
                                                module => <<"bison/object_id"/utf8>>,
                                                function => <<"from_int_list"/utf8>>,
                                                line => 155})
                            end,
                            _assert_subject = <<High:4, Low:4>>,
                            <<Num:8>> = case _assert_subject of
                                <<_:8>> -> _assert_subject;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"bison/object_id"/utf8>>,
                                                function => <<"from_int_list"/utf8>>,
                                                line => 156})
                            end,
                            Num
                        end
                    ),
                    _pipe@3 = gleam@list:fold(
                        _pipe@2,
                        <<>>,
                        fun(Acc@1, Code@2) ->
                            gleam@bit_array:append(Acc@1, <<Code@2>>)
                        end
                    ),
                    _pipe@4 = {object_id, _pipe@3},
                    {ok, _pipe@4};

                {error, nil} ->
                    {error, nil}
            end;

        _ ->
            {error, nil}
    end.

-spec from_bit_array(bitstring()) -> {ok, object_id()} | {error, nil}.
from_bit_array(Id) ->
    case erlang:byte_size(Id) of
        12 ->
            {ok, {object_id, Id}};

        _ ->
            {error, nil}
    end.

-spec to_int_list_internal(bitstring(), gleam@queue:queue(integer())) -> list(integer()).
to_int_list_internal(Remaining, Storage) ->
    <<Num:8, Remaining@1/binary>> = case Remaining of
        <<_:8, _/binary>> -> Remaining;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"to_int_list_internal"/utf8>>,
                        line => 194})
    end,
    New_storage = gleam@queue:push_back(Storage, Num),
    case erlang:byte_size(Remaining@1) of
        0 ->
            gleam@queue:to_list(New_storage);

        _ ->
            to_int_list_internal(Remaining@1, New_storage)
    end.

-spec to_int_list(object_id()) -> list(integer()).
to_int_list(Id) ->
    case Id of
        {object_id, Value} ->
            to_int_list_internal(Value, gleam@queue:new())
    end.

-spec to_digit(binary()) -> {ok, integer()} | {error, nil}.
to_digit(Char) ->
    _assert_subject = gleam_stdlib:identity(Char),
    <<Code>> = case _assert_subject of
        <<_>> -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"to_digit"/utf8>>,
                        line => 205})
    end,
    case Code of
        Code@1 when (Code@1 >= 48) andalso (Code@1 =< 57) ->
            _assert_subject@1 = gleam_stdlib:identity(Char),
            <<_:4, Num:4>> = case _assert_subject@1 of
                <<_:4, _:4>> -> _assert_subject@1;
                _assert_fail@1 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@1,
                                module => <<"bison/object_id"/utf8>>,
                                function => <<"to_digit"/utf8>>,
                                line => 209})
            end,
            {ok, Num};

        Code@2 when ((Code@2 >= 65) andalso (Code@2 =< 70)) orelse ((Code@2 >= 97) andalso (Code@2 =< 102)) ->
            _assert_subject@2 = gleam_stdlib:identity(Char),
            <<_:5, Additive:3>> = case _assert_subject@2 of
                <<_:5, _:3>> -> _assert_subject@2;
                _assert_fail@2 ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail@2,
                                module => <<"bison/object_id"/utf8>>,
                                function => <<"to_digit"/utf8>>,
                                line => 214})
            end,
            {ok, 9 + Additive};

        _ ->
            {error, nil}
    end.

-spec from_string(binary()) -> {ok, object_id()} | {error, nil}.
from_string(Id) ->
    case gleam@string:length(Id) of
        24 ->
            case begin
                _pipe = Id,
                _pipe@1 = gleam@string:to_graphemes(_pipe),
                gleam@list:try_map(_pipe@1, fun to_digit/1)
            end of
                {ok, Codes} ->
                    _pipe@2 = Codes,
                    _pipe@3 = gleam@list:sized_chunk(_pipe@2, 2),
                    _pipe@4 = gleam@list:map(
                        _pipe@3,
                        fun(Pair) ->
                            [High, Low] = case Pair of
                                [_, _] -> Pair;
                                _assert_fail ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail,
                                                module => <<"bison/object_id"/utf8>>,
                                                function => <<"from_string"/utf8>>,
                                                line => 114})
                            end,
                            <<High:4, Low:4>>
                        end
                    ),
                    _pipe@5 = gleam_stdlib:bit_array_concat(_pipe@4),
                    _pipe@6 = {object_id, _pipe@5},
                    {ok, _pipe@6};

                {error, nil} ->
                    {error, nil}
            end;

        _ ->
            {error, nil}
    end.

-spec to_char(integer()) -> binary().
to_char(Digit) ->
    Ch = case Digit < 10 of
        true ->
            Digit + 48;

        false ->
            Digit + 87
    end,
    _assert_subject = gleam@bit_array:to_string(<<Ch>>),
    {ok, Digit@1} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"to_char"/utf8>>,
                        line => 227})
    end,
    Digit@1.

-spec to_string_internal(bitstring(), binary()) -> binary().
to_string_internal(Remaining, Storage) ->
    <<High:4, Low:4, Remaining@1/binary>> = case Remaining of
        <<_:4, _:4, _/binary>> -> Remaining;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"to_string_internal"/utf8>>,
                        line => 177})
    end,
    New_storage = begin
        _pipe = Storage,
        _pipe@1 = gleam@string:append(_pipe, to_char(High)),
        gleam@string:append(_pipe@1, to_char(Low))
    end,
    case erlang:byte_size(Remaining@1) of
        0 ->
            New_storage;

        _ ->
            to_string_internal(Remaining@1, New_storage)
    end.

-spec to_string(object_id()) -> binary().
to_string(Id) ->
    case Id of
        {object_id, Value} ->
            to_string_internal(Value, <<""/utf8>>)
    end.

-spec from_datetime(birl:time()) -> object_id().
from_datetime(Datetime) ->
    Moment = birl:to_unix(Datetime),
    _assert_subject = gleam@int:modulo(birl:monotonic_now(), 16#ffffff),
    {ok, Counter} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"from_datetime"/utf8>>,
                        line => 24})
    end,
    _assert_subject@1 = bison_ffi:hash(bison_ffi:get_hostname()),
    <<Machine_id:24, _/bitstring>> = case _assert_subject@1 of
        <<_:24, _/bitstring>> -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"from_datetime"/utf8>>,
                        line => 26})
    end,
    _assert_subject@2 = from_bit_array(
        <<Moment:32/big,
            Machine_id:24/big,
            (bison_ffi:get_pid()):16/big,
            Counter:24/big>>
    ),
    {ok, Id} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"bison/object_id"/utf8>>,
                        function => <<"from_datetime"/utf8>>,
                        line => 28})
    end,
    Id.

-spec new() -> object_id().
new() ->
    from_datetime(birl:utc_now()).

-spec range(
    object_id(),
    gleam@option:option(object_id()),
    birl@duration:duration()
) -> gleam@iterator:iterator(object_id()).
range(A, B, S) ->
    _pipe = to_datetime(A),
    _pipe@1 = birl:range(_pipe, gleam@option:map(B, fun to_datetime/1), S),
    gleam@iterator:map(_pipe@1, fun from_datetime/1).
