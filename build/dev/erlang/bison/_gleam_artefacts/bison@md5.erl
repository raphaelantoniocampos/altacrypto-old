-module(bison@md5).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_bit_array/1, from_int_list/1, from_bit_array/1, to_int_list/1, from_string/1, to_string/1]).
-export_type([md5/0]).

-opaque md5() :: {md5, bitstring()}.

-spec to_bit_array(md5()) -> bitstring().
to_bit_array(Md5) ->
    case Md5 of
        {md5, Value} ->
            Value
    end.

-spec from_int_list(list(integer())) -> {ok, md5()} | {error, nil}.
from_int_list(Md5) ->
    case gleam@list:length(Md5) of
        16 ->
            case gleam@list:try_fold(
                Md5,
                <<>>,
                fun(Acc, Code) -> case (Code >= 0) andalso (Code =< 255) of
                        true ->
                            {ok, gleam@bit_array:append(Acc, <<Code>>)};

                        false ->
                            {error, nil}
                    end end
            ) of
                {ok, Md5@1} ->
                    {ok, {md5, Md5@1}};

                {error, nil} ->
                    {error, nil}
            end;

        32 ->
            case gleam@list:try_map(
                Md5,
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
                                                module => <<"bison/md5"/utf8>>,
                                                function => <<"from_int_list"/utf8>>,
                                                line => 81})
                            end,
                            _assert_subject = <<High:4, Low:4>>,
                            <<Num:8>> = case _assert_subject of
                                <<_:8>> -> _assert_subject;
                                _assert_fail@1 ->
                                    erlang:error(#{gleam_error => let_assert,
                                                message => <<"Assertion pattern match failed"/utf8>>,
                                                value => _assert_fail@1,
                                                module => <<"bison/md5"/utf8>>,
                                                function => <<"from_int_list"/utf8>>,
                                                line => 82})
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
                    _pipe@4 = {md5, _pipe@3},
                    {ok, _pipe@4};

                {error, nil} ->
                    {error, nil}
            end;

        _ ->
            {error, nil}
    end.

-spec from_bit_array(bitstring()) -> {ok, md5()} | {error, nil}.
from_bit_array(Md5) ->
    case erlang:byte_size(Md5) of
        16 ->
            {ok, {md5, Md5}};

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
                        module => <<"bison/md5"/utf8>>,
                        function => <<"to_int_list_internal"/utf8>>,
                        line => 120})
    end,
    New_storage = gleam@queue:push_back(Storage, Num),
    case erlang:byte_size(Remaining@1) of
        0 ->
            gleam@queue:to_list(New_storage);

        _ ->
            to_int_list_internal(Remaining@1, New_storage)
    end.

-spec to_int_list(md5()) -> list(integer()).
to_int_list(Md5) ->
    case Md5 of
        {md5, Value} ->
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
                        module => <<"bison/md5"/utf8>>,
                        function => <<"to_digit"/utf8>>,
                        line => 129})
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
                                module => <<"bison/md5"/utf8>>,
                                function => <<"to_digit"/utf8>>,
                                line => 133})
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
                                module => <<"bison/md5"/utf8>>,
                                function => <<"to_digit"/utf8>>,
                                line => 138})
            end,
            {ok, 9 + Additive};

        _ ->
            {error, nil}
    end.

-spec from_string(binary()) -> {ok, md5()} | {error, nil}.
from_string(Md5) ->
    case gleam@string:length(Md5) =:= 32 of
        true ->
            case begin
                _pipe = Md5,
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
                                                module => <<"bison/md5"/utf8>>,
                                                function => <<"from_string"/utf8>>,
                                                line => 40})
                            end,
                            <<High:4, Low:4>>
                        end
                    ),
                    _pipe@5 = gleam_stdlib:bit_array_concat(_pipe@4),
                    _pipe@6 = {md5, _pipe@5},
                    {ok, _pipe@6};

                {error, nil} ->
                    {error, nil}
            end;

        false ->
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
                        module => <<"bison/md5"/utf8>>,
                        function => <<"to_char"/utf8>>,
                        line => 151})
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
                        module => <<"bison/md5"/utf8>>,
                        function => <<"to_string_internal"/utf8>>,
                        line => 103})
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

-spec to_string(md5()) -> binary().
to_string(Md5) ->
    case Md5 of
        {md5, Value} ->
            to_string_internal(Value, <<""/utf8>>)
    end.
