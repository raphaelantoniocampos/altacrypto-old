-module(mungo@tcp).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([connect/3, execute/3]).

-spec connect(binary(), integer(), integer()) -> {ok, mug:socket()} |
    {error, mug:error()}.
connect(Host, Port, Timeout) ->
    mug:connect({connection_options, Host, Port, Timeout}).

-spec send(mug:socket(), bitstring()) -> {ok, nil} | {error, mug:error()}.
send(Socket, Packet) ->
    mug:send(Socket, Packet).

-spec mapper(mug:tcp_message()) -> {ok, bitstring()} | {error, mug:error()}.
mapper(Message) ->
    case Message of
        {packet, _, Packet} ->
            {ok, Packet};

        {socket_closed, _} ->
            {error, closed};

        {tcp_error, _, Error} ->
            {error, Error}
    end.

-spec internal_receive(
    mug:socket(),
    gleam@erlang@process:selector({ok, bitstring()} | {error, mug:error()}),
    integer(),
    integer(),
    gleam@option:option(integer()),
    bitstring()
) -> {ok, bitstring()} | {error, mug:error()}.
internal_receive(Socket, Selector, Start_time, Timeout, Remaining_size, Storage) ->
    mug:receive_next_packet_as_message(Socket),
    _pipe = Selector,
    _pipe@1 = gleam_erlang_ffi:select(_pipe, Timeout),
    _pipe@2 = gleam@result:replace_error(_pipe@1, timeout),
    _pipe@3 = gleam@result:flatten(_pipe@2),
    _pipe@4 = gleam@result:map(
        _pipe@3,
        fun(Packet) -> gleam@result:then(case Remaining_size of
                    none ->
                        case erlang:byte_size(Packet) > 4 of
                            true ->
                                <<Size:32/little, _/bitstring>> = case Packet of
                                    <<_:32/little, _/bitstring>> -> Packet;
                                    _assert_fail ->
                                        erlang:error(
                                                #{gleam_error => let_assert,
                                                    message => <<"Assertion pattern match failed"/utf8>>,
                                                    value => _assert_fail,
                                                    module => <<"mungo/tcp"/utf8>>,
                                                    function => <<"internal_receive"/utf8>>,
                                                    line => 46}
                                            )
                                end,
                                {ok, Size - erlang:byte_size(Packet)};

                            false ->
                                {error, ebadmsg}
                        end;

                    {some, Remaining_size@1} ->
                        {ok, Remaining_size@1 - erlang:byte_size(Packet)}
                end, fun(Remaining_size@2) ->
                    Storage@1 = gleam@bit_array:append(Storage, Packet),
                    case (erlang:monotonic_time() - Start_time) >= (Timeout * 1000000) of
                        true ->
                            {error, timeout};

                        false ->
                            case gleam@int:compare(Remaining_size@2, 0) of
                                eq ->
                                    {ok, Storage@1};

                                lt ->
                                    {error, ebadmsg};

                                gt ->
                                    internal_receive(
                                        Socket,
                                        Selector,
                                        Start_time,
                                        Timeout,
                                        {some, Remaining_size@2},
                                        Storage@1
                                    )
                            end
                    end
                end) end
    ),
    gleam@result:flatten(_pipe@4).

-spec execute(mug:socket(), bitstring(), integer()) -> {ok, bitstring()} |
    {error, mug:error()}.
execute(Socket, Packet, Timeout) ->
    Selector = begin
        _pipe = gleam_erlang_ffi:new_selector(),
        mug:selecting_tcp_messages(_pipe, fun mapper/1)
    end,
    gleam@result:then(
        send(Socket, Packet),
        fun(_) ->
            internal_receive(
                Socket,
                Selector,
                erlang:monotonic_time(),
                Timeout,
                none,
                <<>>
            )
        end
    ).
