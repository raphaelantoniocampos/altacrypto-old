-module(mug).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/2, timeout/2, connect/1, send_builder/2, send/2, 'receive'/2, shutdown/1, receive_next_packet_as_message/1, selecting_tcp_messages/2]).
-export_type([socket/0, do_not_leak/0, error/0, connection_options/0, gen_tcp_option_name/0, mode_value/0, active_value/0, tcp_message/0]).

-type socket() :: any().

-type do_not_leak() :: any().

-type error() :: closed |
    timeout |
    eaddrinuse |
    eaddrnotavail |
    eafnosupport |
    ealready |
    econnaborted |
    econnrefused |
    econnreset |
    edestaddrreq |
    ehostdown |
    ehostunreach |
    einprogress |
    eisconn |
    emsgsize |
    enetdown |
    enetunreach |
    enopkg |
    enoprotoopt |
    enotconn |
    enotty |
    enotsock |
    eproto |
    eprotonosupport |
    eprototype |
    esocktnosupport |
    etimedout |
    ewouldblock |
    exbadport |
    exbadseq |
    nxdomain |
    eacces |
    eagain |
    ebadf |
    ebadmsg |
    ebusy |
    edeadlk |
    edeadlock |
    edquot |
    eexist |
    efault |
    efbig |
    eftype |
    eintr |
    einval |
    eio |
    eisdir |
    eloop |
    emfile |
    emlink |
    emultihop |
    enametoolong |
    enfile |
    enobufs |
    enodev |
    enolck |
    enolink |
    enoent |
    enomem |
    enospc |
    enosr |
    enostr |
    enosys |
    enotblk |
    enotdir |
    enotsup |
    enxio |
    eopnotsupp |
    eoverflow |
    eperm |
    epipe |
    erange |
    erofs |
    espipe |
    esrch |
    estale |
    etxtbsy |
    exdev.

-type connection_options() :: {connection_options,
        binary(),
        integer(),
        integer()}.

-type gen_tcp_option_name() :: active | mode.

-type mode_value() :: binary.

-type active_value() :: once.

-type tcp_message() :: {packet, socket(), bitstring()} |
    {socket_closed, socket()} |
    {tcp_error, socket(), error()}.

-spec new(binary(), integer()) -> connection_options().
new(Host, Port) ->
    {connection_options, Host, Port, 1000}.

-spec timeout(connection_options(), integer()) -> connection_options().
timeout(Options, Timeout) ->
    erlang:setelement(4, Options, Timeout).

-spec connect(connection_options()) -> {ok, socket()} | {error, error()}.
connect(Options) ->
    Gen_options = [{active, gleam@dynamic:from(false)},
        {mode, gleam@dynamic:from(binary)}],
    Host = unicode:characters_to_list(erlang:element(2, Options)),
    gen_tcp:connect(
        Host,
        erlang:element(3, Options),
        Gen_options,
        erlang:element(4, Options)
    ).

-spec send_builder(socket(), gleam@bytes_builder:bytes_builder()) -> {ok, nil} |
    {error, error()}.
send_builder(Socket, Packet) ->
    mug_ffi:send(Socket, Packet).

-spec send(socket(), bitstring()) -> {ok, nil} | {error, error()}.
send(Socket, Packet) ->
    mug_ffi:send(Socket, gleam_stdlib:wrap_list(Packet)).

-spec 'receive'(socket(), integer()) -> {ok, bitstring()} | {error, error()}.
'receive'(Socket, Timeout) ->
    gen_tcp:recv(Socket, 0, Timeout).

-spec shutdown(socket()) -> {ok, nil} | {error, error()}.
shutdown(Socket) ->
    mug_ffi:shutdown(Socket).

-spec receive_next_packet_as_message(socket()) -> nil.
receive_next_packet_as_message(Socket) ->
    inet:setopts(Socket, [{active, gleam@dynamic:from(once)}]),
    nil.

-spec unsafe_coerce_to_socket(gleam@dynamic:dynamic_()) -> socket().
unsafe_coerce_to_socket(Socket) ->
    gleam@dynamic:unsafe_coerce(Socket).

-spec unsafe_coerce_packet(fun((tcp_message()) -> YCQ)) -> fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> YCQ).
unsafe_coerce_packet(Mapper) ->
    fun(Socket, Data) ->
        _pipe = {packet,
            unsafe_coerce_to_socket(Socket),
            gleam@dynamic:unsafe_coerce(Data)},
        Mapper(_pipe)
    end.

-spec unsafe_coerce_closed(fun((tcp_message()) -> YCR)) -> fun((gleam@dynamic:dynamic_()) -> YCR).
unsafe_coerce_closed(Mapper) ->
    fun(Socket) -> _pipe = {socket_closed, unsafe_coerce_to_socket(Socket)},
        Mapper(_pipe) end.

-spec unsafe_coerce_to_tcp_error(fun((tcp_message()) -> YCS)) -> fun((gleam@dynamic:dynamic_(), gleam@dynamic:dynamic_()) -> YCS).
unsafe_coerce_to_tcp_error(Mapper) ->
    fun(Socket, Reason) ->
        Mapper(
            {tcp_error,
                unsafe_coerce_to_socket(Socket),
                gleam@dynamic:unsafe_coerce(Reason)}
        )
    end.

-spec selecting_tcp_messages(
    gleam@erlang@process:selector(YCN),
    fun((tcp_message()) -> YCN)
) -> gleam@erlang@process:selector(YCN).
selecting_tcp_messages(Selector, Mapper) ->
    Tcp = erlang:binary_to_atom(<<"tcp"/utf8>>),
    Closed = erlang:binary_to_atom(<<"tcp_closed"/utf8>>),
    Error = erlang:binary_to_atom(<<"tcp_error"/utf8>>),
    _pipe = Selector,
    _pipe@1 = gleam@erlang@process:selecting_record3(
        _pipe,
        Tcp,
        unsafe_coerce_packet(Mapper)
    ),
    _pipe@2 = gleam@erlang@process:selecting_record2(
        _pipe@1,
        Closed,
        unsafe_coerce_closed(Mapper)
    ),
    gleam@erlang@process:selecting_record3(
        _pipe@2,
        Error,
        unsafe_coerce_to_tcp_error(Mapper)
    ).
