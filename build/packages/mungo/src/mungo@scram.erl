-module(mungo@scram).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([first_message/1, parse_first_reply/1, parse_second_reply/2, first_payload/1, hi/3, second_message/5]).

-spec first_message(binary()) -> list({binary(), bison@bson:value()}).
first_message(Payload) ->
    Payload@1 = begin
        _pipe = [<<"n,,"/utf8>>, Payload],
        _pipe@1 = gleam@string:concat(_pipe),
        bison@generic:from_string(_pipe@1)
    end,
    [{<<"saslStart"/utf8>>, {boolean, true}},
        {<<"mechanism"/utf8>>, {string, <<"SCRAM-SHA-256"/utf8>>}},
        {<<"payload"/utf8>>, {binary, {generic, Payload@1}}},
        {<<"autoAuthorize"/utf8>>, {boolean, true}},
        {<<"options"/utf8>>,
            {document,
                maps:from_list(
                    [{<<"skipEmptyExchange"/utf8>>, {boolean, true}}]
                )}}].

-spec parse_payload(binary()) -> {ok, list({binary(), binary()})} |
    {error, mungo@error:error()}.
parse_payload(Payload) ->
    _pipe = Payload,
    _pipe@1 = gleam@string:split(_pipe, <<","/utf8>>),
    _pipe@2 = gleam@list:try_map(
        _pipe@1,
        fun(Item) -> gleam@string:split_once(Item, <<"="/utf8>>) end
    ),
    gleam@result:replace_error(
        _pipe@2,
        {server_error, {authentication_failed, <<""/utf8>>}}
    ).

-spec parse_first_reply(gleam@dict:dict(binary(), bison@bson:value())) -> {ok,
        {{binary(), binary(), integer()}, binary(), integer()}} |
    {error, mungo@error:error()}.
parse_first_reply(Reply) ->
    case {gleam@dict:get(Reply, <<"ok"/utf8>>),
        gleam@dict:get(Reply, <<"done"/utf8>>),
        gleam@dict:get(Reply, <<"conversationId"/utf8>>),
        gleam@dict:get(Reply, <<"payload"/utf8>>),
        gleam@dict:get(Reply, <<"errmsg"/utf8>>)} of
        {{ok, {double, +0.0}}, _, _, _, {ok, {string, Msg}}} ->
            {error, {server_error, {authentication_failed, Msg}}};

        {{ok, {double, 1.0}},
            {ok, {boolean, false}},
            {ok, {int32, Cid}},
            {ok, {binary, {generic, Data}}},
            _} ->
            gleam@result:then(
                begin
                    _pipe = bison@generic:to_string(Data),
                    gleam@result:replace_error(
                        _pipe,
                        {server_error,
                            {authentication_failed,
                                <<"First payload is not a string"/utf8>>}}
                    )
                end,
                fun(Data@1) -> case parse_payload(Data@1) of
                        {ok,
                            [{<<"r"/utf8>>, Rnonce},
                                {<<"s"/utf8>>, Salt},
                                {<<"i"/utf8>>, I}]} ->
                            case gleam@int:parse(I) of
                                {ok, Iterations} ->
                                    case Iterations >= 4096 of
                                        true ->
                                            {ok,
                                                {{Rnonce, Salt, Iterations},
                                                    Data@1,
                                                    Cid}};

                                        false ->
                                            {error,
                                                {server_error,
                                                    {authentication_failed,
                                                        <<"Iterations should be an integer"/utf8>>}}}
                                    end;

                                {error, nil} ->
                                    {error,
                                        {server_error,
                                            {authentication_failed,
                                                <<"Iterations was not found"/utf8>>}}}
                            end;

                        _ ->
                            {error,
                                {server_error,
                                    {authentication_failed,
                                        <<"Invalid first payload"/utf8>>}}}
                    end end
            );

        {_, _, _, _, _} ->
            {error,
                {server_error,
                    {authentication_failed, <<"Invalid first reply"/utf8>>}}}
    end.

-spec parse_second_reply(
    gleam@dict:dict(binary(), bison@bson:value()),
    bitstring()
) -> {ok, nil} | {error, mungo@error:error()}.
parse_second_reply(Reply, Server_signature) ->
    case {gleam@dict:get(Reply, <<"ok"/utf8>>),
        gleam@dict:get(Reply, <<"done"/utf8>>),
        gleam@dict:get(Reply, <<"payload"/utf8>>)} of
        {{ok, {double, +0.0}}, _, _} ->
            {error, {server_error, {authentication_failed, <<""/utf8>>}}};

        {{ok, {double, 1.0}},
            {ok, {boolean, true}},
            {ok, {binary, {generic, Data}}}} ->
            gleam@result:then(
                begin
                    _pipe = bison@generic:to_string(Data),
                    gleam@result:replace_error(
                        _pipe,
                        {server_error, {authentication_failed, <<""/utf8>>}}
                    )
                end,
                fun(Data@1) -> case parse_payload(Data@1) of
                        {ok, [{<<"v"/utf8>>, Data@2}]} ->
                            gleam@result:then(
                                begin
                                    _pipe@1 = gleam@bit_array:base64_decode(
                                        Data@2
                                    ),
                                    gleam@result:replace_error(
                                        _pipe@1,
                                        {server_error,
                                            {authentication_failed, <<""/utf8>>}}
                                    )
                                end,
                                fun(Received_signature) ->
                                    case (erlang:byte_size(Server_signature) =:= erlang:byte_size(
                                        Received_signature
                                    ))
                                    andalso gleam@crypto:secure_compare(
                                        Server_signature,
                                        Received_signature
                                    ) of
                                        true ->
                                            {ok, nil};

                                        false ->
                                            {error,
                                                {server_error,
                                                    {authentication_failed,
                                                        <<""/utf8>>}}}
                                    end
                                end
                            );

                        _ ->
                            {error,
                                {server_error,
                                    {authentication_failed, <<""/utf8>>}}}
                    end end
            );

        {_, _, _} ->
            {error, {server_error, {authentication_failed, <<""/utf8>>}}}
    end.

-spec clean_username(binary()) -> binary().
clean_username(Username) ->
    _pipe = Username,
    _pipe@1 = gleam@string:replace(_pipe, <<"="/utf8>>, <<"=3D"/utf8>>),
    gleam@string:replace(_pipe@1, <<","/utf8>>, <<"=2C"/utf8>>).

-spec first_payload(binary()) -> binary().
first_payload(Username) ->
    Nonce = begin
        _pipe = crypto:strong_rand_bytes(24),
        gleam@bit_array:base64_encode(_pipe, true)
    end,
    _pipe@1 = [<<"n="/utf8>>, clean_username(Username), <<",r="/utf8>>, Nonce],
    gleam@string:concat(_pipe@1).

-spec hi(binary(), bitstring(), integer()) -> bitstring().
hi(Password, Salt, Iterations) ->
    crypto:pbkdf2_hmac(sha256, Password, Salt, Iterations, 32).

-spec 'xor'(bitstring(), bitstring(), bitstring()) -> bitstring().
'xor'(A, B, Storage) ->
    <<Fa, Ra/bitstring>> = case A of
        <<_, _/bitstring>> -> A;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"mungo/scram"/utf8>>,
                        function => <<"xor"/utf8>>,
                        line => 247})
    end,
    <<Fb, Rb/bitstring>> = case B of
        <<_, _/bitstring>> -> B;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"mungo/scram"/utf8>>,
                        function => <<"xor"/utf8>>,
                        line => 248})
    end,
    New_storage = begin
        _pipe = [Storage, <<(erlang:'bxor'(Fa, Fb))>>],
        gleam_stdlib:bit_array_concat(_pipe)
    end,
    case [Ra, Rb] of
        [<<>>, <<>>] ->
            New_storage;

        _ ->
            'xor'(Ra, Rb, New_storage)
    end.

-spec second_message(
    {binary(), binary(), integer()},
    binary(),
    binary(),
    integer(),
    binary()
) -> {ok, {list({binary(), bison@bson:value()}), bitstring()}} |
    {error, mungo@error:error()}.
second_message(Server_params, First_payload, Server_payload, Cid, Password) ->
    {Rnonce, Salt, Iterations} = Server_params,
    gleam@result:then(
        begin
            _pipe = gleam@bit_array:base64_decode(Salt),
            gleam@result:replace_error(
                _pipe,
                {server_error,
                    {authentication_failed,
                        <<"Salt is not base64 encoded string"/utf8>>}}
            )
        end,
        fun(Salt@1) ->
            Salted_password = hi(Password, Salt@1, Iterations),
            Client_key = gleam_crypto_ffi:hmac(
                gleam_stdlib:identity(<<"Client Key"/utf8>>),
                sha256,
                Salted_password
            ),
            Server_key = gleam_crypto_ffi:hmac(
                gleam_stdlib:identity(<<"Server Key"/utf8>>),
                sha256,
                Salted_password
            ),
            Stored_key = gleam_crypto_ffi:hash(sha256, Client_key),
            Auth_message = begin
                _pipe@1 = [First_payload,
                    <<","/utf8>>,
                    Server_payload,
                    <<",c=biws,r="/utf8>>,
                    Rnonce],
                _pipe@2 = gleam@string:concat(_pipe@1),
                bison@generic:from_string(_pipe@2)
            end,
            Client_signature = gleam_crypto_ffi:hmac(
                bison@generic:to_bit_array(Auth_message),
                sha256,
                Stored_key
            ),
            Second_payload = begin
                _pipe@4 = [<<"c=biws,r="/utf8>>,
                    Rnonce,
                    <<",p="/utf8>>,
                    begin
                        _pipe@3 = 'xor'(Client_key, Client_signature, <<>>),
                        gleam@bit_array:base64_encode(_pipe@3, true)
                    end],
                _pipe@5 = gleam@string:concat(_pipe@4),
                bison@generic:from_string(_pipe@5)
            end,
            Server_signature = gleam_crypto_ffi:hmac(
                bison@generic:to_bit_array(Auth_message),
                sha256,
                Server_key
            ),
            _pipe@6 = {[{<<"saslContinue"/utf8>>, {boolean, true}},
                    {<<"conversationId"/utf8>>, {int32, Cid}},
                    {<<"payload"/utf8>>, {binary, {generic, Second_payload}}}],
                Server_signature},
            {ok, _pipe@6}
        end
    ).
