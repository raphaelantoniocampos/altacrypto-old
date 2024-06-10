-module(app@db).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_collection/1, insert_crypto_snapshot/0]).

-spec get_connection_string() -> binary().
get_connection_string() ->
    dot_env:load(),
    _assert_subject = dot_env_ffi:get_env(<<"MONGO_USER"/utf8>>),
    {ok, User} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"app/db"/utf8>>,
                        function => <<"get_connection_string"/utf8>>,
                        line => 10})
    end,
    _assert_subject@1 = dot_env_ffi:get_env(<<"MONGO_PASSWORD"/utf8>>),
    {ok, Pass} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"app/db"/utf8>>,
                        function => <<"get_connection_string"/utf8>>,
                        line => 11})
    end,
    _assert_subject@2 = dot_env_ffi:get_env(<<"MONGO_DB"/utf8>>),
    {ok, Db} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"app/db"/utf8>>,
                        function => <<"get_connection_string"/utf8>>,
                        line => 12})
    end,
    _ = <<<<<<<<<<<<"mongodb://"/utf8, User/binary>>/binary, ":"/utf8>>/binary,
                    Pass/binary>>/binary,
                "@cluster0.wovexfa.mongodb.net:27017/"/utf8>>/binary,
            Db/binary>>/binary,
        "?authSource=admin"/utf8>>,
    _ = <<"mongodb+srv://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"/utf8>>,
    _ = <<"mongodb://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net:27017/altadata?authSource=admin&retryWrites=true&w=majority&appName=Cluster0"/utf8>>,
    New_string = <<"mongodb://127.0.0.1:27017/altadata?authSource=admin&directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+2.2.6"/utf8>>,
    New_string.

-spec get_collection(binary()) -> {ok, mungo@client:collection()} |
    {error, gleam@otp@actor:start_error()}.
get_collection(Name) ->
    Connection_string = get_connection_string(),
    case mungo:start(Connection_string, 512) of
        {ok, Client} ->
            Col = begin
                _pipe = Client,
                mungo:collection(_pipe, Name)
            end,
            {ok, Col};

        {error, E} ->
            {error, E}
    end.

-spec insert_crypto_snapshot() -> any().
insert_crypto_snapshot() ->
    erlang:error(#{gleam_error => todo,
            message => <<"This has not yet been implemented"/utf8>>,
            module => <<"app/db"/utf8>>,
            function => <<"insert_crypto_snapshot"/utf8>>,
            line => 50}).
