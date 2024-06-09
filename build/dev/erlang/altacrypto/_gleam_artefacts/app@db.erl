-module(app@db).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([get_collection/1]).

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
                        line => 13})
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
                        line => 14})
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
                        line => 15})
    end,
    _ = <<<<<<<<<<<<"mongodb://"/utf8, User/binary>>/binary, ":"/utf8>>/binary,
                    Pass/binary>>/binary,
                "@cluster0.wovexfa.mongodb.net:27017/"/utf8>>/binary,
            Db/binary>>/binary,
        "?authSource=admin"/utf8>>,
    New_string = <<"mongodb://127.0.0.1:27017/altadata?authSource=admin&directConnection=true&serverSelectionTimeoutMS=2000&appName=mongosh+2.2.6"/utf8>>,
    New_string.

-spec get_collection(binary()) -> {ok, mungo@client:collection()} | {error, nil}.
get_collection(Name) ->
    Connection_string = get_connection_string(),
    Client = mungo:start(Connection_string, 512),
    case Client of
        {ok, Client@1} ->
            Col = begin
                _pipe = Client@1,
                mungo:collection(_pipe, Name)
            end,
            {ok, Col};

        {error, _} ->
            {error, nil}
    end.
