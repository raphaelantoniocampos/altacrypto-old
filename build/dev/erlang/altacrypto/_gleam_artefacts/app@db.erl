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
                        line => 11})
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
                        line => 12})
    end,
    _ = <<"mongodb+srv://{mongo_user}:{
  mongo_password}@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"/utf8>>,
    <<<<<<<<"mongodb+srv://"/utf8, User/binary>>/binary, ":"/utf8>>/binary,
            Pass/binary>>/binary,
        "@cluster0.wovexfa.mongodb.net/?retryWrites=true&w=majority&appName=Cluster0"/utf8>>,
    <<"mongodb+srv://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net/"/utf8>>,
    <<"mongodb://admin:dinheiromtechobatmannmuie@cluster0.wovexfa.mongodb.net:27017/?directConnection=true"/utf8>>.

-spec get_collection(binary()) -> mungo@client:collection().
get_collection(Name) ->
    Connection_string = get_connection_string(),
    _assert_subject = mungo:start(Connection_string, 512),
    {ok, Client} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"app/db"/utf8>>,
                        function => <<"get_collection"/utf8>>,
                        line => 32})
    end,
    gleam@io:debug(Client),
    _pipe = Client,
    mungo:collection(_pipe, Name).
