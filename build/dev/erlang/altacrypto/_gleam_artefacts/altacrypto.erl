-module(altacrypto).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec static_directory() -> binary().
static_directory() ->
    _assert_subject = fun gleam_erlang_ffi:priv_directory/1(
        <<"altacrypto"/utf8>>
    ),
    {ok, Priv_directory} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"altacrypto"/utf8>>,
                        function => <<"static_directory"/utf8>>,
                        line => 43})
    end,
    <<Priv_directory/binary, "/static"/utf8>>.

-spec main() -> nil.
main() ->
    wisp:configure_logger(),
    dot_env:load(),
    _assert_subject = dot_env_ffi:get_env(<<"SECRET_KEY_BASE"/utf8>>),
    {ok, Secret_key_base} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"altacrypto"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 18})
    end,
    Ctx = {context, static_directory()},
    Handler = fun(_capture) -> app@router:handle_request(_capture, Ctx) end,
    _assert_subject@1 = begin
        _pipe = wisp:mist_handler(Handler, Secret_key_base),
        _pipe@1 = mist:new(_pipe),
        _pipe@2 = mist:port(_pipe@1, 8000),
        mist:start_http(_pipe@2)
    end,
    {ok, _} = case _assert_subject@1 of
        {ok, _} -> _assert_subject@1;
        _assert_fail@1 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@1,
                        module => <<"altacrypto"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 24})
    end,
    Start_bot = fun app@bot:start/0,
    gleam@erlang@process:start(Start_bot, true),
    _assert_subject@2 = app@db:get_collection(<<"users"/utf8>>),
    {ok, Db} = case _assert_subject@2 of
        {ok, _} -> _assert_subject@2;
        _assert_fail@2 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@2,
                        module => <<"altacrypto"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 33})
    end,
    _assert_subject@3 = mungo:find_all(Db, [], 512),
    {ok, Cursor} = case _assert_subject@3 of
        {ok, _} -> _assert_subject@3;
        _assert_fail@3 ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail@3,
                        module => <<"altacrypto"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 34})
    end,
    New_list = mungo:to_list(Cursor, 128),
    Reso = gleam@list:pop(New_list, fun(_) -> true end),
    gleam@io:debug(Reso),
    gleam_erlang_ffi:sleep_forever().
