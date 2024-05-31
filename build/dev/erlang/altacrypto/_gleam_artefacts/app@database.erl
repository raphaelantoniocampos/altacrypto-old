-module(app@database).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-spec main() -> mungo@client:collection().
main() ->
    _assert_subject = mungo:start(
        <<"mongodb://app-dev:passwd@localhost/app-db?authSource=admin"/utf8>>,
        512
    ),
    {ok, Client} = case _assert_subject of
        {ok, _} -> _assert_subject;
        _assert_fail ->
            erlang:error(#{gleam_error => let_assert,
                        message => <<"Assertion pattern match failed"/utf8>>,
                        value => _assert_fail,
                        module => <<"app/database"/utf8>>,
                        function => <<"main"/utf8>>,
                        line => 4})
    end,
    Users = begin
        _pipe = Client,
        mungo:collection(_pipe, <<"users"/utf8>>)
    end,
    Users.
