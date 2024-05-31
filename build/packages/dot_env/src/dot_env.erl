-module(dot_env).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([load_with_opts/1, load/0]).
-export_type([opts/0, dot_env/0]).

-type opts() :: {opts, binary(), boolean(), boolean(), boolean()} | default.

-opaque dot_env() :: {dot_env, binary(), boolean(), boolean(), boolean()}.

-spec handle_file_result({ok, binary()} | {error, binary()}, boolean()) -> {ok,
        binary()} |
    {error, binary()}.
handle_file_result(Res, Ignore_error) ->
    gleam@bool:guard(
        gleam@result:is_error(Res) andalso Ignore_error,
        {ok, <<""/utf8>>},
        fun() -> Res end
    ).

-spec set_env(dot_env(), {binary(), binary()}) -> nil.
set_env(Config, Pair) ->
    {Key, Value} = Pair,
    Key@1 = (gleam@bool:guard(
        not erlang:element(4, Config),
        Key,
        fun() -> gleam@string:uppercase(Key) end
    )),
    dot_env_ffi:set_env(Key@1, Value).

-spec recursively_set_environment_variables(
    dot_env(),
    list({binary(), binary()})
) -> nil.
recursively_set_environment_variables(Config, Kv_pairs) ->
    case Kv_pairs of
        [] ->
            nil;

        [Pair] ->
            set_env(Config, Pair);

        [Pair@1 | Rest] ->
            set_env(Config, Pair@1),
            recursively_set_environment_variables(Config, Rest)
    end.

-spec read_file(dot_env()) -> {ok, binary()} | {error, binary()}.
read_file(Dotenv) ->
    gleam@result:'try'(
        begin
            _pipe = simplifile:verify_is_file(erlang:element(2, Dotenv)),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<"Failed to access file, ensure the file exists and is a readable file"/utf8>>
                end
            )
        end,
        fun(Is_file) ->
            gleam@bool:guard(
                not Is_file,
                {error,
                    <<<<"Specified file at `"/utf8,
                            (erlang:element(2, Dotenv))/binary>>/binary,
                        "` does not exist"/utf8>>},
                fun() ->
                    gleam@result:'try'(
                        begin
                            _pipe@1 = simplifile:read(erlang:element(2, Dotenv)),
                            gleam@result:map_error(
                                _pipe@1,
                                fun(_) ->
                                    Msg = <<<<"Unable to read file at `"/utf8,
                                            (erlang:element(2, Dotenv))/binary>>/binary,
                                        "`, ensure the file exists and is readable"/utf8>>,
                                    Msg
                                end
                            )
                        end,
                        fun(Contents) -> {ok, Contents} end
                    )
                end
            )
        end
    ).

-spec load_and_return_error(dot_env()) -> {ok, nil} | {error, binary()}.
load_and_return_error(Dotenv) ->
    gleam@result:'try'(
        begin
            _pipe = read_file(Dotenv),
            handle_file_result(_pipe, erlang:element(5, Dotenv))
        end,
        fun(Content) ->
            gleam@result:'try'(
                dot_env@internal@parser:parse(Content),
                fun(Kv_pairs) ->
                    _pipe@1 = Dotenv,
                    recursively_set_environment_variables(_pipe@1, Kv_pairs),
                    {ok, nil}
                end
            )
        end
    ).

-spec load_with_opts(opts()) -> nil.
load_with_opts(Opts) ->
    Dotenv = case Opts of
        {opts, Path, Debug, Capitalize, Ignore_missing_file} ->
            {dot_env, Path, Debug, Capitalize, Ignore_missing_file};

        default ->
            {dot_env, <<".env"/utf8>>, true, true, true}
    end,
    State = begin
        _pipe = Dotenv,
        load_and_return_error(_pipe)
    end,
    case State of
        {ok, _} ->
            nil;

        {error, Msg} ->
            gleam@bool:guard(
                not erlang:element(3, Dotenv),
                nil,
                fun() -> gleam@io:println_error(Msg) end
            )
    end.

-spec load() -> nil.
load() ->
    load_with_opts(default).
