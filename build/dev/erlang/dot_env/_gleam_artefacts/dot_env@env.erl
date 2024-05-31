-module(dot_env@env).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([set/2, get/1, get_or/2, get_int/1, get_bool/1]).

-spec set(binary(), binary()) -> nil.
set(Key, Value) ->
    dot_env_ffi:set_env(Key, Value).

-spec get(binary()) -> {ok, binary()} | {error, binary()}.
get(Key) ->
    dot_env_ffi:get_env(Key).

-spec get_or(binary(), binary()) -> binary().
get_or(Key, Default) ->
    case dot_env_ffi:get_env(Key) of
        {ok, Value} ->
            Value;

        {error, _} ->
            Default
    end.

-spec get_int(binary()) -> {ok, integer()} | {error, binary()}.
get_int(Key) ->
    case dot_env_ffi:get_env(Key) of
        {ok, Value} ->
            _pipe = gleam@int:parse(Value),
            gleam@result:map_error(
                _pipe,
                fun(_) ->
                    <<"Failed to parse string to int, confirm the value you are trying to retrieve is a valid integer"/utf8>>
                end
            );

        {error, E} ->
            {error, E}
    end.

-spec get_bool(binary()) -> {ok, boolean()} | {error, binary()}.
get_bool(Key) ->
    case dot_env_ffi:get_env(Key) of
        {ok, Value} ->
            case Value of
                <<"True"/utf8>> ->
                    {ok, true};

                <<"true"/utf8>> ->
                    {ok, true};

                <<"1"/utf8>> ->
                    {ok, true};

                _ ->
                    {ok, false}
            end;

        {error, E} ->
            {error, E}
    end.
