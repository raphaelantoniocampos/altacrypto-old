-module(app@bot).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/0]).

-spec start() -> nil.
start() ->
    gleam@io:println(<<"Bot started"/utf8>>),
    Time = birl:now(),
    gleam@io:debug(Time),
    gleam@io:println(<<"Bot running"/utf8>>).
