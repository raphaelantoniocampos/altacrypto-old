-module(app@bot).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/0]).

-spec start() -> birl:time().
start() ->
    gleam@io:println(<<"Bot started"/utf8>>),
    Time = birl:now().
