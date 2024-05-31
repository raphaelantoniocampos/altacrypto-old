-module(app@pages).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([home/0]).

-spec home() -> lustre@internals@vdom:element(any()).
home() ->
    app@pages@home:root().
