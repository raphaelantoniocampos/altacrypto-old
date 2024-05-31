-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([application/3, element/1, simple/3, component/4, start_actor/2, start_server_component/2, register/2, dispatch/1, shutdown/0, is_browser/0, start/3, is_registered/1]).
-export_type([app/3, client_spa/0, server_component/0, error/0]).

-opaque app(OLG, OLH, OLI) :: {app,
        fun((OLG) -> {OLH, lustre@effect:effect(OLI)}),
        fun((OLH, OLI) -> {OLH, lustre@effect:effect(OLI)}),
        fun((OLH) -> lustre@internals@vdom:element(OLI)),
        gleam@option:option(gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
                OLI} |
            {error, list(gleam@dynamic:decode_error())})))}.

-type client_spa() :: any().

-type server_component() :: any().

-type error() :: {actor_error, gleam@otp@actor:start_error()} |
    {bad_component_name, binary()} |
    {component_already_registered, binary()} |
    {element_not_found, binary()} |
    not_a_browser |
    not_erlang.

-spec application(
    fun((OMB) -> {OMC, lustre@effect:effect(OMD)}),
    fun((OMC, OMD) -> {OMC, lustre@effect:effect(OMD)}),
    fun((OMC) -> lustre@internals@vdom:element(OMD))
) -> app(OMB, OMC, OMD).
application(Init, Update, View) ->
    {app, Init, Update, View, none}.

-spec element(lustre@internals@vdom:element(OLP)) -> app(nil, nil, OLP).
element(Html) ->
    Init = fun(_) -> {nil, lustre@effect:none()} end,
    Update = fun(_, _) -> {nil, lustre@effect:none()} end,
    View = fun(_) -> Html end,
    application(Init, Update, View).

-spec simple(
    fun((OLU) -> OLV),
    fun((OLV, OLW) -> OLV),
    fun((OLV) -> lustre@internals@vdom:element(OLW))
) -> app(OLU, OLV, OLW).
simple(Init, Update, View) ->
    Init@1 = fun(Flags) -> {Init(Flags), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-spec component(
    fun((OMK) -> {OML, lustre@effect:effect(OMM)}),
    fun((OML, OMM) -> {OML, lustre@effect:effect(OMM)}),
    fun((OML) -> lustre@internals@vdom:element(OMM)),
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, OMM} |
        {error, list(gleam@dynamic:decode_error())}))
) -> app(OMK, OML, OMM).
component(Init, Update, View, On_attribute_change) ->
    {app, Init, Update, View, {some, On_attribute_change}}.

-spec do_start(app(ONG, any(), ONI), binary(), ONG) -> {ok,
        fun((lustre@internals@runtime:action(ONI, client_spa())) -> nil)} |
    {error, error()}.
do_start(_, _, _) ->
    {error, not_a_browser}.

-spec do_start_actor(app(OOL, any(), OON), OOL) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(OON, server_component()))} |
    {error, error()}.
do_start_actor(App, Flags) ->
    On_attribute_change = gleam@option:unwrap(
        erlang:element(5, App),
        gleam@dict:new()
    ),
    _pipe = (erlang:element(2, App))(Flags),
    _pipe@1 = lustre@internals@runtime:start(
        _pipe,
        erlang:element(3, App),
        erlang:element(4, App),
        On_attribute_change
    ),
    gleam@result:map_error(_pipe@1, fun(Field@0) -> {actor_error, Field@0} end).

-spec start_actor(app(OOA, any(), OOC), OOA) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(OOC, server_component()))} |
    {error, error()}.
start_actor(App, Flags) ->
    do_start_actor(App, Flags).

-spec start_server_component(app(ONQ, any(), ONS), ONQ) -> {ok,
        fun((lustre@internals@runtime:action(ONS, server_component())) -> nil)} |
    {error, error()}.
start_server_component(App, Flags) ->
    gleam@result:map(
        start_actor(App, Flags),
        fun(Runtime) ->
            fun(_capture) -> gleam@otp@actor:send(Runtime, _capture) end
        end
    ).

-spec register(app(nil, any(), any()), binary()) -> {ok, nil} | {error, error()}.
register(_, _) ->
    {error, not_a_browser}.

-spec dispatch(OPD) -> lustre@internals@runtime:action(OPD, any()).
dispatch(Msg) ->
    {dispatch, Msg}.

-spec shutdown() -> lustre@internals@runtime:action(any(), any()).
shutdown() ->
    shutdown.

-spec is_browser() -> boolean().
is_browser() ->
    false.

-spec start(app(OMW, any(), OMY), binary(), OMW) -> {ok,
        fun((lustre@internals@runtime:action(OMY, client_spa())) -> nil)} |
    {error, error()}.
start(App, Selector, Flags) ->
    gleam@bool:guard(
        not is_browser(),
        {error, not_a_browser},
        fun() -> do_start(App, Selector, Flags) end
    ).

-spec is_registered(binary()) -> boolean().
is_registered(_) ->
    false.
