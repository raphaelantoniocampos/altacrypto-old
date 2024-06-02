-module(lustre).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([application/3, element/1, simple/3, component/4, start_actor/2, start_server_component/2, register/2, dispatch/1, shutdown/0, is_browser/0, start/3, is_registered/1]).
-export_type([app/3, client_spa/0, server_component/0, error/0]).

-opaque app(PMS, PMT, PMU) :: {app,
        fun((PMS) -> {PMT, lustre@effect:effect(PMU)}),
        fun((PMT, PMU) -> {PMT, lustre@effect:effect(PMU)}),
        fun((PMT) -> lustre@internals@vdom:element(PMU)),
        gleam@option:option(gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok,
                PMU} |
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
    fun((PNN) -> {PNO, lustre@effect:effect(PNP)}),
    fun((PNO, PNP) -> {PNO, lustre@effect:effect(PNP)}),
    fun((PNO) -> lustre@internals@vdom:element(PNP))
) -> app(PNN, PNO, PNP).
application(Init, Update, View) ->
    {app, Init, Update, View, none}.

-spec element(lustre@internals@vdom:element(PNB)) -> app(nil, nil, PNB).
element(Html) ->
    Init = fun(_) -> {nil, lustre@effect:none()} end,
    Update = fun(_, _) -> {nil, lustre@effect:none()} end,
    View = fun(_) -> Html end,
    application(Init, Update, View).

-spec simple(
    fun((PNG) -> PNH),
    fun((PNH, PNI) -> PNH),
    fun((PNH) -> lustre@internals@vdom:element(PNI))
) -> app(PNG, PNH, PNI).
simple(Init, Update, View) ->
    Init@1 = fun(Flags) -> {Init(Flags), lustre@effect:none()} end,
    Update@1 = fun(Model, Msg) -> {Update(Model, Msg), lustre@effect:none()} end,
    application(Init@1, Update@1, View).

-spec component(
    fun((PNW) -> {PNX, lustre@effect:effect(PNY)}),
    fun((PNX, PNY) -> {PNX, lustre@effect:effect(PNY)}),
    fun((PNX) -> lustre@internals@vdom:element(PNY)),
    gleam@dict:dict(binary(), fun((gleam@dynamic:dynamic_()) -> {ok, PNY} |
        {error, list(gleam@dynamic:decode_error())}))
) -> app(PNW, PNX, PNY).
component(Init, Update, View, On_attribute_change) ->
    {app, Init, Update, View, {some, On_attribute_change}}.

-spec do_start(app(POS, any(), POU), binary(), POS) -> {ok,
        fun((lustre@internals@runtime:action(POU, client_spa())) -> nil)} |
    {error, error()}.
do_start(_, _, _) ->
    {error, not_a_browser}.

-spec do_start_actor(app(PPX, any(), PPZ), PPX) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(PPZ, server_component()))} |
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

-spec start_actor(app(PPM, any(), PPO), PPM) -> {ok,
        gleam@erlang@process:subject(lustre@internals@runtime:action(PPO, server_component()))} |
    {error, error()}.
start_actor(App, Flags) ->
    do_start_actor(App, Flags).

-spec start_server_component(app(PPC, any(), PPE), PPC) -> {ok,
        fun((lustre@internals@runtime:action(PPE, server_component())) -> nil)} |
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

-spec dispatch(PQP) -> lustre@internals@runtime:action(PQP, any()).
dispatch(Msg) ->
    {dispatch, Msg}.

-spec shutdown() -> lustre@internals@runtime:action(any(), any()).
shutdown() ->
    shutdown.

-spec is_browser() -> boolean().
is_browser() ->
    false.

-spec start(app(POI, any(), POK), binary(), POI) -> {ok,
        fun((lustre@internals@runtime:action(POK, client_spa())) -> nil)} |
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
