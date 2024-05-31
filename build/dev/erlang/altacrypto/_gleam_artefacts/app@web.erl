-module(app@web).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([default_responses/1, middleware/3]).
-export_type([context/0]).

-type context() :: {context, binary()}.

-spec default_responses(fun(() -> gleam@http@response:response(wisp:body()))) -> gleam@http@response:response(wisp:body()).
default_responses(Handle_request) ->
    Response = Handle_request(),
    gleam@bool:guard(
        erlang:element(4, Response) /= empty,
        Response,
        fun() -> case erlang:element(2, Response) of
                404 ->
                    _pipe = <<"<h1>Not Found</h1>"/utf8>>,
                    _pipe@1 = gleam@string_builder:from_string(_pipe),
                    wisp:html_body(Response, _pipe@1);

                405 ->
                    _pipe = <<"<h1>Not Found</h1>"/utf8>>,
                    _pipe@1 = gleam@string_builder:from_string(_pipe),
                    wisp:html_body(Response, _pipe@1);

                400 ->
                    _pipe@2 = <<"<h1>Bad Request</h1>"/utf8>>,
                    _pipe@3 = gleam@string_builder:from_string(_pipe@2),
                    wisp:html_body(Response, _pipe@3);

                422 ->
                    _pipe@2 = <<"<h1>Bad Request</h1>"/utf8>>,
                    _pipe@3 = gleam@string_builder:from_string(_pipe@2),
                    wisp:html_body(Response, _pipe@3);

                413 ->
                    _pipe@4 = <<"<h1>Request entity too large</h1>"/utf8>>,
                    _pipe@5 = gleam@string_builder:from_string(_pipe@4),
                    wisp:html_body(Response, _pipe@5);

                500 ->
                    _pipe@6 = <<"<h1>Internal server error</h1>"/utf8>>,
                    _pipe@7 = gleam@string_builder:from_string(_pipe@6),
                    wisp:html_body(Response, _pipe@7);

                _ ->
                    Response
            end end
    ).

-spec middleware(
    gleam@http@request:request(wisp:connection()),
    context(),
    fun((gleam@http@request:request(wisp:connection())) -> gleam@http@response:response(wisp:body()))
) -> gleam@http@response:response(wisp:body()).
middleware(Req, Ctx, Handle_request) ->
    Req@1 = wisp:method_override(Req),
    wisp:serve_static(
        Req@1,
        <<"/static"/utf8>>,
        erlang:element(2, Ctx),
        fun() ->
            wisp:log_request(
                Req@1,
                fun() ->
                    wisp:rescue_crashes(
                        fun() ->
                            wisp:handle_head(
                                Req@1,
                                fun(Req@2) ->
                                    default_responses(
                                        fun() -> Handle_request(Req@2) end
                                    )
                                end
                            )
                        end
                    )
                end
            )
        end
    ).
