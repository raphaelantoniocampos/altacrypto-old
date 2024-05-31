-module(app@router).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([handle_request/2]).

-spec handle_request(
    gleam@http@request:request(wisp:connection()),
    app@web:context()
) -> gleam@http@response:response(wisp:body()).
handle_request(Req, Ctx) ->
    app@web:middleware(
        Req,
        Ctx,
        fun(Req@1) -> case fun gleam@http@request:path_segments/1(Req@1) of
                [] ->
                    _pipe = [app@pages:home()],
                    _pipe@1 = app@pages@layout:layout(_pipe),
                    _pipe@2 = lustre@element:to_document_string_builder(_pipe@1),
                    wisp:html_response(_pipe@2, 200);

                [<<"internal-server-error"/utf8>>] ->
                    wisp:internal_server_error();

                [<<"unprocessable-entity"/utf8>>] ->
                    wisp:unprocessable_entity();

                [<<"method-not-allowed"/utf8>>] ->
                    wisp:method_not_allowed([]);

                [<<"entity-too-large"/utf8>>] ->
                    wisp:entity_too_large();

                [<<"bad-request"/utf8>>] ->
                    wisp:bad_request();

                _ ->
                    wisp:not_found()
            end end
    ).
