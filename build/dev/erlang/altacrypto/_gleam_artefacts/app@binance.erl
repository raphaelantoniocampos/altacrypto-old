-module(app@binance).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([query_binance_status/0]).

-spec fetch_usdt_pairs() -> any().
fetch_usdt_pairs() ->
    erlang:error(#{gleam_error => todo,
            message => <<"This has not yet been implemented"/utf8>>,
            module => <<"app/binance"/utf8>>,
            function => <<"fetch_usdt_pairs"/utf8>>,
            line => 14}).

-spec make_request(binary()) -> binary().
make_request(Endpoint) ->
    case gleam@uri:parse(<<"https://api.binance.us"/utf8, Endpoint/binary>>) of
        {ok, Uri} ->
            case gleam@http@request:from_uri(Uri) of
                {ok, Req} ->
                    gleam@io:debug(erlang:element(4, Req));

                {error, _} ->
                    make_request(Endpoint)
            end;

        {error, _} ->
            make_request(Endpoint)
    end.

-spec query_binance_status() -> binary().
query_binance_status() ->
    Endpoint = <<"/sapi/v1/system/status"/utf8>>,
    Response = make_request(Endpoint).
