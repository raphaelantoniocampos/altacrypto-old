-module(bison@ejson@encoder).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([to_canonical/1]).

-spec bson_to_canonical(bison@bson:value()) -> gleam@json:json().
bson_to_canonical(Value) ->
    case Value of
        null ->
            gleam@json:null();

        {boolean, B} ->
            gleam@json:bool(B);

        {string, S} ->
            gleam@json:string(S);

        {document, Doc} ->
            document(Doc);

        min ->
            gleam@json:object([{<<"$minKey"/utf8>>, gleam@json:int(1)}]);

        max ->
            gleam@json:object([{<<"$maxKey"/utf8>>, gleam@json:int(1)}]);

        {js, Code} ->
            gleam@json:object([{<<"$code"/utf8>>, gleam@json:string(Code)}]);

        na_n ->
            gleam@json:object(
                [{<<"$numberDouble"/utf8>>, gleam@json:string(<<"NaN"/utf8>>)}]
            );

        {array, A} ->
            gleam@json:preprocessed_array(
                gleam@list:map(A, fun bson_to_canonical/1)
            );

        infinity ->
            gleam@json:object(
                [{<<"$numberDouble"/utf8>>,
                        gleam@json:string(<<"Infinity"/utf8>>)}]
            );

        negative_infinity ->
            gleam@json:object(
                [{<<"$numberDouble"/utf8>>,
                        gleam@json:string(<<"-Infinity"/utf8>>)}]
            );

        {int32, N} ->
            gleam@json:object(
                [{<<"$numberInt"/utf8>>,
                        begin
                            _pipe = N,
                            _pipe@1 = gleam@int:to_string(_pipe),
                            gleam@json:string(_pipe@1)
                        end}]
            );

        {int64, N@1} ->
            gleam@json:object(
                [{<<"$numberLong"/utf8>>,
                        begin
                            _pipe@2 = N@1,
                            _pipe@3 = gleam@int:to_string(_pipe@2),
                            gleam@json:string(_pipe@3)
                        end}]
            );

        {double, F} ->
            gleam@json:object(
                [{<<"$numberDouble"/utf8>>,
                        begin
                            _pipe@4 = F,
                            _pipe@5 = gleam@float:to_string(_pipe@4),
                            gleam@json:string(_pipe@5)
                        end}]
            );

        {date_time, Dt} ->
            {duration, Micro_t} = birl:difference(Dt, {time, 0, 0, none, none}),
            gleam@json:object(
                [{<<"$date"/utf8>>,
                        gleam@json:object(
                            [{<<"$numberLong"/utf8>>,
                                    begin
                                        _pipe@6 = (Micro_t div 1000),
                                        _pipe@7 = gleam@int:to_string(_pipe@6),
                                        gleam@json:string(_pipe@7)
                                    end}]
                        )}]
            );

        {timestamp, Stamp, Counter} ->
            gleam@json:object(
                [{<<"$timestamp"/utf8>>,
                        gleam@json:object(
                            [{<<"t"/utf8>>, gleam@json:int(Stamp)},
                                {<<"i"/utf8>>, gleam@json:int(Counter)}]
                        )}]
            );

        {regex, Pattern, Options} ->
            gleam@json:object(
                [{<<"$regularExpression"/utf8>>,
                        gleam@json:object(
                            [{<<"pattern"/utf8>>, gleam@json:string(Pattern)},
                                {<<"options"/utf8>>, gleam@json:string(Options)}]
                        )}]
            );

        {object_id, Id} ->
            gleam@json:object(
                [{<<"$oid"/utf8>>,
                        begin
                            _pipe@8 = Id,
                            _pipe@9 = bison@object_id:to_string(_pipe@8),
                            gleam@json:string(_pipe@9)
                        end}]
            );

        {binary, {md5, Md5}} ->
            gleam@json:object(
                [{<<"$binary"/utf8>>,
                        gleam@json:object(
                            [{<<"base64"/utf8>>,
                                    begin
                                        _pipe@10 = Md5,
                                        _pipe@11 = bison@md5:to_bit_array(
                                            _pipe@10
                                        ),
                                        _pipe@12 = gleam@bit_array:base64_encode(
                                            _pipe@11,
                                            true
                                        ),
                                        gleam@json:string(_pipe@12)
                                    end},
                                {<<"subType"/utf8>>,
                                    gleam@json:string(<<"05"/utf8>>)}]
                        )}]
            );

        {binary, {uuid, Uuid}} ->
            gleam@json:object(
                [{<<"$binary"/utf8>>,
                        gleam@json:object(
                            [{<<"base64"/utf8>>,
                                    begin
                                        _pipe@13 = Uuid,
                                        _pipe@14 = bison@uuid:to_bit_array(
                                            _pipe@13
                                        ),
                                        _pipe@15 = gleam@bit_array:base64_encode(
                                            _pipe@14,
                                            true
                                        ),
                                        gleam@json:string(_pipe@15)
                                    end},
                                {<<"subType"/utf8>>,
                                    gleam@json:string(<<"04"/utf8>>)}]
                        )}]
            );

        {binary, {generic, Generic}} ->
            gleam@json:object(
                [{<<"$binary"/utf8>>,
                        gleam@json:object(
                            [{<<"base64"/utf8>>,
                                    begin
                                        _pipe@16 = Generic,
                                        _pipe@17 = bison@generic:to_bit_array(
                                            _pipe@16
                                        ),
                                        _pipe@18 = gleam@bit_array:base64_encode(
                                            _pipe@17,
                                            true
                                        ),
                                        gleam@json:string(_pipe@18)
                                    end},
                                {<<"subType"/utf8>>,
                                    gleam@json:string(<<"00"/utf8>>)}]
                        )}]
            );

        {binary, {custom, Custom}} ->
            {Code@1, Value@1} = bison@custom:to_bit_array_with_code(Custom),
            _assert_subject = gleam@int:to_base_string(Code@1, 16),
            {ok, Code@2} = case _assert_subject of
                {ok, _} -> _assert_subject;
                _assert_fail ->
                    erlang:error(#{gleam_error => let_assert,
                                message => <<"Assertion pattern match failed"/utf8>>,
                                value => _assert_fail,
                                module => <<"bison/ejson/encoder"/utf8>>,
                                function => <<"bson_to_canonical"/utf8>>,
                                line => 176})
            end,
            gleam@json:object(
                [{<<"$binary"/utf8>>,
                        gleam@json:object(
                            [{<<"base64"/utf8>>,
                                    begin
                                        _pipe@19 = Value@1,
                                        _pipe@20 = gleam@bit_array:base64_encode(
                                            _pipe@19,
                                            true
                                        ),
                                        gleam@json:string(_pipe@20)
                                    end},
                                {<<"subType"/utf8>>, gleam@json:string(Code@2)}]
                        )}]
            )
    end.

-spec document(gleam@dict:dict(binary(), bison@bson:value())) -> gleam@json:json().
document(Doc) ->
    _pipe = Doc,
    _pipe@1 = maps:to_list(_pipe),
    _pipe@2 = gleam@list:map(
        _pipe@1,
        fun(Field) ->
            {erlang:element(1, Field),
                bson_to_canonical(erlang:element(2, Field))}
        end
    ),
    gleam@json:object(_pipe@2).

-spec to_canonical(gleam@dict:dict(binary(), bison@bson:value())) -> binary().
to_canonical(Doc) ->
    _pipe = Doc,
    _pipe@1 = document(_pipe),
    gleam@json:to_string(_pipe@1).
