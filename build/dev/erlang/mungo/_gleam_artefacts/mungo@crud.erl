-module(mungo@crud).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([count_all/2, count/3, insert_many/3, insert_one/3, find_many/4, find_one/4, find_by_id/3, find_all/3, update_one/5, update_many/5, delete_one/3, delete_many/3]).
-export_type([find_option/0, update_option/0, insert_result/0, update_result/0]).

-type find_option() :: {skip, integer()} |
    {limit, integer()} |
    {batch_size, integer()} |
    {sort, list({binary(), bison@bson:value()})} |
    {projection, list({binary(), bison@bson:value()})}.

-type update_option() :: upsert |
    {array_filters, list(list({binary(), bison@bson:value()}))}.

-type insert_result() :: {insert_result, integer(), list(bison@bson:value())}.

-type update_result() :: {update_result,
        integer(),
        integer(),
        list(bison@bson:value())}.

-spec count_all(mungo@client:collection(), integer()) -> {ok, integer()} |
    {error, mungo@error:error()}.
count_all(Collection, Timeout) ->
    Cmd = [{<<"count"/utf8>>, {string, erlang:element(2, Collection)}}],
    _pipe = gleam@erlang@process:try_call(
        erlang:element(3, Collection),
        fun(_capture) -> {command, Cmd, _capture} end,
        Timeout
    ),
    _pipe@1 = gleam@result:replace_error(_pipe, actor_error),
    _pipe@2 = gleam@result:flatten(_pipe@1),
    _pipe@3 = gleam@result:map(
        _pipe@2,
        fun(Reply) -> case gleam@dict:get(Reply, <<"n"/utf8>>) of
                {ok, {int32, N}} ->
                    {ok, N};

                _ ->
                    {error, structure_error}
            end end
    ),
    gleam@result:flatten(_pipe@3).

-spec count(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
count(Collection, Filter, Timeout) ->
    Cmd = [{<<"count"/utf8>>, {string, erlang:element(2, Collection)}},
        {<<"query"/utf8>>, {document, maps:from_list(Filter)}}],
    _pipe = gleam@erlang@process:try_call(
        erlang:element(3, Collection),
        fun(_capture) -> {command, Cmd, _capture} end,
        Timeout
    ),
    _pipe@1 = gleam@result:replace_error(_pipe, actor_error),
    _pipe@2 = gleam@result:flatten(_pipe@1),
    _pipe@3 = gleam@result:map(
        _pipe@2,
        fun(Reply) -> case gleam@dict:get(Reply, <<"n"/utf8>>) of
                {ok, {int32, N}} ->
                    {ok, N};

                _ ->
                    {error, structure_error}
            end end
    ),
    gleam@result:flatten(_pipe@3).

-spec insert_many(
    mungo@client:collection(),
    list(list({binary(), bison@bson:value()})),
    integer()
) -> {ok, insert_result()} | {error, mungo@error:error()}.
insert_many(Collection, Docs, Timeout) ->
    Docs@1 = gleam@list:map(
        Docs,
        fun(Fields) ->
            _pipe = case gleam@list:find(
                Fields,
                fun(Kv) -> gleam@pair:first(Kv) =:= <<"_id"/utf8>> end
            ) of
                {ok, _} ->
                    Fields;

                {error, nil} ->
                    Id = bison@object_id:new(),
                    Fields@1 = gleam@list:prepend(
                        Fields,
                        {<<"_id"/utf8>>, {object_id, Id}}
                    ),
                    Fields@1
            end,
            _pipe@1 = maps:from_list(_pipe),
            {document, _pipe@1}
        end
    ),
    Inserted_ids = gleam@list:map(Docs@1, fun(D) -> case D of
                {document, Fields@2} ->
                    case gleam@dict:get(Fields@2, <<"_id"/utf8>>) of
                        {ok, Id@1} ->
                            Id@1;

                        _ ->
                            {string, <<""/utf8>>}
                    end;

                _ ->
                    {string, <<""/utf8>>}
            end end),
    Cmd = [{<<"insert"/utf8>>, {string, erlang:element(2, Collection)}},
        {<<"documents"/utf8>>, {array, Docs@1}}],
    _pipe@2 = gleam@erlang@process:try_call(
        erlang:element(3, Collection),
        fun(_capture) -> {command, Cmd, _capture} end,
        Timeout
    ),
    _pipe@3 = gleam@result:replace_error(_pipe@2, actor_error),
    _pipe@4 = gleam@result:flatten(_pipe@3),
    _pipe@6 = gleam@result:map(
        _pipe@4,
        fun(Reply) ->
            case {gleam@dict:get(Reply, <<"n"/utf8>>),
                gleam@dict:get(Reply, <<"writeErrors"/utf8>>)} of
                {_, {ok, {array, Errors}}} ->
                    {error,
                        {write_errors,
                            begin
                                _pipe@5 = Errors,
                                gleam@list:map(
                                    _pipe@5,
                                    fun(Error) ->
                                        {document, Fields@3} = case Error of
                                            {document, _} -> Error;
                                            _assert_fail ->
                                                erlang:error(
                                                        #{gleam_error => let_assert,
                                                            message => <<"Assertion pattern match failed"/utf8>>,
                                                            value => _assert_fail,
                                                            module => <<"mungo/crud"/utf8>>,
                                                            function => <<"insert_many"/utf8>>,
                                                            line => 216}
                                                    )
                                        end,
                                        _assert_subject = {gleam@dict:get(
                                                Fields@3,
                                                <<"code"/utf8>>
                                            ),
                                            gleam@dict:get(
                                                Fields@3,
                                                <<"keyValue"/utf8>>
                                            ),
                                            gleam@dict:get(
                                                Fields@3,
                                                <<"errmsg"/utf8>>
                                            )},
                                        {{ok, {int32, Code}},
                                            {ok, Source},
                                            {ok, {string, Msg}}} = case _assert_subject of
                                            {{ok, {int32, _}},
                                                {ok, _},
                                                {ok, {string, _}}} -> _assert_subject;
                                            _assert_fail@1 ->
                                                erlang:error(
                                                        #{gleam_error => let_assert,
                                                            message => <<"Assertion pattern match failed"/utf8>>,
                                                            value => _assert_fail@1,
                                                            module => <<"mungo/crud"/utf8>>,
                                                            function => <<"insert_many"/utf8>>,
                                                            line => 217}
                                                    )
                                        end,
                                        {write_error, Code, Msg, Source}
                                    end
                                )
                            end}};

                {{ok, {int32, N}}, _} ->
                    {ok, {insert_result, N, Inserted_ids}};

                {_, _} ->
                    {error, structure_error}
            end
        end
    ),
    gleam@result:flatten(_pipe@6).

-spec insert_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, bison@bson:value()} | {error, mungo@error:error()}.
insert_one(Collection, Doc, Timeout) ->
    case begin
        _pipe = Collection,
        insert_many(_pipe, [Doc], Timeout)
    end of
        {ok, {insert_result, _, [Id]}} ->
            {ok, Id};

        {error, Error} ->
            {error, Error};

        _ ->
            {error, structure_error}
    end.

-spec find(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list(find_option()),
    integer()
) -> {ok, mungo@cursor:cursor()} | {error, mungo@error:error()}.
find(Collection, Filter, Options, Timeout) ->
    Body = gleam@list:fold(
        Options,
        [{<<"find"/utf8>>, {string, erlang:element(2, Collection)}},
            {<<"filter"/utf8>>, {document, maps:from_list(Filter)}}],
        fun(Acc, Opt) -> case Opt of
                {sort, Sort} ->
                    gleam@list:key_set(
                        Acc,
                        <<"sort"/utf8>>,
                        {document, maps:from_list(Sort)}
                    );

                {projection, Projection} ->
                    gleam@list:key_set(
                        Acc,
                        <<"projection"/utf8>>,
                        {document, maps:from_list(Projection)}
                    );

                {skip, Skip} ->
                    gleam@list:key_set(Acc, <<"skip"/utf8>>, {int32, Skip});

                {limit, Limit} ->
                    gleam@list:key_set(Acc, <<"limit"/utf8>>, {int32, Limit});

                {batch_size, Size} ->
                    gleam@list:key_set(Acc, <<"batchSize"/utf8>>, {int32, Size})
            end end
    ),
    _pipe = gleam@erlang@process:try_call(
        erlang:element(3, Collection),
        fun(_capture) -> {command, Body, _capture} end,
        Timeout
    ),
    _pipe@1 = gleam@result:replace_error(_pipe, actor_error),
    _pipe@2 = gleam@result:flatten(_pipe@1),
    _pipe@4 = gleam@result:map(
        _pipe@2,
        fun(Reply) -> case gleam@dict:get(Reply, <<"cursor"/utf8>>) of
                {ok, {document, Cursor}} ->
                    case {gleam@dict:get(Cursor, <<"id"/utf8>>),
                        gleam@dict:get(Cursor, <<"firstBatch"/utf8>>)} of
                        {{ok, {int64, Id}}, {ok, {array, Batch}}} ->
                            _pipe@3 = mungo@cursor:new(Collection, Id, Batch),
                            {ok, _pipe@3};

                        {_, _} ->
                            {error, structure_error}
                    end;

                _ ->
                    {error, structure_error}
            end end
    ),
    gleam@result:flatten(_pipe@4).

-spec find_many(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list(find_option()),
    integer()
) -> {ok, mungo@cursor:cursor()} | {error, mungo@error:error()}.
find_many(Collection, Filter, Options, Timeout) ->
    _pipe = Collection,
    find(_pipe, Filter, Options, Timeout).

-spec find_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, gleam@option:option(bison@bson:value())} |
    {error, mungo@error:error()}.
find_one(Collection, Filter, Projection, Timeout) ->
    _pipe = Collection,
    _pipe@1 = find_many(
        _pipe,
        Filter,
        [{limit, 1}, {projection, Projection}],
        Timeout
    ),
    _pipe@5 = gleam@result:map(
        _pipe@1,
        fun(Cursor) -> case mungo@cursor:next(Cursor, Timeout) of
                {{some, Doc}, _} ->
                    _pipe@2 = Doc,
                    _pipe@3 = {some, _pipe@2},
                    {ok, _pipe@3};

                {none, _} ->
                    _pipe@4 = none,
                    {ok, _pipe@4}
            end end
    ),
    gleam@result:flatten(_pipe@5).

-spec find_by_id(mungo@client:collection(), binary(), integer()) -> {ok,
        gleam@option:option(bison@bson:value())} |
    {error, mungo@error:error()}.
find_by_id(Collection, Id, Timeout) ->
    _pipe = bison@object_id:from_string(Id),
    _pipe@2 = gleam@result:map(_pipe, fun(Id@1) -> _pipe@1 = Collection,
            find_one(
                _pipe@1,
                [{<<"_id"/utf8>>, {object_id, Id@1}}],
                [],
                Timeout
            ) end),
    _pipe@3 = gleam@result:replace_error(_pipe@2, structure_error),
    gleam@result:flatten(_pipe@3).

-spec find_all(mungo@client:collection(), list(find_option()), integer()) -> {ok,
        mungo@cursor:cursor()} |
    {error, mungo@error:error()}.
find_all(Collection, Options, Timeout) ->
    _pipe = Collection,
    find(_pipe, [], Options, Timeout).

-spec update(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    list(update_option()),
    boolean(),
    integer()
) -> {ok, update_result()} | {error, mungo@error:error()}.
update(Collection, Filter, Change, Options, Multi, Timeout) ->
    Update = begin
        _pipe@2 = gleam@list:fold(
            Options,
            [{<<"q"/utf8>>, {document, maps:from_list(Filter)}},
                {<<"u"/utf8>>, {document, maps:from_list(Change)}},
                {<<"multi"/utf8>>, {boolean, Multi}}],
            fun(Acc, Opt) -> case Opt of
                    upsert ->
                        gleam@list:key_set(
                            Acc,
                            <<"upsert"/utf8>>,
                            {boolean, true}
                        );

                    {array_filters, Filters} ->
                        gleam@list:key_set(
                            Acc,
                            <<"arrayFilters"/utf8>>,
                            {array,
                                begin
                                    _pipe = Filters,
                                    _pipe@1 = gleam@list:map(
                                        _pipe,
                                        fun maps:from_list/1
                                    ),
                                    gleam@list:map(
                                        _pipe@1,
                                        fun(Field@0) -> {document, Field@0} end
                                    )
                                end}
                        )
                end end
        ),
        _pipe@3 = maps:from_list(_pipe@2),
        {document, _pipe@3}
    end,
    Cmd = [{<<"update"/utf8>>, {string, erlang:element(2, Collection)}},
        {<<"updates"/utf8>>, {array, [Update]}}],
    _pipe@4 = gleam@erlang@process:try_call(
        erlang:element(3, Collection),
        fun(_capture) -> {command, Cmd, _capture} end,
        Timeout
    ),
    _pipe@5 = gleam@result:replace_error(_pipe@4, actor_error),
    _pipe@6 = gleam@result:flatten(_pipe@5),
    _pipe@8 = gleam@result:map(
        _pipe@6,
        fun(Reply) ->
            case {gleam@dict:get(Reply, <<"n"/utf8>>),
                gleam@dict:get(Reply, <<"nModified"/utf8>>),
                gleam@dict:get(Reply, <<"upserted"/utf8>>),
                gleam@dict:get(Reply, <<"writeErrors"/utf8>>)} of
                {{ok, {int32, N}},
                    {ok, {int32, Modified}},
                    {ok, {array, Upserted}},
                    _} ->
                    {ok, {update_result, N, Modified, Upserted}};

                {{ok, {int32, N@1}}, {ok, {int32, Modified@1}}, _, _} ->
                    {ok, {update_result, N@1, Modified@1, []}};

                {_, _, _, {ok, {array, Errors}}} ->
                    {error,
                        {write_errors,
                            begin
                                _pipe@7 = Errors,
                                gleam@list:map(
                                    _pipe@7,
                                    fun(Error) ->
                                        {document, Fields} = case Error of
                                            {document, _} -> Error;
                                            _assert_fail ->
                                                erlang:error(
                                                        #{gleam_error => let_assert,
                                                            message => <<"Assertion pattern match failed"/utf8>>,
                                                            value => _assert_fail,
                                                            module => <<"mungo/crud"/utf8>>,
                                                            function => <<"update"/utf8>>,
                                                            line => 340}
                                                    )
                                        end,
                                        _assert_subject = {gleam@dict:get(
                                                Fields,
                                                <<"code"/utf8>>
                                            ),
                                            gleam@dict:get(
                                                Fields,
                                                <<"keyValue"/utf8>>
                                            ),
                                            gleam@dict:get(
                                                Fields,
                                                <<"errmsg"/utf8>>
                                            )},
                                        {{ok, {int32, Code}},
                                            {ok, Source},
                                            {ok, {string, Msg}}} = case _assert_subject of
                                            {{ok, {int32, _}},
                                                {ok, _},
                                                {ok, {string, _}}} -> _assert_subject;
                                            _assert_fail@1 ->
                                                erlang:error(
                                                        #{gleam_error => let_assert,
                                                            message => <<"Assertion pattern match failed"/utf8>>,
                                                            value => _assert_fail@1,
                                                            module => <<"mungo/crud"/utf8>>,
                                                            function => <<"update"/utf8>>,
                                                            line => 341}
                                                    )
                                        end,
                                        {write_error, Code, Msg, Source}
                                    end
                                )
                            end}};

                {_, _, _, _} ->
                    {error, structure_error}
            end
        end
    ),
    gleam@result:flatten(_pipe@8).

-spec update_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    list(update_option()),
    integer()
) -> {ok, update_result()} | {error, mungo@error:error()}.
update_one(Collection, Filter, Change, Options, Timeout) ->
    _pipe = Collection,
    update(_pipe, Filter, Change, Options, false, Timeout).

-spec update_many(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    list(update_option()),
    integer()
) -> {ok, update_result()} | {error, mungo@error:error()}.
update_many(Collection, Filter, Change, Options, Timeout) ->
    _pipe = Collection,
    update(_pipe, Filter, Change, Options, true, Timeout).

-spec delete(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    boolean(),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
delete(Collection, Filter, Multi, Timeout) ->
    Cmd = [{<<"delete"/utf8>>, {string, erlang:element(2, Collection)}},
        {<<"deletes"/utf8>>,
            {array,
                [{document,
                        maps:from_list(
                            [{<<"q"/utf8>>, {document, maps:from_list(Filter)}},
                                {<<"limit"/utf8>>, {int32, case Multi of
                                            true ->
                                                0;

                                            false ->
                                                1
                                        end}}]
                        )}]}}],
    _pipe = gleam@erlang@process:try_call(
        erlang:element(3, Collection),
        fun(_capture) -> {command, Cmd, _capture} end,
        Timeout
    ),
    _pipe@1 = gleam@result:replace_error(_pipe, actor_error),
    _pipe@2 = gleam@result:flatten(_pipe@1),
    _pipe@4 = gleam@result:map(
        _pipe@2,
        fun(Reply) ->
            case {gleam@dict:get(Reply, <<"n"/utf8>>),
                gleam@dict:get(Reply, <<"writeErrors"/utf8>>)} of
                {{ok, {int32, N}}, _} ->
                    {ok, N};

                {_, {ok, {array, Errors}}} ->
                    {error,
                        {write_errors,
                            begin
                                _pipe@3 = Errors,
                                gleam@list:map(
                                    _pipe@3,
                                    fun(Error) ->
                                        {document, Fields} = case Error of
                                            {document, _} -> Error;
                                            _assert_fail ->
                                                erlang:error(
                                                        #{gleam_error => let_assert,
                                                            message => <<"Assertion pattern match failed"/utf8>>,
                                                            value => _assert_fail,
                                                            module => <<"mungo/crud"/utf8>>,
                                                            function => <<"delete"/utf8>>,
                                                            line => 393}
                                                    )
                                        end,
                                        _assert_subject = {gleam@dict:get(
                                                Fields,
                                                <<"code"/utf8>>
                                            ),
                                            gleam@dict:get(
                                                Fields,
                                                <<"keyValue"/utf8>>
                                            ),
                                            gleam@dict:get(
                                                Fields,
                                                <<"errmsg"/utf8>>
                                            )},
                                        {{ok, {int32, Code}},
                                            {ok, Source},
                                            {ok, {string, Msg}}} = case _assert_subject of
                                            {{ok, {int32, _}},
                                                {ok, _},
                                                {ok, {string, _}}} -> _assert_subject;
                                            _assert_fail@1 ->
                                                erlang:error(
                                                        #{gleam_error => let_assert,
                                                            message => <<"Assertion pattern match failed"/utf8>>,
                                                            value => _assert_fail@1,
                                                            module => <<"mungo/crud"/utf8>>,
                                                            function => <<"delete"/utf8>>,
                                                            line => 394}
                                                    )
                                        end,
                                        {write_error, Code, Msg, Source}
                                    end
                                )
                            end}};

                {_, _} ->
                    {error, structure_error}
            end
        end
    ),
    gleam@result:flatten(_pipe@4).

-spec delete_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
delete_one(Collection, Filter, Timeout) ->
    _pipe = Collection,
    delete(_pipe, Filter, false, Timeout).

-spec delete_many(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
delete_many(Collection, Filter, Timeout) ->
    _pipe = Collection,
    delete(_pipe, Filter, true, Timeout).
