-module(mungo@aggregation).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([aggregate/3, append_stage/2, match/2, lookup/5, pipelined_lookup/5, unwind/3, unwind_with_index/4, project/2, add_fields/2, sort/2, group/2, skip/2, limit/2, to_cursor/1]).
-export_type([pipeline/0, aggregate_option/0]).

-opaque pipeline() :: {pipeline,
        mungo@client:collection(),
        list(aggregate_option()),
        integer(),
        gleam@queue:queue(list({binary(), bison@bson:value()}))}.

-type aggregate_option() :: {batch_size, integer()} |
    {'let', list({binary(), bison@bson:value()})}.

-spec aggregate(mungo@client:collection(), list(aggregate_option()), integer()) -> pipeline().
aggregate(Collection, Options, Timeout) ->
    {pipeline, Collection, Options, Timeout, gleam@queue:new()}.

-spec append_stage(pipeline(), {binary(), bison@bson:value()}) -> pipeline().
append_stage(Pipeline, Stage) ->
    {pipeline,
        erlang:element(2, Pipeline),
        erlang:element(3, Pipeline),
        erlang:element(4, Pipeline),
        begin
            _pipe = erlang:element(5, Pipeline),
            gleam@queue:push_back(_pipe, [Stage])
        end}.

-spec match(pipeline(), list({binary(), bison@bson:value()})) -> pipeline().
match(Pipeline, Doc) ->
    append_stage(Pipeline, {<<"$match"/utf8>>, {document, maps:from_list(Doc)}}).

-spec lookup(pipeline(), binary(), binary(), binary(), binary()) -> pipeline().
lookup(Pipeline, From, Local_field, Foreign_field, Alias) ->
    append_stage(
        Pipeline,
        {<<"$lookup"/utf8>>,
            {document,
                maps:from_list(
                    [{<<"from"/utf8>>, {string, From}},
                        {<<"localField"/utf8>>, {string, Local_field}},
                        {<<"foreignField"/utf8>>, {string, Foreign_field}},
                        {<<"as"/utf8>>, {string, Alias}}]
                )}}
    ).

-spec pipelined_lookup(
    pipeline(),
    binary(),
    list({binary(), bison@bson:value()}),
    list(list({binary(), bison@bson:value()})),
    binary()
) -> pipeline().
pipelined_lookup(Pipeline, From, Definitions, Lookup_pipeline, Alias) ->
    append_stage(
        Pipeline,
        {<<"$lookup"/utf8>>,
            {document,
                maps:from_list(
                    [{<<"from"/utf8>>, {string, From}},
                        {<<"let"/utf8>>,
                            {document, maps:from_list(Definitions)}},
                        {<<"pipeline"/utf8>>,
                            {array,
                                begin
                                    _pipe = Lookup_pipeline,
                                    _pipe@1 = gleam@list:map(
                                        _pipe,
                                        fun maps:from_list/1
                                    ),
                                    gleam@list:map(
                                        _pipe@1,
                                        fun(Field@0) -> {document, Field@0} end
                                    )
                                end}},
                        {<<"as"/utf8>>, {string, Alias}}]
                )}}
    ).

-spec unwind(pipeline(), binary(), boolean()) -> pipeline().
unwind(Pipeline, Path, Preserve_null_and_empty_arrays) ->
    append_stage(
        Pipeline,
        {<<"$unwind"/utf8>>,
            {document,
                maps:from_list(
                    [{<<"path"/utf8>>, {string, Path}},
                        {<<"preserveNullAndEmptyArrays"/utf8>>,
                            {boolean, Preserve_null_and_empty_arrays}}]
                )}}
    ).

-spec unwind_with_index(pipeline(), binary(), binary(), boolean()) -> pipeline().
unwind_with_index(Pipeline, Path, Index_field, Preserve_null_and_empty_arrays) ->
    append_stage(
        Pipeline,
        {<<"$unwind"/utf8>>,
            {document,
                maps:from_list(
                    [{<<"path"/utf8>>, {string, Path}},
                        {<<"includeArrayIndex"/utf8>>, {string, Index_field}},
                        {<<"preserveNullAndEmptyArrays"/utf8>>,
                            {boolean, Preserve_null_and_empty_arrays}}]
                )}}
    ).

-spec project(pipeline(), list({binary(), bison@bson:value()})) -> pipeline().
project(Pipeline, Doc) ->
    append_stage(
        Pipeline,
        {<<"$project"/utf8>>, {document, maps:from_list(Doc)}}
    ).

-spec add_fields(pipeline(), list({binary(), bison@bson:value()})) -> pipeline().
add_fields(Pipeline, Doc) ->
    append_stage(
        Pipeline,
        {<<"$addFields"/utf8>>, {document, maps:from_list(Doc)}}
    ).

-spec sort(pipeline(), list({binary(), bison@bson:value()})) -> pipeline().
sort(Pipeline, Doc) ->
    append_stage(Pipeline, {<<"$sort"/utf8>>, {document, maps:from_list(Doc)}}).

-spec group(pipeline(), list({binary(), bison@bson:value()})) -> pipeline().
group(Pipeline, Doc) ->
    append_stage(Pipeline, {<<"$group"/utf8>>, {document, maps:from_list(Doc)}}).

-spec skip(pipeline(), integer()) -> pipeline().
skip(Pipeline, Count) ->
    append_stage(Pipeline, {<<"$skip"/utf8>>, {int32, Count}}).

-spec limit(pipeline(), integer()) -> pipeline().
limit(Pipeline, Count) ->
    append_stage(Pipeline, {<<"$limit"/utf8>>, {int32, Count}}).

-spec to_cursor(pipeline()) -> {ok, mungo@cursor:cursor()} |
    {error, mungo@error:error()}.
to_cursor(Pipeline) ->
    Body = gleam@list:fold(
        erlang:element(3, Pipeline),
        [{<<"aggregate"/utf8>>,
                {string, erlang:element(2, erlang:element(2, Pipeline))}},
            {<<"pipeline"/utf8>>,
                begin
                    _pipe = erlang:element(5, Pipeline),
                    _pipe@1 = gleam@queue:to_list(_pipe),
                    _pipe@2 = gleam@list:map(
                        _pipe@1,
                        fun(Stage) -> maps:from_list(Stage) end
                    ),
                    _pipe@3 = gleam@list:map(
                        _pipe@2,
                        fun(Stage@1) -> {document, Stage@1} end
                    ),
                    {array, _pipe@3}
                end},
            {<<"cursor"/utf8>>, {document, gleam@dict:new()}}],
        fun(Acc, Opt) -> case Opt of
                {batch_size, Size} ->
                    gleam@list:key_set(
                        Acc,
                        <<"cursor"/utf8>>,
                        {document,
                            maps:from_list(
                                [{<<"batchSize"/utf8>>, {int32, Size}}]
                            )}
                    );

                {'let', Let_doc} ->
                    gleam@list:key_set(
                        Acc,
                        <<"let"/utf8>>,
                        {document, maps:from_list(Let_doc)}
                    )
            end end
    ),
    _pipe@4 = gleam@erlang@process:try_call(
        erlang:element(3, erlang:element(2, Pipeline)),
        fun(_capture) -> {command, Body, _capture} end,
        erlang:element(4, Pipeline)
    ),
    _pipe@5 = gleam@result:replace_error(_pipe@4, actor_error),
    _pipe@6 = gleam@result:flatten(_pipe@5),
    _pipe@8 = gleam@result:map(
        _pipe@6,
        fun(Reply) -> case gleam@dict:get(Reply, <<"cursor"/utf8>>) of
                {ok, {document, Cursor}} ->
                    case {gleam@dict:get(Cursor, <<"id"/utf8>>),
                        gleam@dict:get(Cursor, <<"firstBatch"/utf8>>)} of
                        {{ok, {int64, Id}}, {ok, {array, Batch}}} ->
                            _pipe@7 = mungo@cursor:new(
                                erlang:element(2, Pipeline),
                                Id,
                                Batch
                            ),
                            {ok, _pipe@7};

                        {_, _} ->
                            {error, structure_error}
                    end;

                _ ->
                    {error, structure_error}
            end end
    ),
    gleam@result:flatten(_pipe@8).
