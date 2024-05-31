-module(mungo@cursor).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([new/3, next/2, to_list/2]).
-export_type([cursor/0]).

-opaque cursor() :: {cursor,
        mungo@client:collection(),
        integer(),
        integer(),
        gleam@iterator:iterator(bison@bson:value())}.

-spec new(mungo@client:collection(), integer(), list(bison@bson:value())) -> cursor().
new(Collection, Id, Batch) ->
    {cursor,
        Collection,
        Id,
        erlang:length(Batch),
        gleam@iterator:from_list(Batch)}.

-spec get_more(cursor(), integer()) -> {ok, cursor()} |
    {error, mungo@error:error()}.
get_more(Cursor, Timeout) ->
    Cmd = [{<<"getMore"/utf8>>, {int64, erlang:element(3, Cursor)}},
        {<<"collection"/utf8>>,
            {string, erlang:element(2, erlang:element(2, Cursor))}},
        {<<"batchSize"/utf8>>, {int32, erlang:element(4, Cursor)}}],
    _pipe = gleam@erlang@process:try_call(
        erlang:element(3, erlang:element(2, Cursor)),
        fun(_capture) -> {command, Cmd, _capture} end,
        Timeout
    ),
    _pipe@1 = gleam@result:replace_error(_pipe, actor_error),
    _pipe@2 = gleam@result:flatten(_pipe@1),
    _pipe@4 = gleam@result:map(
        _pipe@2,
        fun(Reply) -> case gleam@dict:get(Reply, <<"cursor"/utf8>>) of
                {ok, {document, Reply_cursor}} ->
                    case {gleam@dict:get(Reply_cursor, <<"id"/utf8>>),
                        gleam@dict:get(Reply_cursor, <<"nextBatch"/utf8>>)} of
                        {{ok, {int64, Id}}, {ok, {array, Batch}}} ->
                            _pipe@3 = new(erlang:element(2, Cursor), Id, Batch),
                            {ok, _pipe@3};

                        {_, _} ->
                            {error, structure_error}
                    end;

                _ ->
                    {error, structure_error}
            end end
    ),
    gleam@result:flatten(_pipe@4).

-spec next(cursor(), integer()) -> {gleam@option:option(bison@bson:value()),
    cursor()}.
next(Cursor, Timeout) ->
    case gleam@iterator:step(erlang:element(5, Cursor)) of
        {next, Doc, Rest} ->
            {{some, Doc},
                {cursor,
                    erlang:element(2, Cursor),
                    erlang:element(3, Cursor),
                    erlang:element(4, Cursor),
                    Rest}};

        done ->
            case erlang:element(3, Cursor) of
                0 ->
                    {none,
                        {cursor,
                            erlang:element(2, Cursor),
                            0,
                            erlang:element(4, Cursor),
                            gleam@iterator:empty()}};

                _ ->
                    _assert_subject = get_more(Cursor, Timeout),
                    {ok, New_cursor} = case _assert_subject of
                        {ok, _} -> _assert_subject;
                        _assert_fail ->
                            erlang:error(#{gleam_error => let_assert,
                                        message => <<"Assertion pattern match failed"/utf8>>,
                                        value => _assert_fail,
                                        module => <<"mungo/cursor"/utf8>>,
                                        function => <<"next"/utf8>>,
                                        line => 39})
                    end,
                    case gleam@iterator:step(erlang:element(5, New_cursor)) of
                        {next, Doc@1, Rest@1} ->
                            {{some, Doc@1},
                                {cursor,
                                    erlang:element(2, Cursor),
                                    erlang:element(3, New_cursor),
                                    erlang:element(4, New_cursor),
                                    Rest@1}};

                        done ->
                            {none,
                                {cursor,
                                    erlang:element(2, Cursor),
                                    erlang:element(3, New_cursor),
                                    erlang:element(4, New_cursor),
                                    gleam@iterator:empty()}}
                    end
            end
    end.

-spec to_list_internal(cursor(), list(bison@bson:value()), integer()) -> list(bison@bson:value()).
to_list_internal(Cursor, Storage, Timeout) ->
    case next(Cursor, Timeout) of
        {{some, Next}, New_cursor} ->
            to_list_internal(New_cursor, lists:append(Storage, [Next]), Timeout);

        {none, _} ->
            Storage
    end.

-spec to_list(cursor(), integer()) -> list(bison@bson:value()).
to_list(Cursor, Timeout) ->
    to_list_internal(Cursor, [], Timeout).
