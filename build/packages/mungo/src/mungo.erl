-module(mungo).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([start/2, next/2, to_list/2, collection/2, count_all/2, count/3, find_by_id/3, insert_one/3, insert_many/3, find_all/3, delete_one/3, delete_many/3, find_many/4, find_one/4, update_one/5, update_many/5]).

-spec start(binary(), integer()) -> {ok,
        gleam@erlang@process:subject(mungo@client:message())} |
    {error, gleam@otp@actor:start_error()}.
start(Uri, Timeout) ->
    mungo@client:start(Uri, Timeout).

-spec next(mungo@cursor:cursor(), integer()) -> {gleam@option:option(bison@bson:value()),
    mungo@cursor:cursor()}.
next(Cursor, Timeout) ->
    mungo@cursor:next(Cursor, Timeout).

-spec to_list(mungo@cursor:cursor(), integer()) -> list(bison@bson:value()).
to_list(Cursor, Timeout) ->
    mungo@cursor:to_list(Cursor, Timeout).

-spec collection(gleam@erlang@process:subject(mungo@client:message()), binary()) -> mungo@client:collection().
collection(Db, Name) ->
    _pipe = Db,
    mungo@client:collection(_pipe, Name).

-spec count_all(mungo@client:collection(), integer()) -> {ok, integer()} |
    {error, mungo@error:error()}.
count_all(Collection, Timeout) ->
    _pipe = Collection,
    mungo@crud:count_all(_pipe, Timeout).

-spec count(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
count(Collection, Filter, Timeout) ->
    _pipe = Collection,
    mungo@crud:count(_pipe, Filter, Timeout).

-spec find_by_id(mungo@client:collection(), binary(), integer()) -> {ok,
        gleam@option:option(bison@bson:value())} |
    {error, mungo@error:error()}.
find_by_id(Collection, Id, Timeout) ->
    _pipe = Collection,
    mungo@crud:find_by_id(_pipe, Id, Timeout).

-spec insert_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, bison@bson:value()} | {error, mungo@error:error()}.
insert_one(Collection, Doc, Timeout) ->
    _pipe = Collection,
    mungo@crud:insert_one(_pipe, Doc, Timeout).

-spec insert_many(
    mungo@client:collection(),
    list(list({binary(), bison@bson:value()})),
    integer()
) -> {ok, mungo@crud:insert_result()} | {error, mungo@error:error()}.
insert_many(Collection, Docs, Timeout) ->
    _pipe = Collection,
    mungo@crud:insert_many(_pipe, Docs, Timeout).

-spec find_all(
    mungo@client:collection(),
    list(mungo@crud:find_option()),
    integer()
) -> {ok, mungo@cursor:cursor()} | {error, mungo@error:error()}.
find_all(Collection, Options, Timeout) ->
    _pipe = Collection,
    mungo@crud:find_all(_pipe, Options, Timeout).

-spec delete_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
delete_one(Collection, Filter, Timeout) ->
    _pipe = Collection,
    mungo@crud:delete_one(_pipe, Filter, Timeout).

-spec delete_many(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, integer()} | {error, mungo@error:error()}.
delete_many(Collection, Filter, Timeout) ->
    _pipe = Collection,
    mungo@crud:delete_many(_pipe, Filter, Timeout).

-spec find_many(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list(mungo@crud:find_option()),
    integer()
) -> {ok, mungo@cursor:cursor()} | {error, mungo@error:error()}.
find_many(Collection, Filter, Options, Timeout) ->
    _pipe = Collection,
    mungo@crud:find_many(_pipe, Filter, Options, Timeout).

-spec find_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    integer()
) -> {ok, gleam@option:option(bison@bson:value())} |
    {error, mungo@error:error()}.
find_one(Collection, Filter, Projection, Timeout) ->
    _pipe = Collection,
    mungo@crud:find_one(_pipe, Filter, Projection, Timeout).

-spec update_one(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    list(mungo@crud:update_option()),
    integer()
) -> {ok, mungo@crud:update_result()} | {error, mungo@error:error()}.
update_one(Collection, Filter, Change, Options, Timeout) ->
    _pipe = Collection,
    mungo@crud:update_one(_pipe, Filter, Change, Options, Timeout).

-spec update_many(
    mungo@client:collection(),
    list({binary(), bison@bson:value()}),
    list({binary(), bison@bson:value()}),
    list(mungo@crud:update_option()),
    integer()
) -> {ok, mungo@crud:update_result()} | {error, mungo@error:error()}.
update_many(Collection, Filter, Change, Options, Timeout) ->
    _pipe = Collection,
    mungo@crud:update_many(_pipe, Filter, Change, Options, Timeout).
