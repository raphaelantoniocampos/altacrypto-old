-record(collection, {
    name :: binary(),
    client :: gleam@erlang@process:subject(mungo@client:message())
}).
