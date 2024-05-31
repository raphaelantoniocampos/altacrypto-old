-record(cursor, {
    collection :: mungo@client:collection(),
    id :: integer(),
    batch_size :: integer(),
    iterator :: gleam@iterator:iterator(bison@bson:value())
}).
