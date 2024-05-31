-record(pipeline, {
    collection :: mungo@client:collection(),
    options :: list(mungo@aggregation:aggregate_option()),
    timeout :: integer(),
    stages :: gleam@queue:queue(list({binary(), bison@bson:value()}))
}).
