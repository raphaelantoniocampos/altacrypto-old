-record(update_result, {
    matched :: integer(),
    modified :: integer(),
    upserted :: list(bison@bson:value())
}).
