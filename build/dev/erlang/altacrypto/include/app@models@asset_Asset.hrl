-record(asset, {
    user_id :: bison@object_id:object_id(),
    symbol :: binary(),
    quantity :: float(),
    purchase_price :: float(),
    purchase_datetime :: birl:time(),
    highest_price :: float(),
    current_price :: float(),
    id :: bison@object_id:object_id(),
    should_be_sold :: boolean(),
    sold :: birl:time()
}).
