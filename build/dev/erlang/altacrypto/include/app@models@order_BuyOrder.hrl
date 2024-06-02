-record(buy_order, {
    user_id :: bison@object_id:object_id(),
    side :: binary(),
    interval :: birl@interval:interval(),
    symbol :: binary(),
    variation :: float(),
    current_price :: float()
}).
