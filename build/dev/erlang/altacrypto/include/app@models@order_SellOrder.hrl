-record(sell_order, {
    user_id :: bison@object_id:object_id(),
    side :: binary(),
    interval :: birl@interval:interval(),
    asset :: app@models@asset:asset()
}).
