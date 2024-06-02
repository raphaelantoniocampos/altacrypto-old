-module(app@models@order).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([order/0]).

-type order() :: {buy_order,
        bison@object_id:object_id(),
        binary(),
        birl@interval:interval(),
        binary(),
        float(),
        float()} |
    {sell_order,
        bison@object_id:object_id(),
        binary(),
        birl@interval:interval(),
        app@models@asset:asset()}.


