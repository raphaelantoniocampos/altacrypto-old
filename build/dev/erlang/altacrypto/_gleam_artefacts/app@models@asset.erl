-module(app@models@asset).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([asset/0]).

-type asset() :: {asset,
        bison@object_id:object_id(),
        binary(),
        float(),
        float(),
        birl:time(),
        float(),
        float(),
        bison@object_id:object_id(),
        boolean(),
        birl:time()}.


