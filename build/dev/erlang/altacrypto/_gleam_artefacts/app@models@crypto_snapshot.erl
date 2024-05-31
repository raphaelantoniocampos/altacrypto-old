-module(app@models@crypto_snapshot).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([crypto_snapshot/0]).

-type crypto_snapshot() :: {crypto_snapshot, binary(), birl:time(), float()}.


