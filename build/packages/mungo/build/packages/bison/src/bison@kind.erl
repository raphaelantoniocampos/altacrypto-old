-module(bison@kind).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([kind/0, sub_kind/0]).

-type kind() :: {kind, bitstring()}.

-type sub_kind() :: {sub_kind, bitstring()}.


