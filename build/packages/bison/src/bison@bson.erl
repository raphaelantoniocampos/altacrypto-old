-module(bison@bson).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([value/0, binary_/0]).

-type value() :: min |
    max |
    na_n |
    null |
    infinity |
    {js, binary()} |
    {int32, integer()} |
    {int64, integer()} |
    {double, float()} |
    {boolean, boolean()} |
    {binary, binary_()} |
    {string, binary()} |
    {date_time, birl:time()} |
    negative_infinity |
    {array, list(value())} |
    {object_id, bison@object_id:object_id()} |
    {timestamp, integer(), integer()} |
    {regex, binary(), binary()} |
    {document, gleam@dict:dict(binary(), value())}.

-type binary_() :: {md5, bison@md5:md5()} |
    {uuid, bison@uuid:uuid()} |
    {custom, bison@custom:custom()} |
    {generic, bison@generic:generic()}.


