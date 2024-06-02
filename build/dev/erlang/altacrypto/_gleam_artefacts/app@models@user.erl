-module(app@models@user).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export_type([user/0, user_settings/0]).

-type user() :: {user,
        bison@object_id:object_id(),
        binary(),
        binary(),
        binary(),
        binary(),
        user_settings(),
        float(),
        birl:time(),
        binary()}.

-type user_settings() :: {user_settings, boolean(), float(), float()}.


