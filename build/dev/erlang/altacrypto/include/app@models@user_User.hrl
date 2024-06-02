-record(user, {
    id :: bison@object_id:object_id(),
    login :: binary(),
    name :: binary(),
    api_key :: binary(),
    secret_key :: binary(),
    user_settings :: app@models@user:user_settings(),
    usd_balance :: float(),
    created_at :: birl:time(),
    hashed_password :: binary()
}).
