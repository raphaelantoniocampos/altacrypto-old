-module(app@pages@layout).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([layout/1]).

-spec layout(list(lustre@internals@vdom:element(ACFN))) -> lustre@internals@vdom:element(ACFN).
layout(Elements) ->
    lustre@element@html:html(
        [],
        [lustre@element@html:head(
                [],
                [lustre@element@html:title([], <<"Altacrypto"/utf8>>),
                    lustre@element@html:meta(
                        [lustre@attribute:name(<<"viewport"/utf8>>),
                            lustre@attribute:attribute(
                                <<"content"/utf8>>,
                                <<"width=device-width, initial-scale=1"/utf8>>
                            )]
                    ),
                    lustre@element@html:link(
                        [lustre@attribute:rel(<<"stylesheet"/utf8>>),
                            lustre@attribute:href(<<"/static/app.css"/utf8>>)]
                    )]
            ),
            lustre@element@html:body([], Elements)]
    ).
