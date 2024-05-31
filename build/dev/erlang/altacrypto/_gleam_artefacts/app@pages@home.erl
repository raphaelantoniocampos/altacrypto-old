-module(app@pages@home).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([root/0]).

-spec root() -> lustre@internals@vdom:element(any()).
root() ->
    lustre@element@html:'div'(
        [lustre@attribute:class(<<"app"/utf8>>)],
        [lustre@element@html:h1(
                [lustre@attribute:class(<<"app-title"/utf8>>)],
                [lustre@element:text(<<"Altacrypto"/utf8>>)]
            ),
            lustre@element@html:'div'(
                [lustre@attribute:class(<<"container"/utf8>>)],
                [lustre@element@html:h2(
                        [lustre@attribute:class(<<"login-title"/utf8>>)],
                        [lustre@element:text(<<"Login"/utf8>>)]
                    ),
                    lustre@element@html:form(
                        [lustre@attribute:class(<<"add-todo-input"/utf8>>),
                            lustre@attribute:method(<<"POST"/utf8>>),
                            lustre@attribute:action(<<"/login/"/utf8>>)],
                        [lustre@element@html:input(
                                [lustre@attribute:name(<<"todo_title"/utf8>>),
                                    lustre@attribute:class(
                                        <<"add-todo-input__input"/utf8>>
                                    ),
                                    lustre@attribute:placeholder(
                                        <<"Username"/utf8>>
                                    ),
                                    lustre@attribute:autofocus(true)]
                            )]
                    )]
            ),
            lustre@element@html:'div'(
                [lustre@attribute:class(<<"container"/utf8>>)],
                [lustre@element@html:h2(
                        [lustre@attribute:class(<<"login-title"/utf8>>)],
                        [lustre@element:text(<<"Password"/utf8>>)]
                    ),
                    lustre@element@html:form(
                        [lustre@attribute:class(<<"add-todo-input"/utf8>>),
                            lustre@attribute:method(<<"POST"/utf8>>),
                            lustre@attribute:action(<<"/login/"/utf8>>)],
                        [lustre@element@html:input(
                                [lustre@attribute:name(<<"todo_title"/utf8>>),
                                    lustre@attribute:class(
                                        <<"add-todo-input__input"/utf8>>
                                    ),
                                    lustre@attribute:placeholder(
                                        <<"Password"/utf8>>
                                    ),
                                    lustre@attribute:autofocus(true)]
                            )]
                    )]
            )]
    ).
