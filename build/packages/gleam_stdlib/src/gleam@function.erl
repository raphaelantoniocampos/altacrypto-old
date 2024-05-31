-module(gleam@function).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2, apply1/2, apply2/3, apply3/4]).

-spec compose(fun((ETV) -> ETW), fun((ETW) -> ETX)) -> fun((ETV) -> ETX).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((ETY, ETZ) -> EUA)) -> fun((ETY) -> fun((ETZ) -> EUA)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((EUC, EUD, EUE) -> EUF)) -> fun((EUC) -> fun((EUD) -> fun((EUE) -> EUF))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((EUH, EUI, EUJ, EUK) -> EUL)) -> fun((EUH) -> fun((EUI) -> fun((EUJ) -> fun((EUK) -> EUL)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((EUN, EUO, EUP, EUQ, EUR) -> EUS)) -> fun((EUN) -> fun((EUO) -> fun((EUP) -> fun((EUQ) -> fun((EUR) -> EUS))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((EUU, EUV, EUW, EUX, EUY, EUZ) -> EVA)) -> fun((EUU) -> fun((EUV) -> fun((EUW) -> fun((EUX) -> fun((EUY) -> fun((EUZ) -> EVA)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((EVC, EVD) -> EVE)) -> fun((EVD, EVC) -> EVE).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(EVF) -> EVF.
identity(X) ->
    X.

-spec constant(EVG) -> fun((any()) -> EVG).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(EVI, fun((EVI) -> any())) -> EVI.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.

-spec apply1(fun((EVK) -> EVL), EVK) -> EVL.
apply1(Fun, Arg1) ->
    Fun(Arg1).

-spec apply2(fun((EVM, EVN) -> EVO), EVM, EVN) -> EVO.
apply2(Fun, Arg1, Arg2) ->
    Fun(Arg1, Arg2).

-spec apply3(fun((EVP, EVQ, EVR) -> EVS), EVP, EVQ, EVR) -> EVS.
apply3(Fun, Arg1, Arg2, Arg3) ->
    Fun(Arg1, Arg2, Arg3).
