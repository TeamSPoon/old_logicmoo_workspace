
:- module(adb_pfc,[]).

:- use_module(library(logicmoo/logicmoo_user)).

not(P),P ==> contrradiction.

bird(X), ~not(fly(X)) ==> fly(X).

penguin(X) ==> bird(X).

penguin(X) ==> not(fly(X)).

bird(X), injured(X) ==> not(fly(X)).

bird(X), dead(X) ==> not(fly(X)).
