:- module(pqr,[]).


% :- qcompile(library(logicmoo/logicmoo_user)).
:- baseKB:use_module(library('logicmoo/logicmoo_user')).

:- begin_pfc.

==> p(1).
p(X) ==> q(X).
q(X) ==> r(X).

:- must(r(1)).

==> p(2).

:- must(r(2)).

  
