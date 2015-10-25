
:- module(pqr,[]).
:- baseKB:use_module(library(logicmoo/logicmoo_user)).
:- w_tl(t_l:user_abox(baseKB), baseKB:( checkKB:m1 )).
:- set_user_abox(pqr).
:- begin_pfc.

:- listing(t_l:user_abox/1).
:- mpred_trace.
%:- baseKB:compile_predicates([baseKB:(==>)/1]).
%:- baseKB:compile_predicates([baseKB:(==>)/2]).
:- debug(mpred).
:- module(pqr).
==> p(1).
==> tCol(r).
p(X) ==> q(X).
q(X) ==> r(X).
?- listing(pqrTBox:_).
?- listing(pqr:_).




