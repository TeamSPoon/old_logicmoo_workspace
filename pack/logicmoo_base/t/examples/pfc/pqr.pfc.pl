
:- module(pqr,[]).
:- use_module(library(logicmoo/logicmoo_user)).
:- begin_pfc.
:- set_support_module(pqrSupport).

p(X) ==> q(X).
q(X) ==> r(X).
==> p(1).
