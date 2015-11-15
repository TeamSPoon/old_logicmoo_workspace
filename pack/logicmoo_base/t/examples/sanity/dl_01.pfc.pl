#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles


%  cls ; kill -9 %1 ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

:- module(sanity,[]).

:- use_module(library(logicmoo_base)).

:- begin_pfc.

:- dynamic(most/1).

meta_argtypes(most(ftAssertable)).

% most(P==>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test))  ==> ((P, \+ R/Test ) ==> Q).
most(P==>Q)/nonvar(Q) ==> (P ==> most(Q)).
most(P)/mpred_literal_nv(P)  ==>  (( \+ ~P ) ==> P ).


most(Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)) ==> ( (~R/Test) ==> Q ).
most(Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)) ==> ( ( (\+ R)/Test) ==> Q ).
% most(Q) ==> if_missing(Q,Q).

(most((Q <- P))/mpred_literal(Q)) ==> (Q <-(P, \+ ~(Q))).
%(most(P=>Q)/(mpred_literal_nv(Q),if_missing_mask(Q,R,Test)))  ==> ((P, ~R/Test) => Q).
%(most(P=>Q)/nonvar(Q)) ==> (P => most(Q)).

:-dynamic((a/1,b/1,c/1)).

a(X) ==> c(X).
a(1).

most(c(X) ==> b(X)) .

:- listing([a/1,b/1,c/1,(==>)/2,most/1,pt,nt,bt]).

:- mpred_test(b(1)).

