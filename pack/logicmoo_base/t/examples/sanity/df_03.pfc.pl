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

:- dynamic((most/1,mostly/1)).

meta_argtypes(most(ftAssertable)).

% BWD chaining
(most((Q <- P))/mpred_literal(Q)) ==> (Q <-(P, \+ ~(Q))).

% FWD chaining
most(P==>Q)/nonvar(Q) ==> (P ==> most(Q)).

% NEG chaining
most(~P)/nonvar(P)  ==>  (( \+ P ) ==> ~ P ).

% POS chaining
most(P)/mpred_positive_literal(P)  ==>  mostly(P).



mostly(P) ==> (( \+ ~P ) ==> P ).
mostly(P)/(if_missing_mask(P,Q,Test)) ==> ( (~Q/Test) ==> P ).
mostly(P)/(if_missing_mask(P,Q,Test)) ==> ( ( (\+ Q)/Test) ==> P ).

% mostly(Q) ==> if_missing(Q,Q).

:-dynamic((a/1,b/1,c/1)).

a(X) ==> c(X).
most(c(X) ==> b(X)) .
a(1).


:- listing([a/1,b/1,c/1,(==>)/2,most/1,mostly/1,pt,nt,bt]).

:- mpred_test(b(1)).

:- mpred_why(b(1)).


