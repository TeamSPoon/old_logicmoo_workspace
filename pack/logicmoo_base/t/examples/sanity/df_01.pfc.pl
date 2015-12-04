#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles
%  cls ; kill -9 %1 ; fg ; swipl -g "ensure_loaded(pack(logicmoo_base/t/examples/base/'sanity_abc.pfc'))."

:- module(sanity,[]).

:- use_module(library(logicmoo_base)).

:- begin_pfc.

:- include('df_include.pfc').

:-dynamic((a/1,b/1,c/1)).

a(X) ==> c(X).
most(c(X) ==> b(X)) .
a(1).

:- listing([a/1,b/1,c/1,(==>)/2,most/1,pt,nt,bt]).

:- mpred_test(b(1)).

:- mpred_why(b(1)).

% Justifications for b(1):
%     1.1 \+ ~(b(1))
%     1.2 ==>(\+ ~(b(1)),b(1))


~b(1).

:- mpred_test(\+ b(1)).

