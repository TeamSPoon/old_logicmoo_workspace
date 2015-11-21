#!/usr/bin/env swipl
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
% Dec 13, 2035
% Douglas Miles



:- use_module(library(logicmoo_base)).

:- dmsg(begin_abc).
              
:- file_begin(pfc).

:- abolish(a,1).
:- abolish(b,1).
:- dynamic((a/1,b/1)).

:- debug(mpred).
:- mpred_trace_exec.


a(t).
b(t).
a(X) ==> b(X).


:- mpred_test(a(t)).
:- mpred_test(b(t)).



\+ a(t).

:- mpred_test(\+ a(t)).
:- mpred_test(b(t)).
