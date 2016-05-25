/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(mt_01,[]).

:- user:use_module(library(logicmoo_utils)).
:- user:use_module(library(logicmoo_base)).
% :- user:use_module(library(logicmoo_user)).


:- begin_pfc.

:- set_defaultAssertMt(myMt).

baseKB:mtCycL(socialMt).

socialMt:loves(sally,joe).

:- set_defaultAssertMt(myMt).

:- mpred_test(clause_u(socialMt:loves(_,_))).
:- mpred_test(\+clause_u(myMt:loves(_,_))).



