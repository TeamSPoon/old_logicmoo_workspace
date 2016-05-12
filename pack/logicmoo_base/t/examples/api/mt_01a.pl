/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(mt_01a,[]).

:- user:use_module(library(logicmoo_base)).

:- begin_pfc.

predicateConventionMt(loves,socialMt).

:- set_defaultAssertMt(myMt).

loves(sally,joe).

:- set_defaultAssertMt(myMt).

:- mpred_test(clause_u(socialMt:loves(_,_))).
:- mpred_test(\+clause_u(myMt:loves(_,_))).



