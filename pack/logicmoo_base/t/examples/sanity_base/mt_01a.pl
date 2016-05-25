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

% :- use_module(library(logicmoo_base)).

:- set_defaultAssertMt(mt_01a).

:- begin_pfc.

:- set_defaultAssertMt(myMt).

:- begin_pfc.

predicateConventionMt(loves,socialMt).

loves(sally,joe).

:- mpred_test(clause_u(socialMt:loves(_,_))).
:- mpred_test(\+clause_u(myMt:loves(_,_))).



