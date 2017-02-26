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

% :- ensure_loaded(library(pfc)).

:- set_defaultAssertMt(mt_01a).

:- begin_pfc.

baseKB:mtCycL(socialMt).

:- set_defaultAssertMt(myMt).

:- begin_pfc.


baseKB:predicateConventionMt(loves,socialMt).

:- must((fix_mp(clause(_,_),loves(x,y),M,P),M:P==socialMt:loves(x,y))).

loves(sally,joe).

:- mpred_test(clause_u(socialMt:loves(_,_))).
:- mpred_test(\+clause_u(myMt:loves(_,_))).



