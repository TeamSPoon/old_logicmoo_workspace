/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(mt_01b,[]).

:- user:use_module(library(logicmoo_base)).

:- begin_pfc.

loves(sally,joe).

:- mpred_test(clause_u(mt_01b:loves(_,_))).

:- mpred_test(\+clause_u(baseKB:loves(_,_))).



