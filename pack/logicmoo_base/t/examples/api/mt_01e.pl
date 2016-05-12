/* <module>
%
%  PFC is a language extension for prolog.
%
%  It adds a new type of module inheritance
%
% Dec 13, 2035
% Douglas Miles
*/
:- module(mt_01e,[]).

:- user:use_module(library(logicmoo_base)).

:- begin_pfc.

:- mpred_test(mtProlog(mt_01e)).
:- mpred_test(\+ mtCycL(mt_01e)).
:- mpred_test(\+ mtCycL(kb1)).

genMt(kb1,mt_01e).

:- mpred_test(\+ mtProlog(kb1)).

:- mpred_test(mtCycL(kb1)).


