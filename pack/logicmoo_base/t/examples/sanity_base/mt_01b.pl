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

:- ensure_loaded(library(pfc)).

:- begin_pfc.

loves(sally,joe).

:- mpred_test(clause_u(mt_01b:loves(_,_))).

:- mpred_test(\+clause_u(baseKB:loves(_,_))).


end_of_file.


%TODO Make a test to show new inheretence 

inheritableRelation(a/1).

nonInheritableRelation(a/1).

kb1:a(1).
kb2:a(2).
kb3:a(3).

:- import_module(kb2,kb1).

kb2: ?- a(W).

W=2.
% feaurta added
W=1.

