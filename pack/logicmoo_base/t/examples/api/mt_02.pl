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

%:- add_import_module(mt_01,baseKB,end).

:- set_defaultAssertMt(myMt).

:- begin_pfc.

predicateConventionMt(loves/2,socialMt).

mt1:like(sally,joe).

genlMt(mt1,socialMt).

like(Sally,Joe)==>loves(Joe,Sally).

:- mpred_must(loves(joe,sally)).


