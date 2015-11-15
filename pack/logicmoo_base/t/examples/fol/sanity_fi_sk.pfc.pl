#!/usr/bin/env swipl

:- module(sanity_fi_sk,[]).
/*
:- nodebug(_).
:- use_module(library(gui_tracer)).
:- noguitracer.
:- guitracer.
:- debug.
:- visible(+all).
:- leash(-all).
:- leash(+exception).
%:- ((trace,ls)).
:- noguitracer.
:- notrace(leash(+all)).
:- notrace.
*/
:- use_module(library(logicmoo_user)).

%=  setup pfc
:- begin_pfc.

:- process_this_script.

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

%= ````
%= logic tests...
%= ````

%= trudy is human
human(trudy).
human(eileen).
human(douglas).
mother(douglas,eileen).


%= catch a regression bug that may couse trudy to lose human assertion
never_retract_u(human(trudy)).
never_assert_u(mother(trudy,das)).

%= these we want but i am trigging some breakpoints
% never_assert_u(father(_,_)).
% never_assert_u(mother(trudy,_)).

:- mpred_trace_exec.

clif(forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f)))))).

:- must(clif(forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))))).

mother(eileen,trudy).

:- must(\+ mother(eileen,skArg1ofMother_1Fn(_))).

:- ain(father(douglas,russ)).

:- printAll(must(father(_,_))).
:- printAll(must(mother(_,_))).





