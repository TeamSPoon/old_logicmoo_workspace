#!/usr/bin/env swipl

:- module(sanity_fi,[]).

:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

must_is_entailed(G):- cwc, must(is_entailed(G)).
show_test(G):- cwc, get_user_abox(KB),printAll(must(KB:G)).

:- op(1100,fx,(shared_multifile)).

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
:- debug(mpred).
:- debug(mpred(_)).

%= trudy is human
human(trudy).
human(eileen).
human(douglas).
mother(eileen,trudy).
mother(douglas,eileen).

%= catch a regression bug that may couse trudy to lose human assertion
never_retract_u(human(trudy)).
never_assert_u(mother(trudy,_)).
never_assert_u(mother(trudy,das)).


never_assert_u(father(_,_)).
never_retract_u(father(_,_)).

:- mpred_trace_exec.

forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))).

:- must(clif(forall(c,exists([m,f], if(human(c), (mother(c,m) & father(c,f))))))).

:- must(\+ mother(eileen,skArg1ofMother_1Fn(_))).

:- printAll(must(father(_,_))).
:- printAll(must(mother(_,_))).





