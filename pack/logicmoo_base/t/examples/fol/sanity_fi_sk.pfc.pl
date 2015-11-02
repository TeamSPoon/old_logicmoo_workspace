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

% :- include('test_header.pfc').
:- use_module(library(logicmoo/logicmoo_user)).

:- set_user_abox(baseKB).
%=  setup pfc
:- set_current_module(baseKB).
:- begin_pfc.
:- sanity(get_user_abox(baseKB)).

:- process_this_script.

%= save compiled clauses using forward chaining storage (by default)
%= we are using forward chaining just so any logical errors, performance and program bugs manefest
%= immediately
:- set_clause_compile(fwc).

%= ````
%= logic tests...
%= ````
%:- debug(mpred).

%= trudy is human
human(trudy).
human(eileen).
human(douglas).
mother(trudy,eileen).
mother(eileen,douglas).

%= catch a regression bug that may couse trudy to lose human assertion
never_retract_u(human(trudy)).

forall(c,exists([m,f], if(human(c), (mother(m,c) & father(f,c))))).

:- must(\+ mother(skArg1ofMotherFn(_),eileen)).

:- printAll(must(father(_,_))).
:- printAll(must(mother(_,_))).





