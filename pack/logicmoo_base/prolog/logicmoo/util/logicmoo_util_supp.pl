/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   TeamSPoon
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.prologmoo.com
    SCM:           https://github.com/TeamSPoon/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/
% :- if(\+ current_predicate(system:must_or_die/1)).
:- module(logicmoo_util_supp,[must_or_die/1,must_atomic/1,nop/1,setup_call_cleanup_each/3]).
%:- endif.
:- '$set_source_module'(system).
:- meta_predicate
      must_atomic(0),
      must_notrace(0),
      must_or_die(0),      
      setup_call_cleanup_each(0,0,0),
      call_cleanup_each(0,0).


:- set_prolog_flag(scce,pure).
	 
:- dynamic(scce0/0).
:- retractall(scce0).
r0(X):- scce4(writeln(start),(between(1,3,X),writeln(X)), writeln(end)).

r0(REF,X):-
 scce4(
         (asserta(scce0,REF),nl,nl,w(enter(REF:X))),
         (between(1,3,X),w(goal(REF:X))),
         (erase(REF),w(cleanup(REF:X)),nl)),
 \+ call(scce0),
 w(success(REF:X)),nl,nl.


w(G):-notrace(dmsg(G)).



:- use_module(library(logicmoo_utils)).

:- meta_predicate scce_orig(0,0,0).
:- meta_predicate scce3(0,0,0).


scce4(S,G,C):-
  b_setval(setup4,v(S,G,C)),
  nb_linkval(setup4,v(S,G,C)),
  setup_call_cleanup(setup3,
   (((G,deterministic(Det),true) *->
     (Det == true -> ! ; (cleanup4;(setup4,fail))))),
   cleanup4), 
   (exit_4(S,G,C)).
  
exit_4(S,G,C):-nb_getval(cleanup4,v(S1,G1,C1)),ignore(S=S1),ignore(G=G1),ignore(C=C1).

cleanup4:- nb_getval(cleanup4,v(_S1,_G1,C1)),call(C1).
setup3:- 
  nb_getval(setup4,Orig),Orig=v(S,_G,_C),
  copy_term(Orig,Copy),Copy=v(S0,G0,C0),  
  call(S0),nb_setval(cleanup4,v(S,G0,C0)).
setup4:- 
  nb_getval(setup4,Orig),
  copy_term(Orig,v(S0,G0,C0)),
  call(S0),nb_setval(cleanup4,v(S0,G0,C0)).


:- if(\+ current_predicate(system:must_or_die/1)).

:- module_transparent(must_or_die/1).
must_or_die(G):- (G *-> true ; throw(failed_must_or_die(G))).

:- module_transparent(must_atomic/1).
must_atomic(Goal):- notrace('$sig_atomic'(must_or_die(Goal))).


call_cleanup_each(Goal, Cleanup) :-
	setup_call_cleanup_each(true, Goal, Cleanup).

:- endif.

%% nop( ?VALUE1) is semidet.
%
% Nop.
%
:- if( \+ current_predicate(nop/1)).
:- export(nop/1).
nop(_).
:- endif.
:- if( \+ current_predicate(setup_call_cleanup_each/3)).
:- export(setup_call_cleanup_each/3).
:- meta_predicate(setup_call_cleanup_each(0,0,0)).
setup_call_cleanup_each(Setup,Goal,Cleanup):-  \+ current_prolog_flag(scce,pure), !, setup_call_cleanup(Setup,Goal,Cleanup).
setup_call_cleanup_each(Setup,Goal,Cleanup):-
     catch((
        call((must_atomic(Setup),Goal,deterministic(Det),true))
        *->
        (Det == true
          -> (must_atomic(Cleanup),!)
          ; (must_atomic(Cleanup);(must_atomic(Setup),fail)))
     ; (must_atomic(Cleanup),!,fail)),
     E, (ignore(must_atomic(Cleanup)),throw(E))).

:- endif.

