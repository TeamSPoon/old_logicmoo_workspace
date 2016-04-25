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


:- if(\+ current_predicate(system:must_or_die/1)).

:- module_transparent(must_or_die/1).
must_or_die(G):- (G *-> true ; throw(failed_must_or_die(G))).

:- module_transparent(must_atomic/1).
must_atomic(Goal):- notrace('$sig_atomic'(must_or_die(Goal))).

:- module_transparent(must_atomic/1).
must_notrace(Goal):- notrace('$sig_atomic'(must_or_die(Goal))).

:- endif.

:- if(\+ current_predicate(system:call_cleanup_each/2)).
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
setup_call_cleanup_each(Setup,Goal,Cleanup):- current_prolog_flag(scce,Pred), !, call(Pred,Setup,Goal,Cleanup).
setup_call_cleanup_each(Setup,Goal,Cleanup):- setup_call_cleanup(Setup,Goal,Cleanup).

:- endif.

