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

% We save the name of the module loading this module
:- module(logicmoo_util_all,[]).
/*
:- dynamic(lmconf:logicmoo_utils_separate/0).
:- retractall(lmconf:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


:- if( \+ current_predicate(lmconfig:defaultAssertMt/1)).
:- multifile(lmconfig:defaultAssertMt/1).
:- dynamic(lmconfig:defaultAssertMt/1).
:- endif.
:- if( \+ current_predicate(defaultTBoxMt/1)).
:- multifile(defaultTBoxMt/1).
:- dynamic(defaultTBoxMt/1).
:- endif.
% :- defaultTBoxMt(_)->true;('$current_typein_module'(M),asserta(defaultTBoxMt(M))).


:- dynamic(lmconf:mpred_is_impl_file/2).
:- multifile(lmconf:mpred_is_impl_file/2).
:- volatile(lmconf:mpred_is_impl_file/2).


*/
