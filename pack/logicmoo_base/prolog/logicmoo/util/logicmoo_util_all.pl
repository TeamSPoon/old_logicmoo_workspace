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

:- dynamic(lmconf:logicmoo_utils_separate/0).
:- retractall(lmconf:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).

:- multifile(lmhook:mpred_hook_init_files/0).
:- dynamic(lmhook:mpred_hook_init_files/0).

:- if( \+ current_predicate(lmconfig:mpred_user_kb/1)).
:- multifile(lmconfig:mpred_user_kb/1).
:- dynamic(lmconfig:mpred_user_kb/1).
:- endif.
:- if( \+ current_predicate(lmconfig:mpred_system_kb/1)).
:- multifile(lmconfig:mpred_system_kb/1).
:- dynamic(lmconfig:mpred_system_kb/1).
:- endif.
:- lmconfig:mpred_system_kb(_)->true;('$module'(M,M),asserta(lmconfig:mpred_system_kb(M))).


:- dynamic(lmconf:mpred_is_impl_file/1).
:- multifile(lmconf:mpred_is_impl_file/1).
:- volatile(lmconf:mpred_is_impl_file/1).



