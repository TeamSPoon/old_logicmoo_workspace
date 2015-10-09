#!/usr/bin/env swipl
/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_utils).'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_utils).' 1.0.0
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
%:- if(( \+ current_predicate(lmconf:logicmoo_utils_separate/0);( \+lmconf:logicmoo_utils_separate))).

:- module(logicmoo_utils,[]).

:- dynamic(lmconf:logicmoo_utils_separate/0).
:- retractall(lmconf:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).

:- multifile(lmhook:mpred_hook_init_files/0).
:- dynamic(lmhook:mpred_hook_init_files/0).

:- if( \+ current_predicate(lmconf:mpred_user_kb/1)).
:- multifile(lmconf:mpred_user_kb/1).
:- dynamic(lmconf:mpred_user_kb/1).
:- endif.
:- if( \+ current_predicate(lmconf:mpred_system_kb/1)).
:- multifile(lmconf:mpred_system_kb/1).
:- dynamic(lmconf:mpred_system_kb/1).
:- endif.
:- lmconf:mpred_system_kb(_)->true;('$module'(M,M),asserta(lmconf:mpred_system_kb(M))).

% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).
add_file_search_path(Name,Dir):- is_absolute_file_name(Dir),!,exists_directory(Dir),(( \+ user:file_search_path(Name,Dir)) ->asserta(user:file_search_path(Name,Dir));true).
add_file_search_path(Name,UpUp):- 
   (prolog_load_context(directory,SDir);(prolog_load_context(file,File),file_directory_name(File,SDir)),working_directory(SDir,SDir)),
   absolute_file_name(UpUp,Dir,[relative_to(SDir),file_type(directory)]),
   exists_directory(Dir),!,is_absolute_file_name(Dir),add_file_search_path(Name,Dir).
   
% ======================================================
% Add Extra pack-ages directory
% ======================================================
:- add_file_search_path(pack,'../../../').
:- initialization(attach_packs).

% ======================================================
% Save a directory of *this* file into logicmoo(..)
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- add_file_search_path(logicmoo,'.').

% :- include(logicmoo(util/logicmoo_util_header)).

% ======================================================
% Pre-release Sanity tests
% ======================================================
:- dynamic(lmconf:logicmoo_pre_release/0).
lmconf:logicmoo_pre_release.
:- if(lmconf:logicmoo_pre_release).


:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),set_prolog_flag(debug, true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(100000)]).
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).
:- set_prolog_flag(verbose_load,true).

:- if(current_prolog_flag(gui,true)).
% :- guitracer.
:- notrace(trace).
:- notrace.
:- endif.

:- endif.
% ======================================================
% Pre-release Should check if autoloading messes up anything
% ======================================================
:- dynamic(lmconf:logicmoo_scan_autoloads/0).
% lmconf:logicmoo_scan_autoloads
:- if(lmconf:logicmoo_scan_autoloads).
:- set_prolog_flag(verbose_autoload, false).
:- autoload.
:- set_prolog_flag(verbose_autoload, true).
:- endif.

% ======================================================
% Create one big logicmoo_utils module
% ======================================================
:- multifile((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
:- dynamic((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
%logicmoo_utils:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(logicmoo_util_,_,Name).
logicmoo_utils:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).

% ======================================================
% Include separated logicmoo util file
% ======================================================
:- lmconf:mpred_system_kb(M), M:((
 ensure_loaded(util/logicmoo_util_first),
 ensure_loaded(util/logicmoo_util_database),
 ensure_loaded(util/logicmoo_util_bugger_catch),
 ensure_loaded(util/logicmoo_util_with_assertions),
 ensure_loaded(util/logicmoo_util_loop_check),
 ensure_loaded(util/logicmoo_util_dmsg),
 ensure_loaded(util/logicmoo_util_bugger),
 ensure_loaded(util/logicmoo_util_ctx_frame),
 ensure_loaded(util/logicmoo_util_filestreams),
 ensure_loaded(util/logicmoo_util_filesystem),
 ensure_loaded(util/logicmoo_util_multivar),
 ensure_loaded(util/logicmoo_util_no_repeats),
 ensure_loaded(util/logicmoo_util_preddefs),
 ensure_loaded(util/logicmoo_util_prolog_frames),
%  ensure_loaded(util/logicmoo_util_prolog_streams),
 ensure_loaded(util/logicmoo_util_term_listing),
 ensure_loaded(util/logicmoo_util_terms),
 ensure_loaded(util/logicmoo_util_varnames))).

/*
:- 'mpred_trace_none'(tlbugger:dont_skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:rtracing/0).
*/
% :- 'mpred_trace_none'(tlbugger:_/0).

% :- start_rtrace.

% :- ensure_loaded(util/logicmoo_util_structs).
%:- ensure_loaded(util/logicmoo_util_bb_env).
%:- ensure_loaded(util/logicmoo_util_bb_gvar).
% :- ensure_loaded(util/logicmoo_util_coroutining_iz).
% :- ensure_loaded(util/logicmoo_util_coroutining_was).
% :- use_module(util/logicmoo_util_dcg).
% :- ensure_loaded(util/logicmoo_util_help).

:- lmconf:mpred_system_kb(M),M:use_module(util/logicmoo_util_strings).

unused:- M= logicmoo_utils, 
     doall((current_predicate(_,M:P),functor(P,F,A),
         \+ predicate_property(M:P,imported_from(_)), show_call((must( export(M:F/A)), \+ predicate_property(M:P, transparent),must( module_transparent(M:F/A)))))).

unused:- M= lmhook, 
     doall((current_predicate(_,M:P),functor(P,F,A),
         \+ predicate_property(M:P,imported_from(_)), show_call((must( export(M:F/A)), \+ predicate_property(M:P, transparent),must( module_transparent(M:F/A)))))).

/*

:- M= user, 
     doall((current_predicate(_,M:P),functor(P,F,A),
         \+ predicate_property(M:P,imported_from(_)), \+ predicate_property(M:P, transparent),must( module_transparent(M:F/A)))).
*/

% :- write_modules.

% :- start_rtrace.
:- set_prolog_flag(xlisting,true).


i_have_a_name(AndThisIsIt,_Singleton,_,_):-writeq(AndThisIsIt),nl.

:- listing(i_have_a_name/4).

% :-prolog.

?- list_undefined.

:- user: use_module(util/logicmoo_util_term_listing).
:- user: use_module(util/logicmoo_util_bugger_catch).

?- logicmoo_util_term_listing:xlisting(get_gtime).

% Found new meta-predi
