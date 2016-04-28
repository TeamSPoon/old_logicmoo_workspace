/* <module> Adds autoloading of LogicMOO Utilities predicates
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
:- module(logicmoo_utils,[]).

:- ensure_loaded('./logicmoo/util/logicmoo_util_filesystem').
:- module_transparent(user:term_expansion/1).
user:term_expansion(EOF,POS,O,POS2):- 
 is_file_based_expansion(term,EOF,POS,O,POS2),
 nonvar(EOF),
 EOF==end_of_file,
 prolog_load_context(module,M),
 M\==logicmoo_utils, 
 ignore((
    source_location(S,_),
    '$current_typein_module'(TM),
    glean_prolog_impl_file(EOF,S,M,TM))),fail.

:- dynamic(lmconf:logicmoo_utils_separate/0).
:- retractall(lmconf:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).

/*
:- ensure_loaded('./logicmoo/util/logicmoo_util_catch').
:- ensure_loaded('./logicmoo/util/logicmoo_util_varnames').
:- ensure_loaded('./logicmoo/util/logicmoo_util_dumpst').
:- ensure_loaded('./logicmoo/util/logicmoo_util_dmsg').
*/


% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).


% ======================================================
% Add Extra pack-ages directory
% ======================================================
% :- initialization(attach_packs,now).
:- if( \+ exists_source(pack(logicmoo_base/prolog/logicmoo/logicmoo_utils))).
:- add_file_search_path(pack,'../../').
% :- initialization(attach_packs,now).
:- endif.
% ======================================================
% Save a directory of *this* file into logicmoo(..)
% And adds the local directories to file search path of logicmoo(..)
% ======================================================
:- if( \+ exists_source(logicmoo(logicmoo_utils))).
:- add_file_search_path(logicmoo,'./logicmoo/').
:- endif.


% ======================================================
% Add Utils files to autoloads
% ======================================================
:- add_library_search_path('./logicmoo/util/',[ 'logicmoo_util_*.pl']).

% ======================================================
% Pre-release Sanity tests
% ======================================================
:- dynamic(lmconf:logicmoo_scan_autoloads/0).
:- dynamic(lmconf:logicmoo_pre_release/0).

%= 	 	 

%% logicmoo_pre_release is semidet.
%
% Hook To [lmconf:logicmoo_pre_release/0] For Module Logicmoo_utils.
% Logicmoo Pre Release.
%
lmconf:logicmoo_pre_release.

:- if(lmconf:logicmoo_pre_release).
/*
:- set_prolog_flag(report_error,true),set_prolog_flag(debug_on_error,true),set_prolog_flag(debug, true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(100000)]).
:- set_prolog_flag(verbose_load,true).
*/
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).

:- if(current_prolog_flag(gui,true)).
% :- guitracer.
% :- notrace(trace).
:- notrace.
:- endif.


%= 	 	 

%% logicmoo_scan_autoloads is semidet.
%
% Hook To [lmconf:logicmoo_scan_autoloads/0] For Module Logicmoo_utils.
% Logicmoo Scan Autoloads.
%
lmconf:logicmoo_scan_autoloads:-false.

:- endif.
% ======================================================
% Pre-release Should check if autoloading messes up anything
% ======================================================

:- if(lmconf:logicmoo_scan_autoloads).
%:- set_prolog_flag(verbose_autoload, false).
%:- autoload([verbose(false)]).
%:- set_prolog_flag(verbose_autoload, true).
:- endif.

% ======================================================
% Create one big logicmoo_utils module
% ======================================================
:- multifile((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
:- dynamic((term_expansion/2,user:term_expansion/2,system:term_expansion/2)).
%user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).
%user:term_expansion((:-ensure_loaded(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).

% ======================================================
% Included separated logicmoo util files
% ======================================================
%:- export(use_libraries/1). 
%use_libraries(M):- F= (util/_),foreach(lmconf:mpred_is_impl_file(F),(writeln(M:use_module(F)),M:ensure_loaded(F))).

%:- export(use_libraries/0). 
%use_libraries:- source_context_module(M),use_libraries(M).

:- multifile(lmconf:mpred_is_impl_file/1).
:- dynamic(lmconf:mpred_is_impl_file/1).

%= 	 	 

%% mpred_is_impl_file( ?A) is semidet.
%
% Hook To [lmconf:mpred_is_impl_file/1] For Module Logicmoo_utils.
% Managed Predicate If Is A Implimentation File.
%
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_first).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_database).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_catch).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_with_assertions).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_loop_check).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_dmsg).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_ctx_frame).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_filestreams).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_bugger).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_filesystem).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_multivar).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_no_repeats).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_preddefs).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_prolog_frames).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_prolog_streams).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_term_listing).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_terms).
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_varnames). 
 lmconf:mpred_is_impl_file(logicmoo/util/logicmoo_util_strings). 

%:- use_module(library(logicmoo/util/logicmoo_util_bugger)).
%:- use_module(library(logicmoo/util/logicmoo_util_first)).
%:- use_module(library(logicmoo/util/logicmoo_util_catch)).

:- thread_local logicmoo_utils_test_tl/0.
:- w_tl((logicmoo_utils_test_tl:-dmsg("Adding logicmoo/utils to autoload path",[])),logicmoo_utils_test_tl).

% :- autoload([verbose(false)]).

% ?- logicmoo_util_term_listing:xlisting(get_gtime).

% ?- list_undefined.


/*
:- 'mpred_trace_none'(tlbugger:dont_skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:rtracing/0).
*/

