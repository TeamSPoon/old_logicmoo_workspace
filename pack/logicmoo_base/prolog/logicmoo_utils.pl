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
:- if((
   user:use_module(user:library('logicmoo/util/logicmoo_util_filesystem.pl')),
   push_modules,
   UTILS = logicmoo_utils,
   TBOX = baseKB,
   current_smt(SM,M),
   ignore((
   SM\==user,
   
   maybe_add_import_module(SM,TBOX),
   maybe_add_import_module(SM,UTILS),
   system:asserta(lmconf:source_typein_boxes(SM,M,SM:TBOX)))))).
:- endif.

:- module(logicmoo_utils_file,[]).

:- '$set_source_module'('logicmoo_utils').

:- user:use_module(user:library('logicmoo/util/logicmoo_util_filesystem.pl')).
% :- ensure_loaded('./logicmoo/util/logicmoo_util_filesystem').
% :- autoload([verbose(false)]).
% :- set_prolog_flag(autoload, false).

:- module_transparent(user:term_expansion/1).
user:term_expansion(EOF,POS,O,POS2):- 
 is_file_based_expansion(term,EOF,POS,O,POS2),
 nonvar(EOF),
 (EOF=end_of_file;EOF=(:-(module(_,_)))),
 prolog_load_context(module,M),
 M\==logicmoo_utils, 
 ignore((
    source_location(S,_),
    '$current_typein_module'(TM),
    glean_prolog_impl_file(EOF,S,M,TM))),fail.

:- set_prolog_flag(access_level,system).

:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_first.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_catch.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_dmsg.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_rtrace.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_varnames.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_bugger.pl')).


:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_attvar_reader.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_with_assertions.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_ctx_frame.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_database.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_dra.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_dumpst.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_engines.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_filestreams.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_help.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_loop_check.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_no_repeats.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_scce.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_terms.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_preddefs.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_prolog_frames.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_prolog_streams.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_shared_dynamic.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_strings.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_term_listing.pl')).
:- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_bb_gvar.pl')).
% the next are loaded idomaticalyl later (if needed)
% :- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_bb_env.pl')).
% :- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_dcg.pl')).
% :- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_varfunctors.pl')).
% :- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_structs.pl')).
% :- user:ensure_loaded(user:library('logicmoo/util/logicmoo_util_supp.pl')).

:- if( \+ current_predicate(baseKB:setup_call_cleanup_each/3)).
:- user:ensure_loaded(system:library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

:- set_prolog_flag(access_level,user).

:- dynamic(lmconf:logicmoo_utils_separate/0).
:- retractall(lmconf:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


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
:- if( \+ exists_source(logicmoo(logicmoo_engine))).
:- add_file_search_path(logicmoo,'./logicmoo/').
:- exists_source(logicmoo(logicmoo_engine)).
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
%use_libraries(M):- F= (util/_),foreach(lmconf:mpred_is_impl_file(logicmoo_utils,F),(writeln(M:use_module(F)),M:ensure_loaded(F))).

%:- export(use_libraries/0). 
%use_libraries:- source_context_module(M),use_libraries(M).

:- multifile(lmconf:mpred_is_impl_file/2).
:- dynamic(lmconf:mpred_is_impl_file/2).

%= 	 	 

%% mpred_is_impl_file(?Type, ?A) is semidet.
%
% Hook To [lmconf:mpred_is_impl_file/2] For Module Logicmoo_utils.
% Managed Predicate If Is A Implimentation File.
%
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_first).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_database).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_catch).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_with_assertions).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_loop_check).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_dmsg).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_ctx_frame).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_filestreams).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_bugger).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_filesystem).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_multivar).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_no_repeats).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_preddefs).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_prolog_frames).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_prolog_streams).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_term_listing).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_terms).
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_varnames). 
 lmconf:mpred_is_impl_file(logicmoo_utils,logicmoo/util/logicmoo_util_strings). 

%:- use_module(library(logicmoo/util/logicmoo_util_bugger)).
%:- use_module(library(logicmoo/util/logicmoo_util_first)).
%:- use_module(library(logicmoo/util/logicmoo_util_catch)).

:- thread_local logicmoo_utils_test_tl/0.
:- w_tl((logicmoo_utils_test_tl:-dmsg("Adding logicmoo/utils to autoload path",[])),logicmoo_utils_test_tl).

%:- autoload([verbose(true)]).
%:- autoload([verbose(false)]).
% ?- logicmoo_util_term_listing:xlisting(get_gtime).

% ?- list_undefined.

% :- predicate_property(M:maplist(_,_,_),exported),maybe_add_import_module(baseKB,M,end).
/*
:- 'mpred_trace_none'(tlbugger:dont_skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:skip_bugger/0).
:- 'mpred_trace_none'(tlbugger:rtracing/0).
*/

:- set_prolog_flag(autoload_logicmoo,true).
:- dmsg("Adding logicmoo/[snark|mpred_online] to autoload path",[]).
:- add_library_search_path('./logicmoo/snark/',[ '*.pl']).
:- add_library_search_path('./logicmoo/mpred/',[ 'mpred_*.pl']).
:- pop_modules.
:- use_module(library(logicmoo/mpred/mpred_loader)).

