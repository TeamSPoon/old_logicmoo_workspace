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
:- if(( system:use_module(system:library('logicmoo/util/logicmoo_util_filesystem.pl')), push_modules)). 
:- endif.
:- module(logicmoo_utils_file,[]).
% restore entry state
:- reset_modules.

:- if( \+ current_predicate(system:setup_call_cleanup_each/3)).
:- use_module(system:library('logicmoo/util/logicmoo_util_supp.pl')).
:- endif.

% ======================================================
% Included separated logicmoo util files
% ======================================================
/*
:- module_transparent(user:term_expansion/1).
user:term_expansion(EOF,POS,O,POS2):- 
 is_file_based_expansion(term,EOF,POS,O,POS2),
 nonvar(EOF),
 (EOF=end_of_file;EOF=(:-(module(_,_)))),
 prolog_load_context(module,M),
 M\==user, 
 ignore((
    source_location(S,_),
    '$current_typein_module'(TM),
    glean_prolog_impl_file(EOF,S,M,TM))),fail.

*/
:- set_prolog_flag(system:generate_debug_info, true).
:- set_prolog_flag(generate_debug_info, true).

:- use_module(system:library('logicmoo/util/logicmoo_util_database.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_first.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_catch.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_dmsg.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_rtrace.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_varnames.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_bugger.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_loop_check.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_no_repeats.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_scce.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_terms.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_dumpst.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_with_assertions.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_shared_dynamic.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_preddefs.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_attvar_reader.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_term_listing.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_strings.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_filestreams.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_prolog_frames.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_prolog_streams.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_engines.pl')).
:- use_module(system:library('logicmoo/util/logicmoo_util_help.pl')).



% ======================================================
% Preload library so that autoloading is not used
% ======================================================

% HTTP related autoloads
:- use_module(http_exception:library(settings)).

% XPCE related autoloads
:- use_module(system:library(swi_compatibility)).
:- use_module(system:library(pce_util)).
:- use_module(system:library(pce_emacs)).
:- use_module(system:library(pce),except([op(_,_,_)])).

% ======================================================
% Rest of the standard library
% ======================================================
:- use_module(system:library(backcomp), [ '$arch'/2,
	    '$version'/1,
	    '$home'/1,
	    '$argv'/1,
	    '$set_prompt'/1,
	    '$strip_module'/3,
	    '$declare_module'/3,
	    '$module'/2,
	    at_initialization/1,	% :Goal
	    displayq/1,
	    displayq/2,
	    sformat/2,			% -String, +Fmt
	    sformat/3,			% -String, +Fmt, +Args
	    concat/3,
	    concat_atom/2,		% +List, -Atom
	    concat_atom/3,		% +List, +Sep, -Atom
	    '$apropos_match'/2,		% +Needle, +Hashstack
	    read_clause/1,		% -Term
	    read_clause/2,		% +Stream, -Term
	    read_variables/2,		% -Term, -VariableNames
	    read_variables/3,		% +Stream, -Term, -VariableNames
	    read_pending_input/3,	% +Stream, -List, ?Tail
	    feature/2,
	    set_feature/2,
	    substring/4,
	    string_to_list/2,		% ?String, ?Codes
	    string_to_atom/2,		% ?String, ?Atom
	    flush/0,
	    write_ln/1,			% +Term
	    proper_list/1,		% @Term
	    free_variables/2,		% +Term, -Variables
	    subsumes_chk/2,		% @Generic, @Specific
	    subsumes/2,			% @Generic, @Specific
	    hash_term/2,		% +Term, -Hash
	    checklist/2,		% :Goal, +List
	    sublist/3,			% :Goal, +List, -Sublist
	    sumlist/2,			% +List, -Sum
	    convert_time/2,		% +Stamp, -String
	    convert_time/8,		% +String, -YMDmhs.ms
	    'C'/3,			% +List, -Head, -Tail
	    current_thread/2,		% ?Thread, ?Status
	    current_mutex/3,		% ?Mutex, ?Owner, ?Count
	    message_queue_size/2,	% +Queue, -TermsWaiting
	    lock_predicate/2,		% +Name, +Arity
	    unlock_predicate/2,		% +Name, +Arity
	    current_module/2,		% ?Module, ?File
	    export_list/2,		% +Module, -Exports
	    setup_and_call_cleanup/3,	% :Setup, :Goal, :Cleanup
	    setup_and_call_cleanup/4,	% :Setup, :Goal, ?Catcher, :Cleanup
	    merge/3,			% +List1, +List2, -Union
	    merge_set/3,		% +Set1, +Set2, -Union
	    index/1,			% :Head
	    hash/1,			% :PI
	    set_base_module/1		% :Base
	  ]).
:- use_module(system:library(terms),[term_hash/2,		% @Term, -HashKey
	    term_hash/4,		% @Term, +Depth, +Range, -HashKey
	   % term_variables/2,		% @Term, -Variables
	    term_variables/3,		% @Term, -Variables, +Tail
	    variant/2,			% @Term1, @Term2
	   % subsumes/2,			% +Generic, @Specific
	   % subsumes_chk/2,		% +Generic, @Specific
	    cyclic_term/1,		% @Term
	   % acyclic_term/1,		% @Term
	    term_subsumer/3,		% +Special1, +Special2, -General
	    term_factorized/3]).

:- use_module(system:library(quintus),except([mode/1])).

:- forall(filematch(swi(('library/*.pl')),M),
 ignore((
   \+ (member(C,['/terms.pl','/backcomp.pl',rdf,pengi,win_men,swicli,'swicli.pl',swicffi,quintus,solution_sequences,metaterm,coind,drac,'INDEX',jpl,nb_set,yall,settings]), atom_contains(M,C)),
   \+ (member(C,[persistency,chr,rewrite,bdb,check,xpath,record]),atom_contains(M,C)),
   use_module(system:M)))).


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
%user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(logicmoo_util_,_,Name).


:- thread_local logicmoo_utils_test_tl/0.
:- w_tl((logicmoo_utils_test_tl:-dmsg("Adding logicmoo/utils to autoload path",[])),logicmoo_utils_test_tl).

/*
% the next are loaded idomaticalyl later (if needed)
% :- use_module(system:library('logicmoo/util/logicmoo_util_ctx_frame.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_dra.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_bb_gvar.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_bb_env.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_dcg.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_varfunctors.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_structs.pl')).
% :- use_module(system:library('logicmoo/util/logicmoo_util_supp.pl')).
*/



