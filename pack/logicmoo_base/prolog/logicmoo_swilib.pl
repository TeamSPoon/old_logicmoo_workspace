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
:- if(( system:use_module(library('logicmoo/util/logicmoo_util_clause_expansion.pl')), push_modules)). 
:- endif.
:- module(logicmoo_swilib,[]).
% restore entry state
:- reset_modules.


% ======================================================
% Preload library so that autoloading is not used
% ======================================================

% HTTP related autoloads
:- use_module(http_exception:library(settings)).

% XPCE related autoloads

:- system:use_module(library(pce),except([op(_,_,_)])).
:- system:use_module(library(swi_compatibility)).
:- system:use_module(library(pce_util)).
:- system:use_module(library(pce_emacs)).
:- system:use_module(library(swi_ide)).
:- system:use_module(library(pce_edit)).
:- system:use_module(library(edit_dialog)).
:- system:use_module(library(swi_preferences)).
:- system:use_module(library(pce_manual)).
:- system:use_module(library(gui_tracer)).
:- system:use_module(library(pce_meta)).
:- system:use_module(library(portray_object)).
:- system:use_module(library(keybinding)).
:- system:use_module(library(emacs_tags)).
:- system:use_module(library(pce_require)).
:- system:use_module(library(pce_debug)).
:- system:use_module(library(help_message)).
:- system:use_module(library(toolbar)).
:- system:use_module(library(plot/plotter)).

:- system:use_module(library(imageops)).
:- system:use_module(library(pce_float_item)).

:- user:use_module(library(pce_report)).
:- user:use_module(library('swi/pce_debug_monitor')).
:- user:use_module(library('swi/thread_monitor')).

:- call((system:use_module(library(pce_report)))). % ,except([colour/2])))).
:- call((system:use_module(library('swi/pce_debug_monitor')))). %,except([colour/2,resource/3])))).
:- call((system:use_module(library('swi/thread_monitor')))).


:- system:use_module(library(statistics)).
:- system:use_module(library(dialect/hprolog),[]).


% ======================================================
% Rest of the standard library
% ======================================================
:- system:use_module(library(backcomp), [
            '$arch'/2,
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
:- system:use_module(library(terms),[term_hash/2,		% @Term, -HashKey
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

:- if(current_predicate(system:mode/1)).
:- system:use_module(library(quintus),except([mode/1])). 
:- else.
:- system:use_module(library(quintus)). 
:- endif.


:- dynamic http:location/3.
:- multifile http:location/3.

:- system:use_module(library(prolog_autoload)).
:- system:use_module(library(prolog_clause)).
:- system:use_module(library(occurs)).
:- system:use_module(library(listing)).
:- user:use_module(library(clpfd)).
:- system:use_module(library(qsave)).
:- system:use_module(library(apply)).
:- system:use_module(library(debug)).
:- system:use_module(library(error)).
:- system:use_module(library(lists)).
:- system:use_module(library(operators)).
:- system:use_module(library(option)).
:- system:use_module(library(prolog_source)).
:- system:use_module(library(prolog_history)).
:- system:use_module(library(ansi_term)).
:- system:use_module(library(prolog_xref)).
:- system:use_module(library(readutil)).
:- system:use_module(library(shlib)).
:- system:use_module(library(url)).

:- if(exists_source(library(unix))).
:- system:use_module(library(unix)).
:- endif.


% probably 
:- system:use_module(library(rdf_ntriples),[rdf_ntriple_part/4]).
:- system:use_module(library(tty),[menu/3]).



:- forall(filematch(swi(('library/*.pl')),M),
 ignore((
   \+ (member(C,['/terms.pl','/backcomp.pl','/r.pl','/index.pl',rdf,pengi,win_men,swicli,'swicli.pl',swicffi,quintus,solution_sequences,metaterm,coind,drac,'INDEX',jpl,nb_set,yall,settings]), atom_contains(M,C)),
   \+ (member(C,[persistency,chr,rewrite,bdb,check,xpath,record]),atom_contains(M,C)),
   catch(system:use_module(M,except([op(_,_,_)])),E,wdmsg(E))))).

:- include(library(pldoc/hooks)).

:- if(exists_source(library(pldoc))).
:- system:use_module(library(pldoc), []).
	% Must be loaded before doc_process
:- system:use_module(library(pldoc/doc_process)).
:- endif.

%:- system:use_module(library(pldoc/doc_library)).
%:- doc_load_library.

:- system:use_module(library(pldoc/doc_access)).
:- system:use_module(library(pldoc/doc_pack)).

:- system:use_module(library(doc_http)).
:- reexport(library(pldoc/doc_html)).
:- system:use_module(library(pldoc/doc_wiki)).
:- system:use_module(library(pldoc/doc_search)).
:- system:use_module(library(pldoc/doc_util)).
:- system:use_module(library(pldoc/doc_library)).

:- system:use_module(library(http/thread_httpd)).
:- system:use_module(library(http/http_error)).
:- system:use_module(library(http/http_client)).

% http_reply_from_files is here
:- system:use_module(library(http/http_files)).
% http_404 is in here
:- system:use_module(library(http/http_dispatch)).

:- system:use_module(library(http/http_dispatch)).
:- system:use_module(library(http/html_write),except([op(_,_,_)])).
:- system:use_module(library(http/html_head)).
:- system:use_module(library(http/http_session)).
:- system:use_module(library(http/http_parameters)).
:- system:use_module(library(http/http_server_files)).
:- system:use_module(library(http/http_wrapper)).

:- autoload.

:- M=pldoc_process,ignore((module_property(M,file(S)),
   source_file(PI,S),
   \+ ((predicate_property(M:PI,imported_from(U)),U\==M)),
   functor(PI,F,A),import(F/A),fail)).


