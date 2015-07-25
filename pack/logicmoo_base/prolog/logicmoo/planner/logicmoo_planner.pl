% end_of_file.

:- dynamic   user:file_search_mudFootPath/2.
:- multifile user:file_search_mudFootPath/2.
:- prolog_load_context(directory,Dir),
   DirFor = planner,
   (( \+ user:file_search_mudFootPath(DirFor,Dir)) ->asserta(user:file_search_mudFootPath(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_mudFootPath(pack,Y)) ->asserta(user:file_search_mudFootPath(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).


:- prolog_load_context(directory,Dir),asserta(user:file_search_mudFootPath(planner,Dir)).

:- show_call_entry(with_no_mpred_expansions(ensure_loaded(logicmoo_ocl_and_pddl))).


:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_bb_env)).
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_structs)).
:- show_call_entry(with_no_mpred_expansions(user:ensure_loaded(planner((logicmoo_hyhtn))))).


% :- with_no_mpred_expansions(ensure_loaded(moo_ext_strips)).
