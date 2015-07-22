end_of_file.

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = planner,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
% [Required] Load the Logicmoo Library Utils
:- user:ensure_loaded(library(logicmoo/util/logicmoo_util_all)).


:- prolog_load_context(directory,Dir),asserta(user:file_search_path(planner,Dir)).

:- show_call_entry(with_no_mpred_expansions(ensure_loaded(logicmoo_ocl_and_pddl))).


:- show_call_entry(user:ensure_loaded(planner(logicmoo_util_bb_env))).
:- show_call_entry(user:use_module(planner(logicmoo_util_structs))).
:- show_call_entry(with_no_mpred_expansions(user:ensure_loaded(planner((logicmoo_hyhtn))))).

:- with_assertions((user:term_expansion(_,_):-!,fail),
    gripe_time(7,time(user:load_files([library(el_holds/'el_assertions.qlf')],[if(not_loaded )])))).



% :- with_no_mpred_expansions(ensure_loaded(moo_ext_strips)).




