
:- prolog_load_context(directory,Dir),asserta(user:file_search_path(planner,Dir)).

:- with_no_mpred_expansions(ensure_loaded(logicmoo_ocl_and_pddl)).
:- with_no_mpred_expansions(ensure_loaded(moo_ext_strips)).




