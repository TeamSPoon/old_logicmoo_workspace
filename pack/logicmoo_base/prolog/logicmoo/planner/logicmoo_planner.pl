
:- prolog_load_context(directory,Dir),asserta(user:file_search_path(planner,Dir)).

:- with_no_mpred_expansions(ensure_loaded(pddl_robert_sasak)).
:- with_no_mpred_expansions(ensure_loaded(moo_ext_strips)).
:- with_no_mpred_expansions(ensure_loaded(dbase_i_hyhtn)). 




