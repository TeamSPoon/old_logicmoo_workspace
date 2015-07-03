
:- nodebug(_).

:- with_no_mpred_expansions(ensure_loaded(pddl_robert_sasak)).
:- with_no_mpred_expansions(ensure_loaded(moo_ext_strips)).
%:- with_no_mpred_expansions(ensure_loaded(dbase_i_hyhtn)).

:- if((gethostname(titan),true)).
:- prolog.
:- endif.


