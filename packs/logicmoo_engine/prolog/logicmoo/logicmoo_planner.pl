
:- nodebug(_).

:- ensure_loaded(planner/pddl_robert_sasak).
:- (with_no_mpred_expansions(ensure_loaded(planner/moo_ext_strips))).
%:- (with_no_mpred_expansions(ensure_loaded(planner/dbase_i_hyhtn))).



