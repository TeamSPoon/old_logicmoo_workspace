/** <module> 
% File used as storage place for all predicates which change as
% the world is run.
%
% props(Obj,height(ObjHt)) == k(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == height(Obj,ObjHt)
% padd(Obj,height(ObjHt)) == padd(height,Obj,ObjHt,...) == add(QueryForm)
% kretract[all](Obj,height(ObjHt)) == kretract[all](Obj,height,ObjHt) == pretract[all](height,Obj,ObjHt) == del[all](QueryForm)
% keraseall(AnyTerm).
%
%
% Dec 13, 2035
% Douglas Miles
*/

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),asserta(user:file_search_path(logicmoo,Dir)).
:- dynamic(user:isa_pred_now_locked/0).

:- nb_setval(pldoc_object,pldoc_object_missing).

:- include(mpred/logicmoo_i_header).

/*
:- meta_predicate user:call_mpred_body(*,0).
:- meta_predicate user:decl_mpred_hybrid_ilc_0(*,*,0,*).
:- meta_predicate user:assert_isa_hooked(0,*).
*/
:- meta_predicate user:t(7,?,?,?,?,?,?,?).
:- meta_predicate user:t(6,?,?,?,?,?,?).
:- meta_predicate user:t(5,?,?,?,?,?).
:- meta_predicate user:t(3,?,?,?).
:- meta_predicate user:t(4,?,?,?,?).
:- meta_predicate user:t(2,?,?).


% ========================================
% user:mpred_mod/1
% ========================================

% TODO uncomment the next line without breaking it all!
% thglobal:use_cyc_database.

:-asserta(thglobal:pfcManageHybrids).

:- export(user:mpred_mod/1).
:- dynamic user:mpred_mod/1.
user:mpred_mod(user).


% [Manditory] define how we interact with the module system
:-if(not(current_predicate(swi_module/2))).
:-export(swi_module/2).
swi_module(M,E):-dmsg(swi_module(M,E)).
:-endif.





% ================================================
% Debugging settings
% ================================================

:-export(is_stable/0).
is_stable:-fail.

fast_mud.
xperimental:-fail.
xperimental_big_data:-fail.
simple_code :- fail.
save_in_mpred_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.
% slow_sanity(A):-nop(A).
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, verify_sanity(P).
xtreme_debug(_).

verify_sanity(P):-(true; is_release),!,nop(P).
verify_sanity(P):- debugOnError(hotrace(P)),!.
verify_sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.
:-meta_predicate(when_debugging(+,0)).
when_debugging(What,Call):- debugging(What),!,Call.
when_debugging(_,_).

% :- asserta(tlbugger:no_colors).
% :- asserta(tlbugger:show_must_go_on).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

% ================================================
% DBASE_T System
% ================================================

:- ensure_loaded(mpred/logicmoo_i_listing).
:- ensure_loaded(mpred/logicmoo_i_pfc).
:- ensure_loaded(mpred/logicmoo_i_loader).
:- ensure_loaded(mpred/logicmoo_i_naming).
:- ensure_loaded(mpred/logicmoo_i_types).
:- ensure_loaded(mpred/logicmoo_i_term_expansion).
:- ensure_loaded(mpred/logicmoo_i_mpred_props).
:- ensure_loaded(mpred/logicmoo_i_agenda).
:- ensure_loaded(mpred/logicmoo_i_call).
:- ensure_loaded(mpred/logicmoo_i_coroutining).
:- ensure_loaded(mpred/logicmoo_i_hooks).
:- ensure_loaded(mpred/logicmoo_i_store).
:- ensure_loaded(mpred/logicmoo_i_mpred_stubs).
:- ensure_loaded(mpred/logicmoo_i_argtypes).

:-dynamic(thlocal:pfc_already_in_file_expansion/1).

:- asserta(thlocal:disable_mpred_term_expansions_locally).

% user:goal_expansion(ISA,G) :- compound(ISA),thlocal:is_calling,use_was_isa(ISA,I,C),to_isa_out(I,C,OUT),G=no_repeats(OUT).
:-meta_predicate(lmbase_user_term_expansion(?,?)).
lmbase_user_term_expansion(I,OO):- nonvar(I),current_predicate(pfc_loader_file/0),current_predicate(logicmoo_bugger_loaded/0), 
  \+ thlocal:pfc_already_in_file_expansion(I), 
  with_assertions(thlocal:pfc_already_in_file_expansion(I),if_defined(pfc_file_expansion(I,OO))),!,I\=@=OO,
  nop(dmsg(pfc_file_expansion(I,OO))).

user:term_expansion(I,OO):- (I==end_of_file->(must(do_end_of_file_actions),fail);
                                 (\+ thlocal:disable_mpred_term_expansions_locally, 
                                     if_defined(lmbase_user_term_expansion(I,OO)),I\=@=OO)).

:-export(pfc_file_loaded/0).
pfc_file_loaded.

:- retractall(thlocal:disable_mpred_term_expansions_locally).

:- ensure_mpred_file_loaded(mpred/logicmoo_i_builtin).

:- asserta(thlocal:disable_mpred_term_expansions_locally).


