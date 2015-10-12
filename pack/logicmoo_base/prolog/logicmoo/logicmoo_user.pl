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

:- module(logicmoo_user,[]).
:- multifile '$was_imported_kb_content$'/2.
:- dynamic '$was_imported_kb_content$'/2.
:- discontiguous('$was_imported_kb_content$'/2).
:- module(logicmoo_user).


:- set_prolog_flag(report_error,true).
:- set_prolog_flag(fileerrors,false).
:- set_prolog_flag(access_level,system).
:- set_prolog_flag(debug_on_error,true).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(gc,false).
:- set_prolog_flag(optimise,false).
:- set_prolog_flag(last_call_optimisation,false).
:- debug.


% :- ensure_loaded(logicmoo_utils).
:- use_module(logicmoo_base).

m1:- gripe_time(40,ensure_loaded(logicmoo(mpred_online/mpred_www))),if_defined(mpred_www:ensure_webserver), make,list_undefined.

% :- hook_message_hook.
:- set_prolog_flag(verbose_autoload,false).
:- set_prolog_flag(verbose_load,true).
m2:- (F = mpred/_),foreach(must(lmconf:mpred_is_impl_file(F)),must_det_l((dmsg(list_file_preds(F)),ensure_loaded(F),export_file_preds(F),list_file_preds(F)))).

m3:- rtrace(ensure_mpred_system).

m4:- w_tl(tlbugger:ifHideTrace,(ensure_mpred_file_loaded(mpred/mpred_builtin))).






end_of_file.

















%

:- multifile lmconf:startup_option/2. 
:- dynamic lmconf:startup_option/2. 

:- ensure_loaded(logicmoo_utils).

lmconf:startup_option(datalog,sanity). %  Run datalog sanity tests while starting
lmconf:startup_option(clif,sanity). %  Run datalog sanity tests while starting




/*
:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.
:- user:prolog_load_context(directory,Dir),
   %Dir = (DirThis/planner),
   DirFor = logicmoo,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(file_search_path(pack,Y));true).
:- user:attach_packs.
:- initialization(user:attach_packs).

% [Required] Load the Logicmoo Library Utils
% lmconf:mpred_is_impl_file(logicmoo(logicmoo_utils)).

:- user:file_search_path(logicmoo,_)-> true; (user:prolog_load_context(directory,Dir),asserta(user:file_search_path(logicmoo,Dir))).

:- dynamic(lmconf:isa_pred_now_locked/0).
*/

% :- include(mpred/'mpred_header.pi').



/*
:- meta_predicate call_mpred_body(*,0).
:- meta_predicate decl_mpred_hybrid_ilc_0(*,*,0,*).
:- meta_predicate assert_isa_hooked(0,*).
*/
:- meta_predicate t(7,?,?,?,?,?,?,?).
:- meta_predicate t(6,?,?,?,?,?,?).
:- meta_predicate t(5,?,?,?,?,?).
:- meta_predicate t(3,?,?,?).
:- meta_predicate t(4,?,?,?,?).
:- meta_predicate t(2,?,?).


% ========================================
% lmconf:mpred_user_kb/1
% ========================================

% TODO uncomment the next line without breaking it all!
% lmconf:use_cyc_database.

:-asserta(lmconf:pfcManageHybrids).






% ================================================
% Debugging settings
% ================================================

:-export(is_stable/0).

is_stable:-fail.

:- if(current_prolog_flag(optimise,true)).
is_recompile.
:- else.
is_recompile:-fail.
:- endif.

fast_mud.
xperimental:-fail.
xperimental_big_data:-fail.

simple_code :- fail.
save_in_mpred_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.
% slow_sanity(A):-nop(A).
:- meta_predicate xtreme_debug(0).
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, sanity(P).
xtreme_debug(_).

:- meta_predicate sanity(0).
sanity(P):- \+ is_recompile, (true; is_release),!,nop(P).
sanity(P):- on_x_rtrace(hotrace(P)),!.
sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.
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
:- gripe_time(40,use_module(logicmoo(mpred_online/mpred_www))).
% user:term_expansion((:-module(Name,List)), :-maplist(export,List)):- atom(Name),atom_concat(mpred_,_,Name).
% user:term_expansion((:-use_module(Name)), :-true):- atom(Name),atom_concat(mpred_,_,Name).



:- asserta(t_l:disable_mpred_term_expansions_locally).

% user:goal_expansion(ISA,G) :- compound(ISA),t_l:is_calling,use_was_isa(ISA,I,C),to_isa_out(I,C,OUT),G=no_repeats(OUT).
:- meta_predicate(lmbase_expander(?,?,?,?)).
:- meta_predicate(lmbase_record_transactions_maybe(?,?)).
:- meta_predicate(mpred_file_expansion(?,?)).


% :- read_source_files.
% logicmoo_html_needs_debug.
:- if((lmconf:startup_option(www,sanity),if_defined(logicmoo_html_needs_debug))).
:- write(ready),nl,flush_output.
:- prolog.
:- endif.
:-   call(with_mfa_of( (dynamic_safe)),user,user,boxlog_to_compile(_D,_E,_F),boxlog_to_compile/3).
:- retractall(t_l:disable_mpred_term_expansions_locally).


:- list_undefined.
