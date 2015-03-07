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
:- op(500,fx,'~').
:- op(1050,xfx,('=>')).
:- op(1050,xfx,'<=>').
:- op(1050,xfx,('<=')).
:- op(1100,fx,('=>')).
:- op(1150,xfx,('::::')).

:-nb_setval(pldoc_object,pldoc_object_missing).

:- use_module(library(semweb/turtle)).
:- include(dbase_i_header).
:- multifile(system:term_expansion/2).
:- multifile(user:term_expansion/2).
:- multifile(user:goal_expansion/2).
:- dynamic(user:isa_pred_now_locked/0).
:-thread_local user:prolog_mud_disable_term_expansions.
% [Manditory] define how we interact with the module system

:-if(not(current_predicate(swi_module/2))).
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
:-export(is_release/0).
is_release :- fail,1 is random(3).
:-export(not_is_release/0).
not_is_release :- true. % 1 is random(3).
simple_code :- fail.
save_in_dbase_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.
% slow_sanity(A):-nop(A).
xtreme_debug(P):- is_release,!,nop(P).
xtreme_debug(P):- not_is_release, verify_sanity(P).
xtreme_debug(_).

verify_sanity(P):-(true; is_release),!,nop(P).
verify_sanity(P):- debugOnError(notrace(P)),!.
verify_sanity(P):- dmsg('$ERROR_incomplete_SANITY'(P)),!.
:-meta_predicate_transparent(when_debugging(+,0)).
when_debugging(What,Call):- debugging(What),!,Call.
when_debugging(_,_).

% :- asserta(tlbugger:no_colors).
% :- asserta(tlbugger:show_must_go_on).

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

% ================================================
% Thread Locals
% ================================================
:- thread_local thlocal:consulting_sources/0.
:- thread_local thlocal:already_in_file_term_expansion/0.
:- thread_local thlocal:agent_current_action/2.
:- thread_local thlocal:caller_module/2.
:- thread_local thlocal:dbase_opcall/2.
:- thread_local thlocal:deduceArgTypes/1.
:- thread_local thlocal:agenda_slow_op_do_prereqs/0.
:- thread_local thlocal:enable_src_loop_checking/0.
:- thread_local thlocal:in_dynamic_reader/1.
:- thread_local thlocal:in_prolog_source_code/0.
:- thread_local thlocal:is_calling/0.
:- thread_local thlocal:infAssertedOnly/1.
:- thread_local thlocal:infInstanceOnly/1.
:- thread_local thlocal:infSkipArgIsa/0.
:- thread_local thlocal:infSkipFullExpand/0.
:- thread_local thlocal:into_form_code/0.
:- thread_local thlocal:inVoProp/0.
:- thread_local thlocal:no_arg_type_error_checking/0.
:- thread_local thlocal:noDBaseHOOKS/1.
:- thread_local thlocal:noDBaseMODs/1.
:- thread_local thlocal:noRandomValues/1.
:- thread_local thlocal:session_agent/2.
:- thread_local thlocal:agenda_suspend_scans/0.
:- thread_local thlocal:tracing80/0.
:- thread_local thlocal:useAltPOS/0.
:- thread_local thlocal:useOnlyExternalDBs/0.
:- thread_local thlocal:usePlTalk/0.
:- thread_local thlocal:with_callMPred/1.
:- thread_local thlocal:assert_op_override/1.



:- thread_local user:repl_to_string/2.
:- thread_local user:repl_writer/2.

:-dynamic(thlocal:infForward).
thlocal:infForward.
:- dynamic(dbase_module_ready).

% ========================================
% dbase_mod/1
% ========================================

:- export(dbase_mod/1).
:- dynamic dbase_mod/1.
dbase_mod(user).

% TODO uncomment the next line without breaking it all!
% thglobal:use_cyc_database.

:-asserta(thglobal:pfcManageHybrids).

% ========================================
% notice_predicate_head
% ========================================
notice_predicate_body(_).
notice_predicate_head(_):- not(thlocal:in_dynamic_reader(_)),!.
notice_predicate_head(H):- var(H),!.
notice_predicate_head((H1,H2)):-!,notice_predicate_head(H1),notice_predicate_head(H2).
notice_predicate_head((H1;H2)):-!,notice_predicate_head(H1),notice_predicate_head(H2).
notice_predicate_head((H1:-B1)):-!,notice_predicate_head(H1),notice_predicate_body(B1).
notice_predicate_head(H):- compound(H), must_compile_special_clause(H), get_functor(H,F,A),
           \+(current_predicate(F/A)),!,
           not(user:mpred_prop(F,prologOnly)),
           decl_mpred_hybrid(F/A).

expanded_already_functor(was_imported_kb_content).
expanded_already_functor(was_enabled).

% expanded_already_functor(F):-user:mpred_prop(F,prologOnly).

must_compile_special_clause(:- (_) ):-!,fail.
must_compile_special_clause(CL):- sanity(nonvar(CL)),not(thlocal:into_form_code),not(thlocal:already_in_file_term_expansion),not((get_functor(CL,F),expanded_already_functor(F))).


%user:goal_expanstion(H,_):- notice_predicate_head(H),fail.
%user:term_expanstion(H:-_,_):- must_compile_special_clause(H),notice_predicate_head(H),fail.

% makeConstant(X):- trace_or_throw(makeConstant(X)).

% ================================================
% A tiny bit of TMS
% ================================================


%OLD user:decl_database_hook(change(assert,_),Fact):- resolve_if_false(Fact).

was_known_false(Fact):-is_known_false(Fact),doall(retract((is_known_false(_):-true))),dmsg(trace_or_throw(error+was_known_false(Fact))).

resolve_if_false(Fact):- ignore(((is_known_false(Fact),was_known_false(Fact)))).

user:ruleRewrite(isa(isInstFn(Sub),Super),genls(Sub,Super)):-ground(Sub:Super),!.
user:ruleRewrite(mudLabelTypeProps(Lbl,T,[]),typeHasGlyph(T,Lbl)):-must(nonvar(T)).
user:ruleRewrite(mudLabelTypeProps(Lbl,T,Props),typeProps(T,[typeHasGlyph(Lbl)|Props])).


%OLD user:decl_database_hook(change(assert,_A_or_Z),typeProps(T,_)):- decl_type_safe(T).


%OLD user:decl_database_hook(change(assert,_A_or_Z),user:mpred_prop(F,W)):- sanity((nop(user:mpred_prop(F,W)),atom(F))).

%OLD user:decl_database_hook(change(assert,_),defnSufficient(FT,_)):- define_ft(FT).

%OLD user:decl_database_hook(change(assert,_),subFormat(FT,OFT)):- define_ft(OFT),define_ft(FT).


% ================================================
% Capturing Assertions
% ================================================
:- thread_local((record_on_thread/2)).
:- thread_local thlocal:dbase_capture/2.
:- thread_local thlocal:dbase_change/2.
while_capturing_changes(Call,Changes):-thread_self(ID),with_assertions(thlocal:dbase_capture(ID,_),(Call,get_dbase_changes(ID,Changes),clear_dbase_changes(ID))).
clear_dbase_changes(ID):-retractall(thlocal:dbase_change(ID,_)).
get_dbase_changes(ID,Changes):-findall(C,thlocal:dbase_change(ID,C),Changes).
%OLD user:decl_database_hook(AR,C):- record_on_thread(dbase_change,changing(AR,C)).
record_on_thread(Dbase_change,O):- thread_self(ID),thlocal:dbase_capture(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).



% ================================================
% DBASE_T System
% ================================================
%:- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(dbase/dbase_i_rdf_store)))).
%:- asserta(thglobal:using_rdf_dbase_hook).
:- ensure_loaded(dbase_i_kb_agenda).
:- ensure_loaded(dbase_i_mpred_pfc).
:- ensure_loaded(dbase_i_call).
:- ensure_loaded(dbase_i_coroutining).
:- ensure_loaded(dbase_i_pldoc).
:- ensure_loaded(dbase_i_db_preds).
:- ensure_loaded(dbase_i_term_expansion).
:- ensure_loaded(dbase_i_kb_store).
:- ensure_loaded(dbase_i_isa_subclass).


:- asserta((user:isa(I,C):-loop_check(isa_backchaing(I,C)))).
:- asserta(('$toplevel':isa(I,C):-user:isa(I,C))).

:- ensure_loaded(dbase_i_mpred_props).
:- ensure_loaded(dbase_i_mpred_stubs).
:- ensure_loaded(dbase_i_mpred_prolog).
:- ensure_loaded(dbase_i_mpred_dbase_t).
:- ensure_loaded(dbase_i_loader).
:- ensure_loaded(dbase_i_cyc).
:- ensure_loaded(dbase_i_propvals).

:- dynamic(isa/2).
:- decl_mpred_hybrid(isa/2).

:- ensure_loaded(dbase_i_formattypes).
:- ensure_loaded(dbase_i_deduce).
:- ensure_loaded(dbase_i_call_kb).
:- ensure_loaded(dbase_i_mpred_pttp).
:- ensure_loaded(dbase_i_mpred_snark).


user:goal_expansion(G,isa(I,C)):-G\=isa(_,_),(was_isa(G,I,C)),!.
user:term_expansion(G,isa(I,C)):-not(user:prolog_mud_disable_term_expansions),notrace((was_isa(G,I,C))).


dbase_module_ready.
:- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(dbase/dbase_i_rdf_store)))).

:- decl_mpred_hybrid(argIsa/3).
:- add_fast(<=( argIsa(F,N,Isa), argIsa_known(F,N,Isa))).

:-asserta(thlocal:pfcExpansion).
:-decl_mpred_prolog(resolveConflict/1).
:-decl_mpred_prolog(pfcSelect/1).

:-must((fully_expand_goal(_,:-multifile user:create_random_fact/1,O),show_call_failure(O=(:-multifile user:create_random_fact/1)))).

:-decl_type(tPred).
:-decl_mpred_hybrid(isa/2).

user:term_expansion(A,B):- not(user:prolog_mud_disable_term_expansions),in_file_expansion,once(pfc_file_expansion(A,B)),A\=@=B.

user:semweb_startup:- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(dbase/dbase_i_rdf_store)))).

:- with_no_term_expansions(if_file_exists(user_ensure_loaded(logicmoo(mobs/planner/dbase_i_hyhtn)))).
:-decl_type(predIsFlag).
:-decl_type(prologOnly).
:-decl_mpred_hybrid(formatted_resultIsa/2).
:-decl_mpred_hybrid(resultIsa/2).

system:term_expansion(IN,OUT):- not(user:prolog_mud_disable_term_expansions),
  dbase_module_ready, must_compile_special_clause(IN),
  in_file_expansion, 
  loader_term_expansion(IN,WHY),must(OUT = user:WHY).

% :- sanity(test_expand_units(tCol(_A))).

% :- sanity(test_expand_units(number(_A))).

:- sanity((writeq(tCol(_A)),nl)).


:- decl_type(vtTestType).

:- must(must_compile_special_clause(vtTestType(vTest1))).

vtTestType(vTest1).
vtTestType(vTest2).

:-must(not(user:mpred_prop(dbase_t,prologHybrid))).
% :-decl_mpred_hybrid(function_corisponding_predicate(tFunction,tPred)).

:- sanity(tCol(tCol)).

:- must(agenda_rescan_for_module_ready).

:- must(must_compile_special_clause(tCol(tCol))).

:- must(must_compile_special_clause(isa(_,_))).
:- must(must_compile_special_clause(not(_))).

:- show_call(source_location(_,_)).

:-must(in_file_expansion;in_file_directive).
% :- must(show_call(ensure_plmoo_loaded(logicmoo(dbase/dbase_i_builtin)))).

/*
:- pfcAdd(((isa(Compound,prologMacroHead)/compound_functor(Compound,F)) => functorDeclares(F))).
(ttFormatType(FT)/is_declarations(FT))=>ftFormatting(FT).

action_info(ArgTypes)/is_declarations(ArgTypes) =>vtActionTemplate(ArgTypes).
action_info(ArgTypes,_)/is_declarations(ArgTypes) =>vtActionTemplate(ArgTypes).

ftFormatting(ArgTypes)/is_declarations(ArgTypes) => metaFormatting(ArgTypes).
predArgTypes(ArgTypes)/is_declarations(ArgTypes) => metaFormatting(ArgTypes).
vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) => metaFormatting(ArgTypes).

*/

:- must(show_call(with_assertions(thlocal:pfcExpansion,with_assertions(thlocal:consulting_sources,ensure_loaded(logicmoo(dbase/dbase_i_builtin)))))).

% :- if_startup_script(with_assertions(thlocal:pfcExpansion,ensure_loaded(dbase_i_mpred_pfc_testing))).

% :-asserta(user:isa_pred_now_locked).

% :-asserta(user:prolog_mud_disable_term_expansions).

term_expansion(I,O):- thlocal:consulting_sources, with_no_assertions(thlocal:consulting_sources,add(I)),O=true.
user:goal_expansion(ISA,G) :-compound(ISA),thlocal:is_calling,was_isa(ISA,I,C),G=no_repeats(isa(I,C)).

