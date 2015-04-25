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

:- prolog_load_context(directory,Dir),asserta(user:file_search_path(logicmoo,Dir)).
:- dynamic(user:isa_pred_now_locked/0).

:- nb_setval(pldoc_object,pldoc_object_missing).

:- include(mpred/logicmoo_i_header).



:- export(user:mpred_mod/1).
:- dynamic user:mpred_mod/1.
user:mpred_mod(user).


:-dynamic('$was_imported_kb_content$'/2).
:-multifile('$was_imported_kb_content$'/2).
% [Manditory] define how we interact with the module system
:-if(not(current_predicate(swi_module/2))).
swi_module(M,E):-dmsg(swi_module(M,E)).
:-endif.


:- export(with_no_mpred_expansions/1).
:- meta_predicate(with_no_mpred_expansions(0)).
%with_no_mpred_expansions(Goal):-
%  with_assertions(thlocal:disable_mpred_term_expansions_locally,Goal).

:-dynamic(tChannel/1).
:-dynamic(subFormat/2).

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
% Thread Locals
% ================================================
:- thread_local thlocal:consulting_sources/0.
:- thread_local thlocal:already_in_file_term_expansion/0.
:- thread_local thlocal:agent_current_action/2.
:- thread_local thlocal:caller_module/2.
:- thread_local thlocal:mpred_opcall/2.
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
:- dynamic(mpred_module_ready).

% ========================================
% user:mpred_mod/1
% ========================================


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

%expanded_already_functor('$was_imported_kb_content$').
%expanded_already_functor(was_enabled).

% expanded_already_functor(F):-user:mpred_prop(F,prologOnly).

%must_compile_special_clause(:- (_) ):-!,fail.
%must_compile_special_clause(CL):- sanity(nonvar(CL)),not(thlocal:into_form_code),not(thlocal:already_in_file_term_expansion),not((get_functor(CL,F),expanded_already_functor(F))).


%OLD user:goal_expanstion(H,_):- notice_predicate_head(H),fail.
%OLD user:term_expanstion(H:-_,_):- must_compile_special_clause(H),notice_predicate_head(H),fail.

% makeConstant(X):- trace_or_throw(makeConstant(X)).

% ================================================
% A tiny bit of TMS
% ================================================


%OLD user:decl_database_hook(change(assert,_),Fact):- resolve_if_false(Fact).

%OLD was_known_false(Fact):-is_known_false(Fact),doall(retract((is_known_false(_):-true))),dmsg(trace_or_throw(error+was_known_false(Fact))).

%OLD resolve_if_false(Fact):- ignore(((is_known_false(Fact),was_known_false(Fact)))).

user:ruleRewrite(isa(isInstFn(Sub),Super),genls(Sub,Super)):-ground(Sub:Super),!.
user:ruleRewrite(mudLabelTypeProps(Lbl,T,[]),typeHasGlyph(T,Lbl)):-nonvar(T),!.
user:ruleRewrite(mudLabelTypeProps(Lbl,T,Props),typeProps(T,[typeHasGlyph(Lbl)|Props])):-nonvar(T),!.


% ================================================
% DBASE_T System
% ================================================

:- ensure_loaded(mpred/pfc).
% ================================================================================
% DETECT PREDS THAT NEED SPECIAL STORAGE 
% ================================================================================
:-export(functorDeclaresPred/1).
functorDeclaresPred(Prop):- % vFormatted 
	arg(_,v(pred_argtypes,predIsFlag,tPred,
        prologMultiValued,prologSingleValued,prologMacroHead,prologOnly,
		prologOrdered,prologNegByFailure,prologPTTP,prologSNARK,prologHybrid,prologListValued),Prop).


% ================================================
% Capturing Assertions
% ================================================
:- thread_local((record_on_thread/2)).
:- thread_local thlocal:mpred_capture/2.
:- thread_local thlocal:mpred_change/2.
while_capturing_changes(Call,Changes):-thread_self(ID),with_assertions(thlocal:mpred_capture(ID,_),(Call,get_mpred_changes(ID,Changes),clear_mpred_changes(ID))).
clear_mpred_changes(ID):-retractall(thlocal:mpred_change(ID,_)).
get_mpred_changes(ID,Changes):-findall(C,thlocal:mpred_change(ID,C),Changes).
%OLD user:decl_database_hook(AR,C):- record_on_thread(mpred_change,changing(AR,C)).
record_on_thread(Dbase_change,O):- thread_self(ID),thlocal:mpred_capture(ID,Dbase_change),!,Z=..[Dbase_change,ID,O],assertz(Z).



% ================================================
% DBASE_T System
% ================================================

:- ensure_loaded(mpred/pfc).
:- ensure_loaded(mpred/logicmoo_i_mpred_props).
:- ensure_loaded(mpred/logicmoo_i_agenda).
:- ensure_loaded(mpred/logicmoo_i_call).
:- ensure_loaded(mpred/logicmoo_i_coroutining).
:- ensure_loaded(mpred/logicmoo_i_hooks).
:- ensure_loaded(mpred/logicmoo_i_term_expansion).
:- ensure_loaded(mpred/logicmoo_i_store).
:- ensure_loaded(mpred/logicmoo_i_mpred_stubs).
:- ensure_loaded(mpred/logicmoo_i_types).
:- ensure_loaded(mpred/logicmoo_i_loader).
:- ensure_loaded(mpred/logicmoo_i_argtypes).
:- ensure_loaded(mpred/logicmoo_i_term_expansion_pfc).



/*
mpred_numbervars_with_names(Term):- term_variables(Term,Vars),mpred_name_variables(Vars),!,numbervars(Vars,91,_,[attvar(skip),singletons(true)]),!.

mpred_name_variables([]).
mpred_name_variables([Var|Vars]):-
   (var_property(Var, name(Name)) -> Var = '$VAR'(Name) ; true),
   mpred_name_variables(Vars).
*/







user:goal_expansion(G,isa(I,C)):- \+  thlocal:disable_mpred_term_expansions_locally, G\=isa(_,_),(was_isa(G,I,C)),!.
user:term_expansion(G,isa(I,C)):- \+  thlocal:disable_mpred_term_expansions_locally, hotrace(was_isa(G,I,C)).


mpred_module_ready.
:- with_no_term_expansions(if_file_exists(user:ensure_loaded(logicmoo(dbase/mpred_i_rdf_store)))).

:- decl_mpred_hybrid(argIsa/3).
:- add_fast(<=( argIsa(F,N,Isa), argIsa_known(F,N,Isa))).

:-asserta(thlocal:pfcExpansion).
:-decl_mpred_prolog(resolveConflict/1).
:-decl_mpred_prolog(pfc_select/2).

:-must((fully_expand_goal(_,:-multifile user:create_random_fact/1,O),show_call_failure(O=(:-multifile user:create_random_fact/1)))).

:-decl_type(tPred).
:-decl_mpred_hybrid(isa/2).

system:term_expansion(A,B):- \+  thlocal:disable_mpred_term_expansions_locally,
  current_predicate(pfcExpansion_loaded/0),loop_check(pfc_file_expansion(A,B)),A\=@=B,
  nop(dmsg(pfc_file_expansion_base(IN,WHY))).

user:semweb_startup:- with_no_term_expansions(if_file_exists(user:ensure_loaded(logicmoo(dbase/mpred_i_rdf_store)))).

:- with_no_term_expansions(if_file_exists(user:ensure_loaded(logicmoo(mobs/planner/mpred_i_hyhtn)))).
:-decl_type(predIsFlag).
:-decl_type(prologOnly).
:-decl_mpred_hybrid(formatted_resultIsa/2).
:-decl_mpred_hybrid(resultIsa/2).

system:term_expansion(IN,OUT):- \+  thlocal:disable_mpred_term_expansions_locally,
  mpred_module_ready, must_compile_special_clause(IN),
  in_file_expansion, 
  loader_term_expansion(IN,WHY),
  must(OUT = user:WHY),
  dmsg(loader_term_expansion(IN,WHY)).

% :- sanity(test_expand_units(tCol(_A))).

% :- sanity(test_expand_units(number(_A))).

:- sanity((writeq(tCol(_A)),nl)).


:- decl_type(vtTestType).

:- must(must_compile_special_clause(vtTestType(vTest1))).

vtTestType(vTest1).
vtTestType(vTest2).

:-must(not(user:mpred_prop(t,prologHybrid))).
% :-decl_mpred_hybrid(function_corisponding_predicate(tFunction,tPred)).

:- sanity(tCol(tCol)).

:- must(agenda_rescan_for_module_ready).

:- must(must_compile_special_clause(tCol(tCol))).

:- must(must_compile_special_clause(isa(_,_))).
:- must(must_compile_special_clause(not(_))).

:- show_call(source_location(_,_)).

:-must(in_file_expansion;in_file_directive).

/*
:- pfc_add(((vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) => vtActionTemplate(ArgTypes)))).
:- pfc_add(((user:action_info(ArgTypes,_)/is_declarations(ArgTypes) => vtActionTemplate(ArgTypes)))).
:- pfc_add(((isa(Compound,prologMacroHead)/compound_functor(Compound,F)) => functorDeclares(F))).
(ttFormatType(FT)/is_declarations(FT))=>ttFormatted(FT).


% Functions
ttFormatted(ArgTypes)/is_declarations(ArgTypes) => metaFormatting(ArgTypes).
% Preds
pred_argtypes(ArgTypes)/is_declarations(ArgTypes) => metaFormatting(ArgTypes).
% Representations
vtActionTemplate(ArgTypes)/is_declarations(ArgTypes) => metaFormatting(ArgTypes).

*/

:- must(show_call(with_assertions(thlocal:pfcExpansion,with_assertions(thlocal:consulting_sources,ensure_loaded('mpred/logicmoo_i_builtin.pfc'))))).

% :- if_startup_script(with_assertions(thlocal:pfcExpansion,ensure_loaded(mpred_i_mpred_pfc_testing))).

% :-asserta(user:isa_pred_now_locked).


% :-loadTinyAssertions1.

%:-prolog_repl.
%:-showTinyAssertions.
%:-prolog_repl.
%:-loadTinyAssertions2.

% :- source_location(S,_),forall(source_file(H,S),( \+predicate_property(H,built_in), functor(H,F,A),module_transparent(F/A),export(F/A))).

%system:term_expansion(I,O):- \+ thlocal:disable_mpred_term_expansions_locally, thlocal:consulting_sources, with_no_assertions(thlocal:consulting_sources,add(I)),O=true.
user:goal_expansion(ISA,G) :- \+ thlocal:disable_mpred_term_expansions_locally, compound(ISA),thlocal:is_calling,was_isa(ISA,I,C),G=no_repeats(isa(I,C)).


