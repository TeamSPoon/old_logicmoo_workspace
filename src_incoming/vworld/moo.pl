/** <module> 
% This module defines the module types that we use:
% utility,planner,parser,action,database,effects,spawning_loading,connection
% It is the basic module system of the Logicmoo MUD
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- module(moo,[coerce/3, current_context_module/1,
    rescan_dbase_t_once/0,
    rescan_dbase_t/0,
    term_expansion_local/2,
         run_database_hooks/2,
         register_module_type/1,          
         end_module_type/1,
         enter_term_anglify/2,
         op(1120,fx,export),
         op(1120,fx,decl_mpred_prolog),
         op(1150,fx,decl_mpred_hybrid),
         agent_text_command/4,         
         register_timer_thread/3]).


:-export(is_stable/0).
is_stable :- true.
simple_code :- fail.
save_in_dbase_t:-true.
not_simple_code :- \+ simple_code.
type_error_checking:-false.

:- set_prolog_flag(double_quotes, atom).
:- set_prolog_flag(double_quotes, string).

:- '@'((use_module(logicmoo(logicmoo_util/logicmoo_util_bugger)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_library)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_ctx_frame)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_strings)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_terms)),
         use_module(logicmoo(logicmoo_util/logicmoo_util_dcg))),'user').

% ========================================
% decl_mpred_hybrid/1/2/3
% ========================================

:- dynamic_multifile_exported mpred_arity/2.
:- dynamic_multifile_exported mpred_prop/2.

mpred_prop(mpred_prop,prologOnly).
mpred_prop(mpred_arity,prologOnly).
mpred_prop(subft, extentKnown).
mpred_prop(ft_info, extentKnown).
mpred_arity(argsIsa,1).
mpred_arity(mpred_prop,2).
mpred_arity(mpred_arity,2).
mpred_arity(singleValued,1).
mpred_arity(term_anglify,2).
mpred_arity(equivRule,2)

:-op(0,fx,((decl_mpred_hybrid))).
:-export((decl_mpred_hybrid)/1).


scan_missing_stubs(F):-
   ignore((forall(mpred_missing_stubs(F,Stub),(mpred_arity(F,A),show_call(declare_dbase_local(F,A,Stub)))))).


decl_mpred_pi(PI):-ignore((ground(PI),compound(PI),decl_mpred(PI))).
decl_mpred_mfa(_,M:F,A):-atom(M),!,decl_mpred_mfa(M,F,A).
decl_mpred_mfa(M,FF,A):-
   get_functor(FF,F,_),
   must_det_l([
     ignore((var(M),context_module(M),dmsg(decl_mpred_mfa(M,F,A)))),
     ignore((nonvar(M),asserta_if_new(mpred_prop(F,def_module(M))))),
     assert_arity(F,A),
     declare_dbase_local(F,A),        
    '@'((
     (static_predicate(M,F,A)->true;M:dynamic(F/A)), 
     M:export(F/A),
     M:multifile(M:F/A)),M),
     scan_missing_stubs(F) ]).

decl_mpred_stubtype(F,StubType):-decl_mpred(F,stubType(StubType)),decl_mpred(F,StubType).

decl_mpred_hybrid(M):-with_pi(M,decl_mpred_hybrid).
decl_mpred_hybrid(F,A):-
     decl_mpred(F,A),
     decl_mpred_pi(F),
     decl_mpred_stubtype(F,prologHybrid),
     get_functor(F,FF,_),
     must_det((mpred_arity(FF,AR),decl_mpred_mfa(_,F,AR))).
decl_mpred_hybrid(M,PI,F/A):-
     decl_mpred(F,A),  
     decl_mpred_pi(PI),       
     decl_mpred_stubtype(F,prologHybrid),
     decl_mpred_mfa(M,F,A).


:-op(1150,fx,decl_mpred_hybrid).

% ========================================
% dbase_mod/1
% ========================================

:- export dbase_mod/1.
:- dynamic dbase_mod/1.
dbase_mod(moo).


% ========================================
% into_hilog_form/into_mpred_form
% ========================================

:-export(into_form_code/0).
:-dynamic(into_form_code/0).

:-'$hide'(expanded_different/2).
:-export(expanded_different/2).

expanded_different(G0,G1):-with_assertions(into_form_code,expanded_different_ic(G0,G1)).

expanded_different_ic(G0,G1):-G0==G1,!,fail.
expanded_different_ic(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different_ic(G0,G1):- G0\==G1.

expanded_different_1(NV:G0,G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (var(G0);var(G1)),!,trace_or_throw(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


:-export(was_isa/3).
was_isa(dbase_t(C,I),I,C).
was_isa(isa(I,C),I,C).
was_isa(dbase_t(isa,I,C),I,C).
was_isa(M:X,I,C):-atom(M),!,was_isa(M:X,I,C).
was_isa(X,I,C):-compound(X),functor(X,C,1),!,arg(1,X,I),!. 


:-export(into_hilog_form/2).
into_hilog_form(G0,G1):-with_assertions(into_form_code,into_hilog_form_ic(G0,G1)).

into_hilog_form_ic(M:X,O):- atom(M),!,into_hilog_form_ic(X,O).
into_hilog_form_ic(X,O):- X=..[F|A],into_hilog_form(X,F,A,O).

% TODO finish negations
into_hilog_form(X,_,_,dbase_t(C,I)):-was_isa(X,I,C),!.
into_hilog_form(X,F,_A,X):- mpred_prop(F,as_is(_Why)),!.
into_hilog_form(X,F,_A,X):- mpred_prop(F,prologCall),!.
into_hilog_form(X,dbase_t,_A,X).
into_hilog_form(X,mpred_arity,_A,X).
into_hilog_form(X,mpred_prop,_A,X).
into_hilog_form(X,holds_t,_A,X).
into_hilog_form(X,cholds_t,_A,X).
into_hilog_form(_X,F,A,Call):-Call=..[dbase_t,F|A].

:- meta_predicate(call_after_game_load(-)).
call_after_game_load(Code):- call_after_next(moo:not_loading_game_file,Code).

hook:into_assertable_form_trans_hook(G,Dbase):- functor_catch(G,F,A),hook:into_assertable_form_trans_hook(G,F,A,Dbase).
hook:into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,prologBuiltin),!.
hook:into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,as_is(_)),!.
hook:into_assertable_form_trans_hook(G,F,_,Dbase):-mpred_prop(F,prologHybrid),!,into_hilog_form(G,Dbase).
hook:into_assertable_form_trans_hook(G,F,_,Dbase):-mpred_prop(F,is_dbase_t),!,into_hilog_form(G,Dbase).
hook:into_assertable_form_trans_hook(G,F,_,was_asserted_gaf(G)):- mpred_prop(F,was_asserted_gaf),!.

:-dynamic_multifile_exported(into_assertable_form/2).
into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
% into_assertable_form(B,A):- save_in_dbase_t,!,into_hilog_form(B,A),!.
into_assertable_form(G0,G1):-with_assertions(into_form_code,into_assertable_form_ic(G0,G1)).

into_assertable_form_ic(H,G):- call_no_cuts((hook:into_assertable_form_trans_hook(H,G))),expanded_different(H,G),!.
into_assertable_form_ic(H,GO):-expand_term( (H :- true) , C ), reduce_clause(C,G),expanded_different(H,G),!,into_assertable_form(G,GO),!.
into_assertable_form_ic(X,O):- functor_catch(X,F,A),into_assertable_form_via_mpred(X,F,A,O),!.
into_assertable_form_ic(X,O):- into_assertable_form(dbase_t,X,O),!.

into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,prologHybrid),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,as_is(_)),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- not(mpred_prop(F,is_dbase_t)),!,X=O.

:-dynamic_multifile_exported(into_assertable_form/3).
into_assertable_form(HLDS,M:X,O):- atom(M),!,into_assertable_form(HLDS,X,O),!.
into_assertable_form(HLDS,X,O):-with_assertions(into_form_code,(( X=..[F|A],into_assertable_form(HLDS, X,F,A,O)))),!.

% TODO finish negations
into_assertable_form(Dbase_t,X,Dbase_t,_A,X):-!.
into_assertable_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,cholds_t,A,Call):-Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,HLDS,A,Call):- is_holds_true(HLDS), Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,F,A,Call):-Call=..[Dbase_t,F|A].

:-dynamic_multifile_exported(into_mpred_form/2).

into_mpred_form(M:X,O):- atom(M),!,into_mpred_form(X,O),!.
into_mpred_form(G,O):- functor(G,F,A),G=..[F,P|ARGS],!,into_mpred_form(G,F,P,A,ARGS,O),!.

% TODO confirm negations
into_mpred_form(_,':-',C,1,_,':-'(C)):-!.
into_mpred_form(H,_,_,_,_,GO):- with_assertions(into_form_code,once((expand_term( (H :- true) , C ), reduce_clause(C,G)))),expanded_different(H,G),!,into_mpred_form(G,GO),!.
into_mpred_form(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form(G,F,C,1,_,O):-predicate_property(G,builtin),!,into_mpred_form(C,OO),O=..[F,OO].
into_mpred_form(C,_,_,_,_,isa(I,T)):-was_isa(C,I,T),!.
into_mpred_form(_X,H,P,N,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form(_X,H,P,N,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[cholds_f,P|A]).
into_mpred_form(X,_,_,_,_,isa(I,C)):-was_isa(X,I,C),!.
into_mpred_form(X,_H,_P,_N,_A,X).


% ========================================
% assert/retract hooks
% ========================================
% :- include(logicmoo(dbase/dbase_rules_nnf)).


% hooks are declared as
%        hook:decl_database_hook(assert(A_or_Z),Fact):- ...
%        hook:decl_database_hook(retract(One_or_All),Fact):- ...

:- dynamic_multifile_exported hook:decl_database_hook/2.

:- export((rescan_dbase_t_once/0, rescan_dbase_t/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).


run_database_hooks(Type,M:Hook):-atom(M),!,moo:run_database_hooks(Type,Hook).
run_database_hooks(Type,HookIn):- into_mpred_form(HookIn,Hook),run_database_hooks_1(Type,Hook).

run_database_hooks_1(Type,M:Hook):-atom(M),!,moo:run_database_hooks_1(Type,Hook).
run_database_hooks_1(Type,Hook):- loop_check(run_database_hooks_2(Type,Hook),true).
run_database_hooks_2(Type,Hook):- copy_term(Hook,HCopy),doall(call_no_cuts(hook:decl_database_hook(Type,HCopy))).


add_w_hooks(Gaf):- catch(hooked_asserta(Gaf), error(existence_error(procedure, _Call), context(_, _)),add_w_hooks_fallback(Gaf,Gaf)).
add_w_hooks(Data,Gaf):- catch((hooked_asserta(Data,Gaf)), error(existence_error(procedure, _Call), context(_, _)),add_w_hooks_fallback(Data,Gaf)).

add_w_hooks_fallback(Gaf,Gaf):-asserta_if_new(Gaf),run_database_hooks(assert(z),Gaf).
add_w_hooks_fallback(Data,Gaf):-asserta_if_new(Data),asserta_if_new(Gaf),run_database_hooks_1(assert(z),Gaf).

rescan_dbase_t:-  loop_check(rescan_dbase_t_once,true).

reduce_clause((C:- _:B),C):-B==true,!.
reduce_clause((C:- B),C):-B==true,!.
reduce_clause(C,C).

rescan_dbase_t_once:- must_det(rescan_duplicated_facts),must_det(rerun_database_hooks).



rescan_duplicated_facts:- !, notrace( forall(member(M,[moo,world,hook]), forall((predicate_property(M:H,dynamic),mpred_arity(F,A),functor(H,F,A)), rescan_duplicated_facts(M,H)))).
rescan_duplicated_facts(_M,_H):-!.
rescan_duplicated_facts(M,H):-!,rescan_duplicated_facts(M,H,true).
rescan_duplicated_facts(M,H):-findall(H,(clause_safe(M:H,B),B==true),CL1), once((list_to_set(CL1,CL2),reduce_fact_heads(M,H,CL1,CL2))).
rescan_duplicated_facts(M,H,BB):-notrace(doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause_safe(H,B),B=@=BB,reduce_clause((H:-B),C)),CL1),
                                                                     list_to_set(CL1,CL2),once(reduce_fact_heads(M,H,CL1,CL2))))))).
rerun_database_hooks:-doall((gather_fact_heads(_M,H),forall(clause_safe(H,true),run_database_hooks(assert(z),H)))).



reduce_fact_heads(_M,_H,CL1,CL1):-!. % no change
reduce_fact_heads(M,H,CL1,CL2):- 
 ignore((
   predicate_property(M:H,dynamic),
   length(CL1,L1),length(CL2,L2),
   dmsg(reduce_fact_heads(M,H,from(L1,L2))),
   retractall(M:H),
   % forall(member(C,CL1),retractall(M:C)),
   forall(member(C,CL2),assertz(M:C)))).

gather_fact_heads(M,H):- (nonvar(M)->true; member(M,[dbase,moo,world,user,hook])), current_predicate(M:F/A), mpred_arity(F,A),
  once((once((A>0,atom(F),F\=(:),var(H), debugOnError(functor_catch(H,F,A)))),compound(H),predicate_property(M:H,number_of_clauses(_)),
  not((arg(_,vv(system,bugger,logicmoo_util_dcg,user),M);predicate_property(M:H,imported_from(_)))))).

:-dynamic(fskel/7).
:-dynamic_multifile_exported(mpred_prop/2).

hook:decl_database_hook(assert(_),Fact):- ignore((compound(Fact),Fact=..[F,Arg1|PROPS],argsIsaProps(F),decl_mpred(Arg1,[F|PROPS]))).
hook:decl_database_hook(assert(_),mpred_prop(F,P)):- decl_mpred(F,P).
hook:decl_database_hook(assert(_),isa(F,P)):- argsIsaProps(P),decl_mpred(F,P).
hook:decl_database_hook(assert(_),mpred_prop(F,stubType(Stub))):-mpred_arity(F,A),declare_dbase_local(F,A,Stub).

% ================================================
% hooked_assert/1 hooked_retract/1
% ================================================

ensure_predicate_reachable(M,C):-functor(C,F,A),ensure_predicate_reachable(M,C,F,A),fail.
ensure_predicate_reachable(_,_):- is_stable,!.
ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).


ensure_predicate_reachable(M,C,dbase_t,Ap1):-C=..[_,F|_RGS],A is Ap1 -1, declare_dbase_local_dynamic_really(M,F,A).

singletons_throw_or_fail(_):- is_stable,!,fail.
singletons_throw_or_fail(C):- contains_singletons(C),trace_or_throw(contains_singletons(C)).
nonground_throw_or_fail(C):- throw_if_true_else_fail(not(ground(C)),C).


into_assertable_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,was_asserted_gaf),!.
into_assertable_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,query_with_pred(was_asserted_gaf)).

/*
into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
into_assertable_form(H,G):-into_assertable_form_trans(H,G),!.
into_assertable_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).
*/
% DONT_USE into_mpred_form(was_asserted_gaf(H),G):-!,into_mpred_form(H,G).
% DONT_USE into_mpred_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).


run_database_hooks_local(Type,C):- once((run_database_hooks(Type,C))),ignore((into_assertable_form(C,G),differnt_assert(C,G),run_database_hooks(Type,G))).

% only place ever should actual game dbase be changed from

:-export(into_mpred_aform/3).
into_mpred_aform(C,CP,CA):-into_mpred_form(C,CP),into_assertable_form(C,CA),!.

:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).
hooked_asserta(C):- into_mpred_aform(C,CP,CA),hooked_asserta(CP,CA).
hooked_assertz(C):- into_mpred_aform(C,CP,CA),hooked_assertz(CP,CA).
hooked_retract(C):- into_mpred_aform(C,CP,CA),hooked_retract(CP,CA).
hooked_retractall(C):- into_mpred_aform(C,CP,CA),hooked_retractall(CP,CA).

hooked_asserta(CP,_CA):- singletons_throw_or_fail(hooked_asserta(CP)).
hooked_asserta(_CP,CA):- singletons_throw_or_fail(hooked_asserta(CA)).
hooked_asserta(_CP,CA):- clause_asserted(CA),!.
hooked_asserta(CP,CA):- asserta_cloc(CA),run_database_hooks_local(assert(a),CP).


hooked_assertz(CP,_CA):- singletons_throw_or_fail(hooked_assertz(CP)).
hooked_assertz(_CP,CA):- singletons_throw_or_fail(hooked_assertz(CA)).
hooked_assertz(_CP,CA):- clause_asserted(CA),!.
hooked_assertz(CP,CA):- assertz_cloc(CA),run_database_hooks_local(assert(z),CP).

hooked_retract(CP,_CA):- nonground_throw_or_fail(hooked_retract(CP)).
hooked_retract(CP,CA):- must_det(clause_asserted(CA)),!,run_database_hooks_local(retract(one),CP), ignore(retract_cloc(CA)).
hooked_retract(CP,CA):- run_database_hooks_local(retract(one),CP), ignore(retract_cloc(CA)).

hooked_retractall(CP,CA):- copy_term(CA,RT),once(retract_cloc(RT)),!,retractall_cloc(CA), run_database_hooks_local(retract(all),CP).
hooked_retractall(CP,CA):- retractall_cloc(CA),run_database_hooks_local(retract(all),CP).

differnt_assert(G1,G2):- notrace(differnt_assert1(G1,G2)),dmsg(differnt_assert(G1,G2)),ztrace.

differnt_assert1(M:G1,G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,M:G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G1,M1)),G1\=M1,!, differnt_assert1(M1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G2,M2)),G2\=M2,!, differnt_assert1(G1,M2).
differnt_assert1(G1,G2):- not((G1 =@= G2)).

show_cgoal(G):- stack_check(600,dmsg(warning(maybe_overflow(stack_lvl)))),!,call(G).
show_cgoal(G):- % dmsg(show_cgoal(G)),
               call(G).


% only place ever should actual game database be changed from
asserta_cloc(M:C):-atom(M),!,asserta_cloc(M,C),!.
asserta_cloc( C ):-dbase_mod(M),asserta_cloc(M,C),!.
asserta_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
asserta_cloc(_M,C):-singletons_throw_or_fail(C).
asserta_cloc(M,C):-clause_asserted(M:C,true),!.
asserta_cloc(M,C):-database_real(asserta,M:C).


assertz_cloc(M:C):-atom(M),!,assertz_cloc(M,C),!.
assertz_cloc( C ):-dbase_mod(M),assertz_cloc(M,C),!.
assertz_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
assertz_cloc(_M,C):-singletons_throw_or_fail(C).
assertz_cloc(M,C):-clause_asserted(M:C,true),!.
assertz_cloc(M,C):-database_real(assertz,M:C).

retract_cloc(M:C):-atom(M),!,retract_cloc(M,C),!.
retract_cloc( C ):-dbase_mod(M),retract_cloc(M,C),!.
retract_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
retract_cloc(M,C):-clause_asserted(M:C,true),!.
retract_cloc(M,C):-database_real(retract,M:C).

retractall_cloc(M:C):-atom(M),!,retractall_cloc(M,C),!.
retractall_cloc( C ):-dbase_mod(M),retractall_cloc(M,C),!.
retractall_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
retractall_cloc(M,C):-clause_asserted(M:C,true),!.
retractall_cloc(M,C):-database_real(retractall,M:C).

database_real(P,C):- debugOnError(call(P,C)).

% ========================================
% mpred_props database
% ========================================
get_body_functor(Var,_,call):-var(Var),!.
get_body_functor((M:BODY),BodyFunctor,A):-atom(M),!,get_body_functor(BODY,BodyFunctor,A).
get_body_functor((!,BODY),BodyFunctor,A):-!,get_body_functor(BODY,BodyFunctor,A).
get_body_functor(call(BODY),BodyFunctor,A):-!,get_body_functor(BODY,BodyFunctor,A).
get_body_functor(once(BODY),BodyFunctor,A):-!,get_body_functor(BODY,BodyFunctor,A).
get_body_functor((BODY1;BODY2),BodyFunctor,A):-!, (get_body_functor(BODY1,BodyFunctor,A);get_body_functor(BODY2,BodyFunctor,A)).
get_body_functor((BODY1,BODY2),BodyFunctor,A):-!, (get_body_functor(BODY1,BodyFunctor,A);get_body_functor(BODY2,BodyFunctor,A)).
get_body_functor(BODY,BodyFunctor,A):-functor(BODY,BodyFunctor,A).

ensure_clause(HEAD,_,_,_):-functor_safe(HEAD,F,_),mpred_prop(F,prologOnly),!,trace_or_throw(mpred_prop(F,prologOnly)).
ensure_clause(HEAD,_,_,BODY):- clause_asserted(HEAD , BODY),!.
% ensure_clause(HEAD,F,A,_):-pred_as_is(F,A), !.
ensure_clause(HEAD,F,_A,BODY):- assertz((HEAD:-BODY)),
   get_body_functor(BODY,BodyFunctor,_),
   add_w_hooks(mpred_prop(F,prologHybrid)),
   add_w_hooks(mpred_prop(F,hasStub(BodyFunctor))),
   % this is just to catch asserts at these predicates that are supposed to be contained.. We dont really want them compiled
   nop(((compile_predicates([HEAD])),must_det(static_predicate(HEAD)))).


:-export(argsIsaProps/1).
argsIsaProps(Prop):- 
	arg(_,v(argsIsa,multiValued,singleValued,assertionMacroHead,prologBuiltin,nonGroundOK,prologOnly,
		negationByFailure,formatted,prologHybrid,mpred,listValued),Prop).

mpred_arity(Prop,1):-argsIsaProps(Prop).
:-dynamic_multifile_exported(dbase_t/2).
% dbase_t(type,Prop):-mpred_arity(Prop,1).

:-forall(argsIsaProps(F),dynamic(F/1)).

% pass 2
declare_dbase_local(F):- not(mpred_arity(F,A)),trace_or_throw(not(mpred_arity(F,A))).
declare_dbase_local(F):- must_det(mpred_arity(F,A)),declare_dbase_local(F,A).

declare_dbase_local(F,A):- assert_arity(F,A),fail.
declare_dbase_local(F,_):- mpred_prop(F,prologOnly),!.
declare_dbase_local(F,A):- forall(mpred_prop(F,stubType(Stub)),declare_dbase_local(F,A,Stub)).
%declare_dbase_local(F,A):- declare_dbase_local(F,A,prologHybrid),!.

declare_dbase_local(F,A,_Stub):- assert_arity(F,A),fail.
declare_dbase_local(F,_,_):- mpred_prop(F,prologOnly),!. % retractall(mpred_prop(F,stubType(_Stub))).
declare_dbase_local(F,_,Stub):- mpred_prop(F,hasStub(Stub)),!.
declare_dbase_local(F,_,Stub):- add_w_hooks(mpred_prop(F,stubType(Stub))),fail.
declare_dbase_local(F,_,Stub):- add_w_hooks(mpred_prop(F,hasStub(Stub))),fail.
declare_dbase_local(F,A,prologHybrid):- must_det(declare_dbase_local_dynamic(F,A)),!.
declare_dbase_local(F,A,Stub):- trace_or_throw(unknown_stubtype_declare_dbase_local(F,A,Stub)).

mpred_missing_stubs(F,Stub):-mpred_arity(F,_),mpred_prop(F,stubType(Stub)),not(mpred_prop(F,hasStub(Stub))).


cannot_override(F,A,prologBuiltin(F,A)):-mpred_prop(F,prologBuiltin).
cannot_override(F,A,prologOnly(F,A)):-mpred_prop(F,prologOnly).
cannot_override(F,A,Why):-functor_safe(P,F,A),cannot_override(P,F,A,Why).

cannot_override(P,F,A,static_predicate(P)):-static_predicate(moo,F,A).
cannot_override(P,_,_,predicate_property(P,foreign)):-predicate_property(P,foreign),!.
cannot_override(P,_,_,predicate_property(P,builtin)):-predicate_property(P,builtin),!.
cannot_override(P,_,_,predicate_property(P,imported_from(system))):-predicate_property(P,imported_from(system)).


:-export(declare_dbase_local_dynamic/1).
declare_dbase_local_dynamic(F):- must_det(mpred_arity(F,A)),declare_dbase_local(F,A,prologHybrid).
:-export(declare_dbase_local_dynamic/2).
declare_dbase_local_dynamic(F,A):- dbase_mod(M), M:declare_dbase_local_dynamic(M,F,A).

:-export(declare_dbase_local_dynamic/3).
declare_dbase_local_dynamic(M,F,0):- trace_or_throw(illegal_argument_declare_dbase_local_dynamic(M,F,0)).
declare_dbase_local_dynamic(M,F,A):- cannot_override(F,A,Why),!,dmsg(cannot_override(F,A,Why)),nop(listing(M:F/A)).
declare_dbase_local_dynamic(M,F,A):- declare_dbase_local_dynamic_really(M,F,A).

declare_dbase_local_dynamic_really(M,F,A):- functor(HEAD,F,A),'@'(clause_safe(HEAD,(hook:body_req(F,A,HEAD,_))),M),!.
declare_dbase_local_dynamic_really(M,F,A):- mpred_prop(F,prologOnly),!,trace_or_throw(declare_dbase_local_dynamic_really(M,F,A)).
declare_dbase_local_dynamic_really(M,F,A):-
   functor_catch(HEAD,F,A),
   HEAD=..[F|ARGS],
   HEAD_T=..[dbase_t,F|ARGS],
   '@'(ensure_clause(HEAD,F,A,(hook:body_req(F,A,HEAD,HEAD_T))),M),!,
   dynamic_multifile_exported(M:F/A),
   nop(compile_predicates([HEAD])).

declare_dbase_local_dynamic_plus_minus_2(F,AMinus2):-   
   decl_mpred(F,arity(AMinus2)),
   declare_dbase_local(F,AMinus2).
   
declare_dbase_local_dynamic_plus_2(F,A2):- once(( AMinus2 is A2 -2, declare_dbase_local_dynamic_plus_minus_2(F,AMinus2))),fail.

declare_dbase_local_dynamic_plus_2(F,A2):- cannot_override(F,A2,Why),!,dmsg(cannot_override_plus_2(F,A2,Why)).
declare_dbase_local_dynamic_plus_2(F,A2):- 
   functor(HEAD,F,A2),
   HEAD=..[F|ARGS],
   append(ARGSMinus2,[_,_],ARGS),
   HEADMinus2=..[F|ARGSMinus2],
   AMinus2 is A2 -2,
   ensure_clause(HEAD,F,AMinus2,HEADMinus2),!,
  % compile_predicates([HEAD]),
   dbase_mod(M),
   decl_mpred_hybrid(M,F,A2).

   



user_export(_):- dbase_mod(user),!.
user_export(Prop/Arity):- 
   dbase_mod(M), '@'( M:export(Prop/Arity) , M).



% ============================================
% DBASE to Cyc Predicate Mapping
% ============================================

mpred_arity('abbreviationString-PN', 2).

make_functorskel(F,_):- fskel(F,_,_,_,_,_,_),!.
make_functorskel(F,N):- mpred_arity(F,N),make_functorskel(F,N,SKEL),asserta(SKEL),!.
make_functorskel(F,N):- ignore(mpred_arity(F,A)),dmsg(trace_or_throw(illegal_make_functorskel(F,N,A))).

dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-compound(PRED),functor_catch(PRED,F,N),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-compound(DBASE),!,arg(1,DBASE,F),must_det(mpred_arity(F,N)),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.
dbase2pred2svo(DBASE,PRED,svo(A,F,RGS)):-nonvar(F),must(mpred_arity(F,N)),make_functorskel(F,N),!,fskel(F,DBASE,PRED,A,RGS,_,_),!.

typical_mtvars([_,_]).

% arity 1 person
make_functorskel(Person,1,fskel(Person,dbase_t(Person,A),Call,A,[],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[Person,A],Call2=..[Person,A|MtVars]. 
% arity 2 likes
make_functorskel(Likes,2,fskel(Likes,dbase_t(Likes,A,B),Call,A,B,MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Likes,A,B],Call2=..[Likes,A,B|MtVars]. 
% arity 3 between
make_functorskel(Between,3,fskel(Between,dbase_t(Between,A,B,C),Call,A,[B,C],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Between,A,B,C],Call2=..[Between,A,B,C|MtVars]. 
% arity 4 xyz
make_functorskel(Xyz,4,fskel(Xyz,dbase_t(Xyz,I,X,Y,Z),Call,I,[X,Y,Z],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Xyz,I,X,Y,Z],Call2=..[Xyz,I,X,Y,Z|MtVars]. 
% arity 5 rxyz
make_functorskel(RXyz,5,fskel(RXyz,dbase_t(RXyz,I,R,X,Y,Z),Call,I,[R,X,Y,Z],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[RXyz,I,R,X,Y,Z],Call2=..[RXyz,I,R,X,Y,Z|MtVars]. 
% arity >6 
make_functorskel(F,N,fskel(F,DBASE,Call,I,NList,MtVars,Call2)):-typical_mtvars(MtVars),functor_catch(Call,F,N),Call=..[F,I|NList],DBASE=..[dbase_t,F,I|NList],append([F,I|NList],MtVars,CALL2List),Call2=..CALL2List.

% ============================================
% Prolog to Cyc Predicate Mapping
%
%  the following will all do the same things:
%
% :- decl_mpred('BaseKB':isa/2). 
% :- decl_mpred('BaseKB':isa(_,_)). 
% :- decl_mpred(isa(_,_),'BaseKB'). 
% :- decl_mpred('BaseKB',isa,2). 
%
%  Will make calls 
% :- isa(X,Y)
%  Query into #$BaseKB for (#$isa ?X ?Y) 
%
% decl_mpred/N
%
% ============================================
:-export(registerCycPredPlus2/1).

registerCycPredPlus2_3(_M,_PI,F/A2):-   
  A is A2 - 2, decl_mpred_hybrid(F/A),decl_mpred(F,cycPlus2(A2)),decl_mpred(F,cycPred(A)).


registerCycPredPlus2(P):-!,with_pi(P,registerCycPredPlus2_3).

:-dynamic_multifile_exported((loading_module_h/1, loading_game_file/2, loaded_game_file/2)).

:-export(not_loading_game_file/0).
not_loading_game_file:- not(moo:loading_game_file(_,_)),moo:loaded_game_file(_,_),!.

get_mpred_prop(F,P):-mpred_prop(F,P),!.
get_mpred_prop(F,_A,P):-mpred_prop(F,P).

assert_arity(F,A):-not(atom(F)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-not(integer(A)),trace_or_throw(assert_arity(F,A)).
assert_arity(F,A):-mpred_arity(F,A),assert_if_new(mpred_prop(F,arity(A))),!.
assert_arity(F,A):-mpred_arity(F,1),dmsg(trace_or_throw(was_one_assert_arity(F,A))),!.
assert_arity(argsIsa,2):-trace_or_throw(assert_arity_argsIsa(argsIsa,2)).
assert_arity(ArgsIsa,0):-trace_or_throw(assert_arity(ArgsIsa,0)).
assert_arity(F,A):-loop_check(assert_arity_lc(F,A),true).

assert_arity_lc(F,A):-
  retractall(mpred_prop(F,arity(_))),
  retractall(mpred_arity(F,_)),
   must_det(atom(F)),
    add_w_hooks(mpred_arity(F,A)),add_w_hooks(mpred_prop(F,arity(A))),assert_if_new(mpred_arity(F,A)),must_det(make_functorskel(F,A)),!.
   % ignore((forall(mpred_missing_stubs(F,Stub),show_call(declare_dbase_local(F,A,Stub))))).
   


:-export(rescan_missing_stubs/0).
rescan_missing_stubs:-loop_check(rescan_missing_stubs_lc,true).
rescan_missing_stubs_lc:-notrace(ignore((forall(mpred_missing_stubs(F,Stub),(mpred_arity(F,A),show_call(declare_dbase_local(F,A,Stub))))))).

first_mpred_props(arity(_)).
first_mpred_props(argsIsa(_)).

mpred_prop_ordered(Pred,Prop):-first_mpred_props(Prop),mpred_prop(Pred,Prop),not(mpred_prop(Pred,prologOnly)).
mpred_prop_ordered(Pred,Prop):-mpred_prop(Pred,Prop),not(first_mpred_props(Prop)),not(mpred_prop(Pred,prologOnly)).

:-export(rescan_mpred_props/0).

rescan_mpred_props:- loop_check(rescan_mpred_props_lc,true).
rescan_mpred_props_lc:-rescan_duplicated_facts(moo,mpred_prop(_,_)),fail.
rescan_mpred_props_lc:-forall(mpred_prop_ordered(Pred,Prop),add_w_hooks(mpred_prop(Pred,Prop))),fail.
rescan_mpred_props_lc:-rescan_missing_stubs.
rescan_mpred_props_lc.



:- dynamic_multifile_exported((decl_mpred/1)).

decl_mpred((A,B)):-decl_mpred(A),decl_mpred(B).
decl_mpred(M):-loop_check(with_pi(M,decl_mpred_1),true).
decl_mpred_1(_,F,F/0):-!,assert_if_new(dbase_t(mpred,F)).
decl_mpred_1(M,PI,F/A):-
   decl_mpred(F,A),
   ignore((ground(PI),decl_mpred(PI))),
   decl_mpred(F,[ask_module(M)]).

:-dynamic_multifile_exported(decl_mpred/2).
decl_mpred(C,More):- ignore(loop_check(decl_mpred_0(C,More),true)).

decl_mpred_0(C,More):- (var(C);var(More)), trace_or_throw(var_decl_mpred(C,More)).
decl_mpred_0(F,mpred):-!,assert_if_new(dbase_t(mpred,F)).
decl_mpred_0(_,[]):-!.
decl_mpred_0(M:FA,More):-atom(M),!,decl_mpred_0(FA,[ask_module(M)|More]).
decl_mpred_0(F/A,More):-atom(F),!,decl_mpred_1(F,arity(A)),decl_mpred(F,More),!.
decl_mpred_0(C,More):-compound(C),C=..[F,Arg1|PROPS],argsIsaProps(F),!,ground(Arg1),decl_mpred(Arg1,[F,PROPS,More]).
decl_mpred_0(C,More):-compound(C),!,functor_catch(C,F,A),decl_mpred_1(F,arity(A)),decl_mpred_0(F,More),!,ignore((ground(C),decl_mpred(F,argsIsa(C)))),!.
decl_mpred_0(F,A):-number(A),!,decl_mpred_1(F,arity(A)),!.
decl_mpred_0(F,[Prop|Types]):-!,decl_mpred_0(F,Prop),!,decl_mpred_0(F,Types),!.

decl_mpred_0(F,T):-doall(( decl_mpred_1(F,T) )).

decl_mpred_1(F,argsIsa(FARGS)):- functor(FARGS,_,A),decl_mpred(F,A),fail.
decl_mpred_1(_,argsIsa(FARGS)):- functor(FARGS,_,A),arg(A,FARGS,Arg),var(Arg),!.
decl_mpred_1(F,arity(A)):- assert_arity(F,A),fail.

decl_mpred_1(F,prologHybrid):- declare_dbase_local_dynamic(F).
decl_mpred_1(F,cycPlus2(A)):-declare_dbase_local_dynamic_plus_2(F,A).

decl_mpred_1(F,Prop):-mpred_prop(F,Prop),!.
decl_mpred_1(F,Prop):-add_w_hooks(mpred_prop(F,Prop)),fail.

decl_mpred_1(F,A):-once(decl_mpred_2(F,A)).

decl_mpred_2(F,external(Module)):- dmsg(decl_mpred(F,external(Module))),not(dbase_mod(Module)),must_det(mpred_arity(F,A)),functor_catch(HEAD,F,A),must_det(predicate_property(Module:HEAD,_)),!.
decl_mpred_2(F,_):- once((not((mpred_prop(F,external(Module)),not(dbase_mod(Module)))),declare_dbase_local(F))),!.
% decl_mpred_2(F,A):- declare_dbase_local_dynamic(F,A).


decl_mpred(Mt,F,A):-decl_mpred(F,A),ignore((nonvar(Mt),decl_mpred(F,mt(Mt)))).

:-op(0,fx,decl_mpred_prolog).
decl_mpred_prolog(P):- with_pi(P,decl_mpred_prolog).
:-op(1120,fx,decl_mpred_prolog).

decl_mpred_prolog(M,PI,F/A):-
  must_det_l([   
   % retractall(mpred_prop(F,_)),
   assert_arity(F,A),
   decl_mpred(F,prologOnly),   
   decl_mpred(F,prologBuiltin),
   ignore((ground(PI),decl_mpred(PI))),
   decl_mpred(F,as_is(M:F/A)),
   decl_mpred(F,ask_module(M)),
   decl_mpred(F,A)]).


% :- decl_mpred((nameStrings/2,grid_key/1,on_world_load/0,label_type/2,creatableType/2)).
% :- decl_mpred posture/1.
% :- dynamic_multifile_exported((decl_mpred/1)).

:-dmsg_hide(game_assert).
:-dmsg_hide(db_op_exact).

functor_check_univ(M:G1,F,List):-atom(M),member(M,[dbase,moo]),!,functor_check_univ(G1,F,List),!.
functor_check_univ(G1,F,List):-must_det(compound(G1)),must_det(G1 \= _:_),must_det(G1 \= _/_),G1=..[F|List],!.

:-export(glean_pred_props_maybe/1).
glean_pred_props_maybe(_:G):-!,compound(G),glean_pred_props_maybe(G).
glean_pred_props_maybe(G):-compound(G),G=..[F,Arg1|RGS],argsIsaProps(F),!,add_mpred_prop_gleaned(Arg1,[F|RGS]),!.

add_mpred_prop_gleaned(M:Arg1,FRGS):-atom(M),!,add_mpred_prop_gleaned(Arg1,FRGS).
add_mpred_prop_gleaned(Arg1,FRGS):-functor_check_univ(Arg1,F,ARGSISA),add_mpred_prop_gleaned_4(Arg1,F,ARGSISA,FRGS).
add_mpred_prop_gleaned_4(Arg1,_F,[ARG|_],FRGS):-nonvar(ARG),!,decl_mpred(Arg1,[argsIsa(Arg1)|FRGS]).
add_mpred_prop_gleaned_4(Arg1,_F,_,FRGS):-decl_mpred(Arg1,FRGS).

user:term_expansion(G,_):- notrace((once(glean_pred_props_maybe(G)),fail)).

% ========================================
% is_holds_true/is_holds_false
% ========================================


:- dbase_mod(M),dynamic_multifile_exported((
          M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7)).

:-export(is_svo_functor/1).
is_svo_functor(Prop):- notrace((atom(Prop),arg(_,svo(svo,prop,valueOf,rdf),Prop))).

:-export(hilog_functor/1).
hilog_functor(dbase_t).

:-export(is_holds_true_not_hilog/1).
is_holds_true_not_hilog(HOLDS):-is_holds_true(HOLDS),\+ hilog_functor(HOLDS).

:-export(is_holds_true/1).
is_holds_true(Prop):- notrace((atom(Prop),is_holds_true0(Prop))),!.

% k,p,..
is_holds_true0(Prop):-arg(_,vvv(holds,holds_t,dbase_t,asserted_dbase_t,assertion_t,assertion,secondOrder,firstOrder),Prop).

:-export(is_2nd_order_holds/1).
is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

:-export(is_holds_false/1).
% is_holds_false(Prop):-notrace((atom(Prop),once((is_holds_false0(Prop,Stem),is_holds_true0(Stem))))).
is_holds_false(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,aint,assertion_f,asserted_dbase_f,retraction,not_secondOrder,not_firstOrder]).

is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).

:- dynamic_multifile_exported((decl_coerce)/3).

:- decl_mpred_hybrid(singleValued/1).

% ========================================
% decl_mpred_hybrid database
% ========================================

:- discontiguous(singleValued/1).

:- dynamic_multifile_exported(moo:ft_info/2).
:- decl_mpred_hybrid(moo:subft/2).

:- dynamic_multifile_exported action_info/2.
:- dynamic_multifile_exported type_action_info/3.
:- dynamic_multifile_exported mud_test/2.
:- dynamic_multifile_exported agent_call_command/2.

:- dynamic_multifile_exported action_rules/4.
:- dynamic_multifile_exported agent_text_command/4.
:- dynamic_multifile_exported check_permanence/4.
:- dynamic_multifile_exported call_after_load/1.
:- dynamic_multifile_exported decl_mud_test/2.

:- dynamic_multifile_exported default_type_props/3.
:- dynamic_multifile_exported label_type_props/3.
:- dynamic_multifile_exported label_type/2.

:- decl_mpred_hybrid subclass/2.
:- dynamic_multifile_exported term_specifier_text/2.
:- decl_mpred_hybrid type_grid/3.
:- dynamic_multifile_exported update_charge/2.
:- dynamic_multifile_exported update_stats/2.
:- dynamic_multifile_exported use_usable/4.
:- decl_mpred_hybrid last_command/2.
:- decl_mpred_hybrid str/2. 
:- dynamic_multifile_exported verb_alias/2.
:- decl_mpred_hybrid named/2, spd/2.
:- dynamic_multifile_exported world_agent_plan/3.
:- dynamic_multifile_exported((actiontype/1,action_info/2,action_rules/4,type_action_info/3,term_specifier_text/2,action_verb_useable/4)).
:- dynamic_multifile_exported((term_anglify/2,term_anglify_last/2, term_anglify_np/3,term_anglify_np_last/3)).
:- dynamic_multifile_exported((update_charge/2,update_stats/2)).

:- module_transparent register_module_type/1.

:-dynamic_multifile_exported(thlocal:session_agent/2).
:-thread_local thlocal:dbase_change/2.
:-thread_local thlocal:dbase_opcall/2.
:-thread_local thlocal:repl_to_string/2.
:-thread_local thlocal:repl_writer/2.
:-thread_local thlocal:session_agent/2.

:-dynamic_multifile_exported  moo:capturing_changes/2.
:-thread_local moo:capturing_changes/2.

:- dynamic_multifile_exported now_unused/1.

current_context_module(Ctx):-loading_module_h(Ctx),!.
current_context_module(Ctx):-context_module(Ctx).

% ========================================
% begin/end_transform_moo_preds
% ========================================

:-thread_local is_compiling_clause/0.
is_compiling:-is_compiling_clause;compiling.
:-thread_local ended_transform_moo_preds/0, always_expand_on_thread/1, prevent_transform_moo_preds/0, may_moo_term_expand/1, always_transform_heads/0.
:-module_transparent begin_transform_moo_preds/0, end_transform_moo_preds/0.
:-export(((begin_transform_moo_preds/0,end_transform_moo_preds/0))).
begin_transform_moo_preds:- retractall(ended_transform_moo_preds),context_module(CM),asserta(may_moo_term_expand(CM)).
end_transform_moo_preds:- retractall(ended_transform_moo_preds),asserta(ended_transform_moo_preds).


% ========================================
% register_module_type/end_module_type
% ========================================
:- dynamic_multifile_exported registered_module_type/2.

register_module_type(Type):-current_context_module(CM),register_module_type(CM,Type).
register_module_type(CM,Types):-is_list(Types),!,forall(member(T,Types),register_module_type(CM,T)).
register_module_type(CM,Type):-asserta_new(registered_module_type(CM,Type)).

:-export(end_module_type/2).
end_module_type(Type):-current_context_module(CM),end_module_type(CM,Type).
end_module_type(CM,Type):-retractall(registered_module_type(CM,Type)).

% ============================================
% define_type/1
% ============================================
:- decl_mpred_hybrid type/1.
:- dynamic_multifile_exported never_type/1.
:- dynamic_multifile_exported define_type/1.

type(dir).
type(type).
type(mpred).
type(fpred).
type(relation).
type(creatableType).
type(argsIsa).
type(ArgsIsa):-argsIsaProps(ArgsIsa).
type(formattype).
type(actiontype).
type(region).
type(container).
dbase_t(formattype,string).
dbase_t(type,container).

mpred_prop(dbase_t,prologOnly).
mpred_prop(mpred_prop,prologOnly).



:- export(impliedSubClass/2).
impliedSubClass(T,ST):-ground(T:ST),is_known_false(subclass(T,ST)),!,fail.
impliedSubClass(T,ST):-predicate_property(transitive_subclass(T,ST),_),!,call_tabled(transitive_subclass(T,ST)).

:- export(asserted_subclass/2).
asserted_subclass(T,ST):-is_asserted(subclass(T,ST)).

:- export((transitive_subclass/2)).
transitive_subclass(A,T):-fact_loop_checked(subclass(A,T),transitive_subclass0(A,T)).

transitive_subclass0(FT,Sub):-asserted_subclass(FT,Sub).
transitive_subclass0(FT,Sub):-asserted_subclass(FT,A),asserted_subclass(A,Sub).
transitive_subclass0(FT,Sub):-asserted_subclass(FT,A),asserted_subclass(A,B),asserted_subclass(B,Sub).
transitive_subclass0(FT,Sub):-asserted_subclass(FT,A),asserted_subclass(A,B),asserted_subclass(B,C),asserted_subclass(C,Sub).
% transitive_subclass0(A,T):- dmsg(failed(transitive_subclass(A,T))),!,fail.


asserted_or_trans_subclass(A,A).
asserted_or_trans_subclass(A,B):-transitive_subclass(A,B).

:-dynamic_multifile_exported(not_mud_isa/2).
not_mud_isa(agent,formattype).
not_mud_isa(item,formattype).
not_mud_isa(type,formattype).
not_mud_isa(obj, extentKnown).
not_mud_isa(obj, creatableType).
not_mud_isa(obj, formattype).
not_mud_isa(formattype,formattype).
not_mud_isa(subft,type).
not_mud_isa('TemporallyExistingThing', 'TemporallyExistingThing').
not_mud_isa(createableType,'TemporallyExistingThing').
not_mud_isa(X,type):-never_type(X).


:-dynamic_multifile_exported(type_isa/2).

type_isa(Type,creatableType):-arg(_,vv(agent,item,obj,region),Type),!.
type_isa(ArgIsa,mpredtype):-argsIsaProps(ArgIsa),!.
type_isa(Type,valuetype):-arg(_,vv(type,formattype,itemtype,valuetype,ppred,fpred),Type),!.
type_isa(string,formattype):-!.
type_isa(Type,valuetype):-arg(_,vv(concept,channel,place,dir,region),Type),!.
type_isa(Type,formattype):-formattype(Type),!. % text
%type_isa(_,valuetype).

:-dynamic_multifile_exported(is_known_true/1).

is_known_true(C):-has_free_args(C),!,fail.
is_known_true(isa(container,extentKnown)).
is_known_true(isa(extentKnown,extentKnown)).
is_known_true(isa(type,extentKnown)).
is_known_true(isa(gossup,channel)).
is_known_true(subclass(region,channel)).
is_known_true(subclass(agent,channel)).
is_known_true(isa(agent,creatableType)).
is_known_true(isa(region,creatableType)).
is_known_true(isa(_,id)).
is_known_true(isa(_,term)).
is_known_true(isa(singleValued, extentKnown)).
is_known_true(isa(creatableType,extentKnown)).
is_known_true(isa(formattype,extentKnown)).
is_known_true(isa(int,nonCreatableType)).
is_known_true(isa(type,type)).
is_known_true(isa(singleValued, type)).
is_known_true(isa(extentKnown, type)).
is_known_true(subclass(extentKnown, extentDecidable)).
is_known_true(subclass(singleValued, extentDecidable)).
is_known_true(subclass('MaleAnimal',agent)).
is_known_true(subclass(X,X)).
is_known_true(subclass(formattype,type)).
is_known_true(isa(type,nonCreatableType)).
is_known_true(isa(item,creatableType)).
is_known_true(subclass(item,creatableType)).
is_known_true(isa(formattype,nonCreatableType)).
is_known_true(subclass(formattype,nonCreatableType)).
is_known_true(isa('TemporallyExistingThing', 'creatableType')).
is_known_true(isa(term,nonCreatableType)).
is_known_true(subclass(argsIsa,relation)).
is_known_true(subclass(fpred,relation)).
is_known_true(subclass(F,mpred)):-argsIsaProps(F).
is_known_true(subclass(F,fpred)):-argsIsaProps(F).
is_known_true(subclass(F,relation)):-argsIsaProps(F).

has_free_args(C):-not(compound(C));not(not(arg(_,C,var))).

:-dynamic_multifile_exported(is_known_false/1).
% :-dynamic(is_known_false/1).
is_known_false(C):-has_free_args(C),!,fail.
is_known_false(isa(extentKnown,creatableType)).
is_known_false(isa(X,Y)):-not_mud_isa(X,Y).
is_known_false(Fact):-is_known_true(Fact),!,fail.
is_known_false(subclass(spatialthing,'MaleAnimal')).
is_known_false(subclass(Type,_)):-arg(_,vv(type,relation,spatialthing,formattype),Type).

is_known_false(subclass(A,B)):-disjointWith(A,B).
is_known_false(subclass(B,A)):-disjointWith(A,B).

disjointWith(agent,item).
disjointWith(region,obj).
disjointWith(formattype,item).
disjointWith(formattype,obj).
disjointWith(formattype,region).
disjointWith(createableType,nonCreatableType).
disjointWith(A,B):- A=B,!,fail.
disjointWith(A,B):- asserted_or_trans_subclass(A,AS),asserted_or_trans_subclass(B,BS),(is_asserted(disjointWith(AS,BS));is_asserted(disjointWith(BS,AS))).
disjointWith(A,B):- once((type_isa(A,AT),type_isa(B,BT))),AT \= BT.

hook:decl_database_hook(assert(_),Fact):- check_was_known_false(Fact).

was_known_false(Fact):-is_known_false(Fact),retractall((is_known_false(_):-true)),dmsg(trace_or_throw(was_known_false(Fact))),dtrace.
check_was_known_false(Fact):-ignore(((is_known_false(Fact),was_known_false(Fact)))).


% ================================================
% fact_checked/2, fact_loop_checked/2
% ================================================
:-meta_module_transparent(fact_checked(0,0)).

fact_checked(Fact,Call):- no_loop_check(fact_checked0(Fact,Call)).
fact_checked0(Fact,Call):- not(ground(Fact)),!,Call.
fact_checked0(Fact,_):-is_known_true(Fact),!.
fact_checked0(Fact,_):-is_known_false(Fact),!,fail.
fact_checked0(_Fact,Call):-Call,!.
% would only work outside a loop checker (so disable)
% fact_checked0(Fact,_Call):- really_can_table_fact(Fact),asserta(is_known_false(Fact)),!,dmsg(is_known_false(Fact)),!,fail.

really_can_table_fact(Fact):-really_can_table,functor(Fact,F,__),can_table_functor(F),!.


can_table_functor(F):-cannot_table_functor(F),!,fail.
can_table_functor(_).

cannot_table_functor(atloc).
cannot_table_functor(isa).

:-meta_module_transparent(fact_loop_checked(-,0)).
fact_loop_checked(Fact,Call):- fact_checked(Fact,loop_check(Call,fail)).


never_type(Var):-var(Var),!,trace_or_throw(var_never_type(Var)).
never_type('Area1000').
never_type(subft).
never_type(must).
never_type(mpred_prop).
never_type(ft_info).
never_type(F):- mpred_arity(F,A),!, A > 1.

define_type(Spec):- never_type(Spec),!,trace_or_throw(never_type(Spec)).
define_type(M:F):-!, '@'(define_type(F), M).
define_type([]):-!.
define_type([A]):-!,define_type(A).
define_type([A|L]):-!,define_type(A),define_type(L).
define_type((A,L)):-!,define_type(A),define_type(L).


define_type(Spec):- compound(Spec),must_det(define_compound_as_type(Spec)).
define_type(Spec):- decl_mpred(Spec,1),declare_dbase_local_dynamic(Spec,1), define_type_0(Spec).

define_type_0(Spec):- dbase_t(type,Spec),!.
define_type_0(Spec):- add_w_hooks(dbase_t(type,Spec),isa(Spec,type)).

define_compound_as_type(Spec):- dbase_t(F,Spec),dmsg(once(define_compound_as_type(Spec,F))).
define_compound_as_type(Spec):- add(resultIsa(Spec,type)).
define_compound_as_type(Spec):- assertz_if_new(dbase_t(formattype,Spec)),dmsg(once(define_compound_as_type(Spec,formattype))).
define_compound_as_type(Spec):- compound(Spec),trace_or_throw(never_compound_define_type(Spec)).


:- include(moo_stubs).

:-forall(argsIsaProps(F),define_type(F)).

:-dynamic_multifile_exported(define_ft/1).
define_ft(Spec):- never_type(Spec),!,trace_or_throw(never_ft(Spec)).
define_ft(M:F):- !, '@'(define_ft(F), M).
define_ft(Spec):- compound(Spec),functor(Spec,F,_),!,define_ft_0(F),define_ft_0(Spec).
define_ft(Spec):- define_ft_0(Spec).

define_ft_0(Spec):- dbase_t(formattype,Spec),!.
define_ft_0(Spec):- dbase_t(type,Spec),dmsg(once(maybe_converting_plain_type_to_formattype(Spec))),fail.
define_ft_0(Spec):- add_w_hooks(dbase_t(formattype,Spec),isa(Spec,formattype)).

%type(Spec):- is_asserted(isa(Spec,type)).

define_type_if_atom(T):- compound(T),!.
define_type_if_atom(T):- ignore((atom(T),not(number(T)),define_type(T))).

hook:decl_database_hook(assert(_A_or_Z),label_type_props(Lbl,T,Props)):- add_w_hooks(default_type_props(self,T,[label(self,Lbl)|Props])).
hook:decl_database_hook(assert(_A_or_Z),default_type_props(_,T,_)):- define_type_if_atom(T).
hook:decl_database_hook(assert(_A_or_Z),subclass(S,C)):-define_type_if_atom(S),define_type_if_atom(C).

:- define_type(type).
:- define_type(extentKnown).
:- define_type(creatableType).
:- forall(argsIsaProps(Prop),(define_type(Prop),asserta(is_known_true(subclass(Prop,mpred))))).

is_creatable_type(Type):- arg(_,vv(agent,item,region,concept),Type).
is_creatable_type(Type):- atom(Type),call(is_asserted(isa(Type,creatableType))).

hook:decl_database_hook(assert(_A_or_Z),mpred_prop(F,arity(A))):- ignore((A==1,define_type(F))) , ignore((atom(P),define_type(P))).

% ========================================
% include_moo_files(MASK)
% ========================================

include_moo_files(Mask):- expand_file_name(Mask,X),
     forall(member(E,X),ensure_moo_loaded(E)).
/*
module(M,Preds):-
    'format'(user_error,'% visting module ~w.~n',[M]),
    forall(member(P,Preds),dynamic_multifile_exported(P)).
*/
scan_updates:-thread_property(X,alias(loading_code)),thread_property(X,status(running)),!.
scan_updates:-!.
scan_updates:-ignore(catch(make,_,true)).


do_term_expansions:- context_module(CM), notrace(do_term_expansions(CM)).

do_term_expansions(_):- thread_self(ID),always_expand_on_thread(ID),!.
do_term_expansions(_):- always_transform_heads,not(prevent_transform_moo_preds),!.
do_term_expansions(_):- is_compiling_clause.
do_term_expansions(CM):- may_moo_term_expand(CM),!, not(ended_transform_moo_preds), not(prevent_transform_moo_preds).

check_term_expansions:- not(do_term_expansions).

:- meta_predicate locate_moo_file(:,-).

locate_moo_file(I,O):-locate_moo_file0(I,O),!.
locate_moo_file(_:O,O):-!.
locate_moo_file(O,O).
locate_moo_file0(user:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(_:SpecPre, Path):-!,locate_moo_file0(SpecPre, Path).
locate_moo_file0(SpecPre, Path) :-
        catch((expand_file_search_path(SpecPre,Spec)),_,fail),
        catch(absolute_file_name(Spec,
                           [ file_type(prolog),
                             access(read)
                           ],
                           Path),_,fail),
        exists_file(Path),!.

:- meta_predicate ensure_moo_loaded(:).

% the once/1s here arte just for dmiles institional memory
ensure_moo_loaded(A) :-
   setup_call_cleanup(once(asserta(may_moo_term_expand(_))),
        load_moo_files(A),
        once(retract(may_moo_term_expand(asdasdasd)))).

:- meta_predicate load_moo_files(:,+).

load_moo_files(F0):-!,user_use_module(F0).
% load_moo_files(F0):-use_module(F0).

load_moo_files(M:F0,List):-!,
  locate_moo_file(M:F0,F),  % scope_settings  expand(true),register(false),
  % 'format'(user_error,'%  ~q + ~q -> ~q.~n',[M,F0,F]),
  load_files(F,[if(not_loaded), must_be_module(true)|List]).
   %load_files(F,[redefine_module(false),if(not_loaded),silent(false),reexport(true),must_be_module(true)|List]).   
load_moo_files(M:F0,List):-
  locate_moo_file(M:F0,F),  % scope_settings
  'format'(user_error,'% load_moo_files_M ~q.~n',[M=locate_moo_file(F0,F)]),
   load_files(F,[redefine_module(false),module(M),expand(true),if(not_loaded),reexport(true),register(false),silent(false),must_be_module(true)|List]).


% ========================================
% enter_term_anglify(MASK)
% ========================================

enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).
enter_term_anglify(X,Y):-findall(X-Y-Body,clause( term_anglify_np_last(X,Y),Body),List),!,member(X-Y-Body,List),call(Body).


% ================================================
% is_asserted/1
% ================================================

had_module(M:X,X):-atom(M).


:-dynamic_multifile_exported(is_asserted/1).
is_asserted(M:C):- atom(M),!,is_asserted(C).
is_asserted(C):-compound(C),functor(C,F,A),!,is_asserted(F,A,C). 

is_asserted(F,G):-must_det(functor(G,F,A)),is_asserted(F,A,G).

:-dynamic_multifile_exported(is_asserted/3).
is_asserted(M:F,A,C):- atom(M),!,is_asserted(F,A,C).
is_asserted(F,A,M:C):- atom(M),!,is_asserted(F,A,C).
%  %  is_asserted(dbase_t,1,dbase_t(C)):-!,dbase_t(C).
%  %  is_asserted(F,A,G):- is_asserted_lc_isa(F,A,G).
is_asserted(dbase_t,_,C):-C=..[_,L|IST],atom(L),!,CC=..[L|IST],is_asserted_mpred(CC).
%  %  is_asserted(Holds,_,C):-is_holds_true(Holds), C=..[_,L|IST],atom(L),!,CC=..[L|IST],is_asserted_mpred(CC).
is_asserted(_,_,G):-is_asserted_mpred(G).

:-dynamic_multifile_exported(is_asserted_mpred/1).

is_asserted_lc_isa(isa,2,isa(I,C)):-!,is_asserted_mpred_clause_isa(I,C).
is_asserted_lc_isa(dbase_t,2,dbase_t(C,I)):-!,is_asserted_mpred_clause_isa(I,C).
is_asserted_lc_isa(C,1,G):-arg(1,G,I),!,is_asserted_mpred_clause_isa(I,C).

is_asserted_mpred_clause_isa(I,C):-is_asserted_mpred(isa(I,C)).

is_asserted_mpred(G):-fact_loop_checked(G,asserted_mpred_clause(G)).

:-dynamic(was_asserted_gaf/1).
:-dynamic_multifile_exported(was_asserted_gaf/1).
:-export(asserted_mpred_clause/1).
asserted_mpred_clause(naf(C)):-!,not(is_asserted(C)).
asserted_mpred_clause(C):- (functor(C,dbase_t,_);functor(C,holds_t,_)),!,trace_or_throw(use_code(is_asserted(C))).
asserted_mpred_clause(C):-was_asserted_gaf(C).
asserted_mpred_clause(C):-dbase_t(C).
asserted_mpred_clause(C):-clause_asserted(C).
% asserted_mpred_clause(C):- asserted_mpred_clause_hardwork(C).

asserted_mpred_clause_hardwork(C):-clause_asserted(C,moo:game_call_head_body(C,Call)),   
                    must_det(ground(Call)), 
                    moo:game_call_head_body(C,Call).
asserted_mpred_clause_hardwork(C):- has_free_args(C),!,fail.
asserted_mpred_clause_hardwork(C):- hook:deduce_facts(Body, C),req(Body),must_det(ground(Body)),!.

% ============================================
% Prolog will_call_after/do_all_of
% ============================================

:-dynamic(moo:will_call_after/2).

call_after(When,C):- When,!,do_all_of(When),must_det(C),!.
call_after(When,C):- assert_next(When,C),!.


assert_next(_,_:true):-!.
assert_next(_,true):-!.
assert_next(When,C):- clause_asserted(moo:will_call_after(When,logOnFailure(C))),!.
assert_next(When,C):- assertz_if_new(moo:will_call_after(When,logOnFailure(C))).

call_after_next(When,C):- ignore((When,!,do_all_of(When))),assert_next(When,C).

do_all_of(When):- loop_check(do_all_of_lc(When),true).
do_all_of_lc(When):- not(moo:will_call_after(When,_)),!.
do_all_of_lc(When):- repeat, 
   forall(retract(moo:will_call_after(When,A)), 
      % dmsg(doingNow(When,A)),
                           call(A)), 
     not(moo:will_call_after(When,_)).



:- dynamic_multifile_exported(decl_coerce/3).


decl_coerce(_,_,_):-fail.

:-dynamic_multifile_exported(coerce/3).

coerce(What,Type,NewThing):-decl_coerce(What,Type,NewThing),!.
coerce(What,_Type,NewThing):-NewThing = What.



define_argType(F,N,ArgType):-decl_mpred(F,argIsa(N,ArgType)).




:- dynamic_multifile_exported do_expand_args/3.

do_expand_args(Exp,Term,Out):- compound(Term),!,do_expand_args_c(Exp,Term,Out).
do_expand_args(_,Term,Term).

do_expand_args_c(Exp,[L|IST],Out):- !,do_expand_args_l(Exp,[L|IST],Out).
do_expand_args_c(Exp,Term,Out):- Term=..[P|ARGS],do_expand_args_pa(Exp,P,ARGS,Out).

do_expand_args_pa(Exp,Exp,ARGS,Out):- !,member(Out,ARGS).
do_expand_args_pa(Exp,P,ARGS,Out):- do_expand_args_l(Exp,ARGS,EARGS), Out=..[P|EARGS].

do_expand_args_l(_,A,A):- var(A),!.
do_expand_args_l(_,[],[]):- !.
do_expand_args_l(Exp,[A|RGS],[E|ARGS]):- do_expand_args(Exp,A,E),do_expand_args_l(Exp,RGS,ARGS).



:- meta_predicate tick_every(*,*,0).
:- meta_predicate register_timer_thread(*,*,0).

:- decl_mpred_hybrid((nonCreatableType/1, type/1, subclass/2, argsIsa/1 ,creatableType/1, createableSubclassType/2)).

/*
:- decl_mpred_hybrid((createableSubclassType(type,type))).
:- decl_mpred_hybrid((creatableType(type))).
:- decl_mpred_hybrid type_grid/3.
*/
:- decl_mpred_hybrid(((formatted/1,
                       contains/2))).


register_timer_thread(Name,_Seconds,_OnTick):-current_thread(Name,_Status).
register_timer_thread(Name,Seconds,OnTick):-
   thread_create(tick_every(Name,Seconds,OnTick),_ID,[alias(Name)]). 

tick_every(Name,Seconds,OnTick):-repeat,sleep(Seconds),catch(OnTick,E,dmsg(caused(Name,OnTick,E))),fail.

hdr_debug(_,_):-!.
hdr_debug(F,A):-'format'(F,A).
:-meta_predicate term_expansion_local(?,?),term_expansion_local0(?,?).
% :-meta_predicate user:term_expansion(?,?).

term_expansion_local(X,_):-not(compound(X)),!,fail.
term_expansion_local( ((':-'(_))) , _ ):-!,fail.

term_expansion_local(_:B1,B2):-!,term_expansion_local(B1,B2),!.

term_expansion_local(((H1:-B1)),H2B2):- !,
   nonvar(B1),  current_context_module(CM),!,        
      functor_catch(H1,F,A), atom_concat(P,'_hook',F),!,atomic_list_concat_catch([_,_|_],'_',P),
      H1=..[F|ARGS], H2=..[P|ARGS],
      B2 = '@'((nop(CM), B1), CM ),
      module_transparent((P/A)),
      % copy_term(H2,META), meta_predicate(META),
      dynamic(P/A),
      dynamic_multifile_exported(P/A),
      multifile(P/A),
      ignore(H2B2 = ((H2 :- B2))),!.

term_expansion_local(X,Y):- compound(X),loading_module_h(CM),functor_catch(X,F,A),term_expand_local(CM,X,F,A,Y).



term_expand_local(CM,X,F,A,Y):-findall(Y,term_expand_local_each(CM,X,F,A,Y),Ys), Ys == [],!,fail.  

term_expand_local_each(_,_,F,A,_):- member(F / A,[never_expand]),!,fail.
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,utility),dynamic_multifile_exported(F/A).
term_expand_local_each(CM,X,F,A,X):-registered_module_type(CM,dynamic),dynamic(F/A).

term_expansion_local0(A,B):- compound(A),term_expansion_local(A,B),!.

% user:term_expansion(X,Y):- term_expansion_local0(X,Y).

% :- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

agent_text_command(_Agent,_Text,_AgentTarget,_Cmd):-fail.

:- rescan_mpred_props.

:- include(logicmoo('dbase/dbase.pl')).

% :- include(logicmoo('vworld/moo_footer.pl')).



