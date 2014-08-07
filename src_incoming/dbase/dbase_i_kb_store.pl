/** <module> 
% ===================================================================
% File 'dbase_db_preds.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which change as
% the world is run.
%
%
% Dec 13, 2035
% Douglas Miles
*/


% ========================================
% Shared Preds
% ========================================
:-dynamic(fskel/7).
:-dynamic_multifile_exported(mpred_prop/2).


% ========================================
% dbase_mod/1
% ========================================

:- export dbase_mod/1.
:- dynamic dbase_mod/1.
dbase_mod(moo).


% ========================================
% is_holds_true/is_holds_false
% ========================================
% :- include(logicmoo(dbase/dbase_rules_nnf)).

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


:- dynamic(non_assertable/1).
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,notAssertable(Why)):- compound(WW),functor_catch(WW,F,_),mpred_prop(F,notAssertable(Why)),!.
% non_assertable(WW,as_is(Why)):- compound(WW),functor_catch(WW,F,_),!,mpred_prop(F,as_is(Why)),!.
% non_assertable(WW,Why):- db_prop_game_assert

% ========================================
% into_hilog_form/into_mpred_form
% ========================================

:-thread_local(thlocal:into_form_code/0).
:-dynamic_multifile_exported(thlocal:into_form_code/0).

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
into_hilog_form(G0,G1):-with_assertions(thlocal:into_form_code,into_hilog_form_ic(G0,G1)).

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
into_assertable_form(G0,G1):-with_assertions(thlocal:into_form_code,into_assertable_form_ic(G0,G1)).

into_assertable_form_ic(H,G):- call_no_cuts((hook:into_assertable_form_trans_hook(H,G))),expanded_different(H,G),!.
into_assertable_form_ic(H,GO):-expand_term( (H :- true) , C ), reduce_clause(C,G),expanded_different(H,G),!,into_assertable_form(G,GO),!.
into_assertable_form_ic(X,O):- functor_catch(X,F,A),into_assertable_form_via_mpred(X,F,A,O),!.
into_assertable_form_ic(X,O):- into_assertable_form(dbase_t,X,O),!.

into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,prologHybrid),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,as_is(_)),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- not(mpred_prop(F,is_dbase_t)),!,X=O.

:-dynamic_multifile_exported(into_assertable_form/3).
into_assertable_form(HLDS,M:X,O):- atom(M),!,into_assertable_form(HLDS,X,O),!.
into_assertable_form(HLDS,X,O):-with_assertions(thlocal:into_form_code,(( X=..[F|A],into_assertable_form(HLDS, X,F,A,O)))),!.

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
into_mpred_form(H,_,_,_,_,GO):- with_assertions(thlocal:into_form_code,once((expand_term( (H :- true) , C ), reduce_clause(C,G)))),expanded_different(H,G),!,into_mpred_form(G,GO),!.
into_mpred_form(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form(G,F,C,1,_,O):-predicate_property(G,builtin),!,into_mpred_form(C,OO),O=..[F,OO].
into_mpred_form(C,_,_,_,_,isa(I,T)):-was_isa(C,I,T),!.
into_mpred_form(_X,H,P,_N,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form(_X,H,P,_N,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[cholds_f,P|A]).
into_mpred_form(X,_,_,_,_,isa(I,C)):-was_isa(X,I,C),!.
into_mpred_form(X,_H,_P,_N,_A,X).


% ========================================
% assert/retract hooks
% ========================================
:- dynamic_multifile_exported hook:decl_database_hook/2.
% hooks are declared as
%        hook:decl_database_hook(assert(A_or_Z),Fact):- ...
%        hook:decl_database_hook(retract(One_or_All),Fact):- ...



run_database_hooks(Type,M:Hook):-atom(M),!,moo:run_database_hooks(Type,Hook).
run_database_hooks(Type,HookIn):- into_mpred_form(HookIn,Hook),run_database_hooks_1(Type,Hook).

run_database_hooks_1(Type,M:Hook):-atom(M),!,moo:run_database_hooks_1(Type,Hook).
run_database_hooks_1(Type,Hook):- loop_check(run_database_hooks_2(Type,Hook),true).
run_database_hooks_2(Type,Hook):- copy_term(Hook,HCopy),doall(call_no_cuts(hook:decl_database_hook(Type,HCopy))).


add_w_hooks(Gaf):- catch(hooked_asserta(Gaf), error(existence_error(procedure, _Call), context(_, _)),add_w_hooks_fallback(Gaf,Gaf)).
add_w_hooks(Data,Gaf):- catch((hooked_asserta(Data,Gaf)), error(existence_error(procedure, _Call), context(_, _)),add_w_hooks_fallback(Data,Gaf)).

add_w_hooks_fallback(Gaf,Gaf):-asserta_if_new(Gaf),run_database_hooks(assert(z),Gaf).
add_w_hooks_fallback(Data,Gaf):-asserta_if_new(Data),asserta_if_new(Gaf),run_database_hooks_1(assert(z),Gaf).



% ================================================
% fact_checked/2, fact_loop_checked/2
% ================================================
:-meta_module_transparent(fact_checked(0,0)).

fact_checked(Fact,Call):- no_loop_check(fact_checked0(Fact,Call)).
fact_checked0(Fact,Call):- not(ground(Fact)),!,Call.
fact_checked0(Fact,_):-is_known_false(Fact),!,fail.
%fact_checked0(Fact,_):-is_known_true(Fact),!.
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
% is_asserted(_,_,G):-was_isa(G,I,C),!,isa_asserted(I,C).
is_asserted(dbase_t,_,C):-C=..[_,L|IST],atom(L),!,CC=..[L|IST],is_asserted_mpred(CC).
%  %  is_asserted(Holds,_,C):-is_holds_true(Holds), C=..[_,L|IST],atom(L),!,CC=..[L|IST],is_asserted_mpred(CC).
is_asserted(_,_,G):-is_asserted_mpred(G).

:-dynamic_multifile_exported(is_asserted_mpred/1).

is_asserted_lc_isa(isa,2,isa(I,C)):-!,is_asserted_mpred_clause_isa(I,C).
is_asserted_lc_isa(dbase_t,2,dbase_t(C,I)):-!,is_asserted_mpred_clause_isa(I,C).
is_asserted_lc_isa(C,1,G):-arg(1,G,I),!,is_asserted_mpred_clause_isa(I,C).

% is_asserted_mpred_clause_isa(I,C):-is_asserted_mpred(isa(I,C)).

is_asserted_mpred(G):-fact_loop_checked(G,asserted_mpred_clause(G)).

:-dynamic(was_asserted_gaf/1).
:-dynamic_multifile_exported(was_asserted_gaf/1).
:-export(asserted_mpred_clause/1).
asserted_mpred_clause(naf(C)):-nonvar(C),!,not(is_asserted(C)).
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
% Prolog is_asserted_clause/2
% ============================================

is_asserted_clause(Head,Body):-clause(Head,Body).
is_asserted_clause(Head,true):-is_asserted(Head).

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

% singletons_throw_or_fail(_):- is_stable,!,fail.
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
% Rescan for consistency
% ========================================
:- export((rescan_dbase_t_once/0, rescan_dbase_t/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).

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



rerun_database_hooks:-doall((gather_fact_heads(_M,H),forall(is_asserted(H),run_database_hooks(assert(z),H)))).
rerun_database_hooks:-doall((is_asserted(subclass(I,C)),run_database_hooks(assert(z),subclass(I,C)))),fail.
rerun_database_hooks:-doall((isa_asserted(I,C),run_database_hooks(assert(z),isa(I,C)))),fail.

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


