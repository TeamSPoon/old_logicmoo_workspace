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
is_holds_false(Prop):-notrace((atom(Prop),is_holds_false0(Prop))).

is_holds_false0(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,aint,assertion_f,asserted_dbase_f,retraction,not_secondOrder,not_firstOrder]).
%is_holds_false0(Prop):-is_holds_false0(Prop,Stem),is_holds_true0(Stem).
is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).


:- dynamic(non_assertable/1).
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,notAssertable(Why)):- compound(WW),functor_catch(WW,F,_),mpred_prop(F,notAssertable(Why)),!.
% non_assertable(WW,as_is(Why)):- compound(WW),functor_catch(WW,F,_),!,mpred_prop(F,as_is(Why)),!.
% non_assertable(WW,Why):- db_prop_add

% ========================================
% into_hilog_form/into_mpred_form
% ========================================

:-'$hide'(expanded_different/2).
:-export(expanded_different/2).

expanded_different(G0,G1):-call(expanded_different_ic(G0,G1)).

expanded_different_ic(G0,G1):-G0==G1,!,fail.
expanded_different_ic(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different_ic(G0,G1):- G0\==G1.

expanded_different_1(NV:G0,G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (var(G0);var(G1)),!,trace_or_throw(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


:-export(into_hilog_form/2).
into_hilog_form(G0,G1):-call(into_hilog_form_ic(G0,G1)).

into_hilog_form_ic(M:X,O):- atom(M),!,into_hilog_form_ic(X,O).
into_hilog_form_ic(X,O):- is_list(X),list_to_dbase_t(X,D),into_hilog_form_ic(D,O).
into_hilog_form_ic(X,O):- X=..[F|A],into_hilog_form(X,F,A,O).

% TODO finish negations
into_hilog_form(X,_,_,dbase_t(C,I)):-was_isa(X,I,C),!.
into_hilog_form(X,F,_A,X):- mpred_prop(F,as_is(_Why)),!.
into_hilog_form(X,F,_A,X):- mpred_prop(F,prologCall),!.
into_hilog_form(X,dbase_t,_A,X).
into_hilog_form(X,mpred_arity,_A,X).
into_hilog_form(X,mpred_prop,_A,X).
into_hilog_form(X,holds_t,_A,X).
into_hilog_form(X,holds_t,_A,X).
into_hilog_form(_X,F,A,Call):-Call=..[dbase_t,F|A].

list_to_dbase_t([P|List],DBASE_T):-P==dbase_t -> DBASE_T=..[P|List] ; DBASE_T=..[dbase_t,P|List].


hook:into_assertable_form_trans_hook(G,Dbase):- functor_catch(G,F,A),hook:into_assertable_form_trans_hook(G,F,A,Dbase).
hook:into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,prologBuiltin),!.
hook:into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,prologOnly),!.
hook:into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,as_is(_)),!.
hook:into_assertable_form_trans_hook(G,F,_,Dbase):-mpred_prop(F,prologHybrid),!,into_hilog_form(G,Dbase).
hook:into_assertable_form_trans_hook(G,F,_,Dbase):-mpred_prop(F,is_dbase_t),!,into_hilog_form(G,Dbase).
hook:into_assertable_form_trans_hook(G,F,_,was_asserted_gaf(G)):- mpred_prop(F,was_asserted_gaf),!.

:-dynamic_multifile_exported(into_assertable_form/2).
into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
% into_assertable_form(B,A):- save_in_dbase_t,!,into_hilog_form(B,A),!.
into_assertable_form(G0,G1):-call(into_assertable_form_ic(G0,G1)).

into_assertable_form_ic(H,G):- call_no_cuts((hook:into_assertable_form_trans_hook(H,G))),expanded_different(H,G),!.
% into_assertable_form_ic(H,GO):-expand_term( (H :- true) , C ), reduce_clause(C,G),expanded_different(H,G),!,into_assertable_form(G,GO),!.
into_assertable_form_ic(X,O):- functor_catch(X,F,A),into_assertable_form_via_mpred(X,F,A,O),!.
into_assertable_form_ic(X,O):- into_assertable_form(dbase_t,X,O),!.

into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,prologHybrid),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,as_is(_)),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- not(mpred_prop(F,is_dbase_t)),!,X=O.

:-dynamic_multifile_exported(into_assertable_form/3).
into_assertable_form(HLDS,M:X,O):- atom(M),!,into_assertable_form(HLDS,X,O),!.
into_assertable_form(HLDS,X,O):-call((( X=..[F|A],into_assertable_form(HLDS, X,F,A,O)))),!.

% TODO finish negations
into_assertable_form(Dbase_t,X,Dbase_t,_A,X):-!.
into_assertable_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,HLDS,A,Call):- is_holds_true(HLDS), Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,F,A,Call):-Call=..[Dbase_t,F|A].

:-dynamic_multifile_exported(into_mpred_form/2).

into_mpred_form(Var,MPRED):- var(Var), trace_or_throw(var_into_mpred_form(Var,MPRED)).
into_mpred_form(M:X,O):- atom(M),!,into_mpred_form(X,O),!.
into_mpred_form(I,O):-loop_check(into_mpred_form_lc(I,O),trace_or_throw(into_mpred_form(I,O))).

into_mpred_form_lc([L|List],O):-!,G=..[dbase_t|[L|List]], into_mpred_form(G,O).
into_mpred_form_lc(G,O):- functor(G,F,A),G=..[F,P|ARGS],!,into_mpred_form(G,F,P,A,ARGS,O),!.

% TODO confirm negations

into_mpred_form(_,F,_,1,[C],O):-alt_calls(F),!,into_mpred_form(C,O),!.
into_mpred_form(_,':-',C,1,_,':-'(C)):-!.
into_mpred_form(C,isa,_,2,_,C):-!.
% into_mpred_form(H,_,_,_,_,GO):- call(once((expand_term( (H :- true) , C ), reduce_clause(C,G)))),expanded_different(H,G),!,into_mpred_form(G,GO),!.
into_mpred_form(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form(G,F,_,_,_,G):-mpred_prop(F,prologBuiltin),!.
into_mpred_form(G,F,_,1,_,G):-mpred_prop(F,mped_type(callable(prologOnly))),!.
into_mpred_form(G,_,_,1,_,G):-predicate_property(G,number_of_rules(N)),N >0, !.
into_mpred_form(G,F,C,1,_,O):-predicate_property(G,builtin),!,into_mpred_form(C,OO),O=..[F,OO].
into_mpred_form(C,_,_,_,_,isa(I,T)):-was_isa(C,I,T),!.
into_mpred_form(_X,dbase_t,P,_N,A,O):-!,(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form(_X,H,P,_N,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form(_X,H,P,_N,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[holds_f,P|A]).
%into_mpred_form(PropsWas,props,_,2,_,Was):- props_into_mpred_form(PropsWas,Was).
into_mpred_form(X,_H,_P,_N,_A,X).

reduce_clause((C:- _:B),C):-B==true,!.
reduce_clause((C:- B),C):-B==true,!.
reduce_clause(C,C).

props_into_mpred_form(props(Obj,Open),_):- var(Open),!,trace_or_throw(var_props_into_mpred_form(props(Obj,Open))).
props_into_mpred_form(props(Obj,Obj,[]),true):- !.
props_into_mpred_form(props(Obj,[P]),MPRED):- nonvar(P),!,into_mpred_form(props(Obj,P),MPRED).
props_into_mpred_form(props(Obj,[P|ROPS]),(MPRED1,MPRED2)):- !,into_mpred_form(props(Obj,P),MPRED1),into_mpred_form(props(Obj,ROPS),MPRED2).
props_into_mpred_form(props(Obj,PropVal),MPRED):- atom(PropVal),!,Call=..[PropVal,Obj],!,into_mpred_form(Call,MPRED).
props_into_mpred_form(props(Obj,PropVal),MPRED):- safe_univ(PropVal,[Prop,NonVar|Val]),Obj==NonVar,!,into_mpred_form([dbase_t,Prop,Obj|Val],MPRED).
props_into_mpred_form(props(Obj,PropVal),MPRED):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAL=..[OP|Val],PropVal2=..[Pred,OPVAL],into_mpred_form(props(Obj,PropVal2),MPRED).
props_into_mpred_form(props(Obj,PropVal),MPRED):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,into_mpred_form([dbase_t,Prop,Obj|Val],MPRED).
props_into_mpred_form(props(Obj,PropVal),MPRED):- PropVal=..[Prop|Val],!,trace_or_throw(dtrace),into_mpred_form([dbase_t,Prop,Obj|Val],MPRED).
props_into_mpred_form(PROPS,MPRED):- trace_or_throw(unk_props_into_mpred_form(PROPS,MPRED)).

acceptable_xform(From,To):- From \=@= To,  (To = isa(I,C) -> was_isa(From,I,C); true).

:-export(was_isa/3).
was_isa(dbase_t(C,I),I,C):- maybe_typep(C/1),not(prolog_side_effects(C/1)).
was_isa(isa(I,C),I,C).
was_isa(dbase_t(isa,I,C),I,C).
was_isa(M:X,I,C):-atom(M),!,was_isa(M:X,I,C).
was_isa(X,I,C):-compound(X),functor(X,C,1),!,arg(1,X,I),maybe_typep(C/1),not(prolog_side_effects(C/1)).

:-export(prolog_side_effects/1).
prolog_side_effects(G):-var(G),!,fail.
prolog_side_effects(F/A):- ((integer(A);current_predicate(F/A)),functor(G,F,A)), prolog_side_effects(G).
prolog_side_effects(G):-functor_h(G,F),mpred_prop(F,sideEffect),!.
prolog_side_effects(G):-predicate_property(G,number_of_rules(N)),N >0,clause(G,(B,_)),compound(B),!.
prolog_side_effects(G):-predicate_property(G,exported),!.
prolog_side_effects(G):-functor_h(G,F),mpred_prop(F,prologBuiltin),!.
prolog_side_effects(G):-mpred_prop(G,mped_type(callable(prologOnly))),!.
prolog_side_effects(P):-atom(P),!,prolog_side_effects(P/_).


:-export(maybe_typep/1).
maybe_typep(F/A):- ((integer(A);current_predicate(F/A)),functor(G,F,A)), maybe_typep(G),!.
maybe_typep(G):-prolog_side_effects(G),!,fail.
maybe_typep(G):-functor_h(G,F),(mpred_prop(F,type);typeDeclarer(F)),!. %  ;type(F);formattype(F)
maybe_typep(F):-atom(F),!,maybe_typep(F/_).



foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):-not(compound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).

transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(argIsa_ft(F,N,FT)),FT=term,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.


transform_holds(H,In,Out):- once(transform_holds_3(H,In,Out)),!,ignore((In\=Out,fail,dmsg(transform_holds(H,In,Out)))).

transform_holds_3(_,A,A):-not(compound(A)),!.
transform_holds_3(_,props(Obj,Props),props(Obj,Props)):-!.
transform_holds_3(_,A,A):-functor_catch(A,F,N), predicate_property(A,_),mpred_prop(F,arity(N)),!.
transform_holds_3(HLDS,M:Term,OUT):-atom(M),!,transform_holds_3(HLDS,Term,OUT).
transform_holds_3(HLDS,[P,A|ARGS],DBASE):- var(P),!,DBASE=..[HLDS,P,A|ARGS].
transform_holds_3(HLDS, ['[|]'|ARGS],DBASE):- trace_or_throw(list_transform_holds_3(HLDS,['[|]'|ARGS],DBASE)).
transform_holds_3(Op,[SVOFunctor,Obj,Prop|ARGS],OUT):- is_svo_functor(SVOFunctor),!,transform_holds_3(Op,[Prop,Obj|ARGS],OUT).
transform_holds_3(_,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg(transform_holds_3),trace_or_throw(dtrace).
transform_holds_3(HLDS,[HOLDS,P,A|ARGS],OUT):- is_holds_true(HOLDS),!,transform_holds_3(HLDS,[P,A|ARGS],OUT).
transform_holds_3(HLDS,[HOLDS,P,A|ARGS],OUT):- HLDS==HOLDS, !, transform_holds_3(HLDS,[P,A|ARGS],OUT).
transform_holds_3(_,HOLDS,isa(I,C)):- was_isa(HOLDS,I,C),!.
transform_holds_3(_,[Type,Inst],isa(Inst,Type)):-must_det(not(type(Type))).
transform_holds_3(_,HOLDS,isa(I,C)):- holds_args(HOLDS,[ISA,I,C]),ISA==isa,!.

transform_holds_3(Op,[Logical|ARGS],OUT):- 
         call(call,logical_functor(Logical)),!,must(not(is_svo_functor(Logical))),
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Logical,ArgIn,ArgN,ArgOut),LARGS)),
         OUT=..[Logical|LARGS].

transform_holds_3(_,[props,Obj,Props],props(Obj,Props)).
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[isa(Type)|PROPS])):- nonvar(Inst), not(Type=props), cached_isa(Type,typeDeclarer),must_det(not(never_type(Type))),!.
transform_holds_3(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
transform_holds_3(_,[P,A|ARGS],DBASE):- !, nonvar(P),dumpST,trace_or_throw(dtrace), DBASE=..[P,A|ARGS].
transform_holds_3(Op,DBASE_T,OUT):- DBASE_T=..[P,A|ARGS],!,transform_holds_3(Op,[P,A|ARGS],OUT).

% Warning: moo:argIsa_ft/3, which is referenced by
% Warning:        /devel/logicmoo/src_incoming/dbase/dbase.pl:423:56: 1-st clause of moo:transform_functor_holds/5


holds_args([H|LIST],LISTO):- !, is_holds_true(H),!,LIST=LISTO.
holds_args(HOLDS,LIST):- compound(HOLDS),HOLDS=..[H|LIST],is_holds_true(H),!.


% ========================================
% assert/retract hooks
% ========================================
:- dynamic_multifile_exported hook:decl_database_hook/2.
% hooks are declared as
%        hook:decl_database_hook(assert(A_or_Z),Fact):- ...
%        hook:decl_database_hook(retract(One_or_All),Fact):- ...


run_database_hooks(change(A,B),Hook):- Change=..[A,B],!,run_database_hooks(Change,Hook).
run_database_hooks(Type,M:Hook):-atom(M),!,run_database_hooks(Type,Hook).
run_database_hooks(Type,HookIn):- into_mpred_form(HookIn,Hook),loop_check_local(run_database_hooks_1(Type,Hook),dmsg(looped_run_database_hooks(Type,Hook))).

run_database_hooks_1(Type,M:Hook):-atom(M),!,moo:run_database_hooks_1(Type,Hook).
run_database_hooks_1(Type,Hook):- loop_check(run_database_hooks_2(Type,Hook),true).
run_database_hooks_2(Type,Hook):- copy_term(Hook,HCopy),doall(call_no_cuts(hook:decl_database_hook(Type,HCopy))).



% ================================================
% fact_checked/2, fact_loop_checked/2
% ================================================
:- meta_predicate_transparent(fact_checked(?,0)).


fact_checked(Fact,Call):- not(ground(Fact)),!,no_loop_check(Call,fail).
fact_checked(Fact,_):- is_known_false0(Fact),!,fail.
fact_checked(Fact,_):- is_known_trew(Fact),!.
fact_checked(Fact,Call):- no_loop_check(Call,fail),(really_can_table_fact(Fact,true)->asserta(is_known_trew(Fact));true).
% fact_checked0(_Fact,Call):- asserta(is_known_false(Fact)),!,fail.
% would only work outside a loop checker (so disable)
% fact_checked0(Fact,_Call):- really_can_table_fact(Fact),asserta(is_known_false(Fact)),!,dmsg(is_known_false(Fact)),!,fail.

really_can_table_fact(Fact,TF):-really_can_table,functor(Fact,F,_),can_table_functor(F,TF),!.


can_table_functor(F,AsTrueOrFalse):-cannot_table_functor(F,AsTrueOrFalse),!,fail.
can_table_functor(_,_).

cannot_table_functor(atloc,_).
cannot_table_functor(isa,false).

:-meta_predicate_transparent(fact_loop_checked(+,0)).
fact_loop_checked(Fact,Call):- no_repeats(fact_checked(Fact,loop_check(Call,is_asserted(Fact)))).

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
is_asserted(dbase_t,_,C):-C=..[_,nart(List)|IST],!,nart_to_atomic(List,L),atom(L),CC=..[L|IST],is_asserted_mpred(CC).
%  %  is_asserted(Holds,_,C):-is_holds_true(Holds), C=..[_,L|IST],atom(L),!,CC=..[L|IST],is_asserted_mpred(CC).
is_asserted(_,_,G):-is_asserted_mpred(G).

:-dynamic_multifile_exported(is_asserted_mpred/1).

is_asserted_lc_isa(isa,2,isa(I,C)):-!,is_asserted_mpred_clause_isa(I,C).
is_asserted_lc_isa(dbase_t,2,dbase_t(C,I)):-!,is_asserted_mpred_clause_isa(I,C).
is_asserted_lc_isa(C,1,G):-arg(1,G,I),!,is_asserted_mpred_clause_isa(I,C).

is_asserted_mpred_clause_isa(I,C):-isa_asserted(C,I).


is_asserted_mpred(G):-var(G),!,trace_or_throw(var_is_asserted_mpred(G)).
is_asserted_mpred(mpred_prop(F,P)):-!,mpred_prop(F,P).
is_asserted_mpred(G):-fact_loop_checked(G,asserted_mpred_clause(G)).

:-dynamic(was_asserted_gaf/1).
:-dynamic_multifile_exported(was_asserted_gaf/1).
:-export(asserted_mpred_clause/1).
asserted_mpred_clause(naf(C)):-nonvar(C),!,not(is_asserted(C)).
asserted_mpred_clause(C):- (functor(C,dbase_t,_);functor(C,holds_t,_)),!,trace_or_throw(use_code(is_asserted(C))).
asserted_mpred_clause(C):-was_asserted_gaf(C).
asserted_mpred_clause(C):-dbase_t(C).
asserted_mpred_clause(C):-clause_asserted(C).
asserted_mpred_clause(C):-hook:fact_always_true(C).
asserted_mpred_clause(H):-not(ground(H)),predicate_property(H,number_of_clauses(_)),clause(H,true).
% asserted_mpred_clause(C):- asserted_mpred_clause_hardwork(C).

asserted_mpred_clause_hardwork(C):-clause_asserted(C,moo:call_mpred_body(C,Call)),   
                    must_det(ground(Call)), 
                    moo:call_mpred_body(C,Call).
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

:-dynamic(thglobal:will_call_after/2).

call_after(When,C):- When,!,do_all_of(When),must_det(C),!.
call_after(When,C):- assert_next(When,C),!.

assert_next(_,_:true):-!.
assert_next(_,true):-!.
assert_next(When,C):- clause_asserted(thglobal:will_call_after(When,logOnFailure(C))),!.
% assert_next(When,C):- nonground_throw_or_fail(C).
assert_next(When,C):- retractall(thglobal:will_call_after(When,logOnFailure(C))),!, assertz_if_new(thglobal:will_call_after(When,logOnFailure(C))).

call_after_next(When,C):- ignore((When,!,do_all_of(When))),assert_next(When,C).


do_all_of_when(When):- ignore((more_to_do(When),When,do_all_of(When))).

:-export(do_all_of/1).
do_all_of(When):- ignore(loop_check_local(do_all_of_lc(When),true)),!.
do_all_of_lc(When):- not(thglobal:will_call_after(When,_)),!.
do_all_of_lc(When):-  repeat,do_stuff_of_lc(When), not(more_to_do(When)).

more_to_do(When):-predicate_property(thglobal:will_call_after(When,_),number_of_clauses(N)),!,N>0.

do_stuff_of_lc(When):-not(more_to_do(When)),!.
do_stuff_of_lc(When):- thglobal:will_call_after(When,A),!,retract(thglobal:will_call_after(When,A)),!,call(A),!.


% ================================================
% hooked_assert/1 hooked_retract/1
% ================================================

ensure_predicate_reachable(M,C):-functor(C,F,A),ensure_predicate_reachable(M,C,F,A),fail.
ensure_predicate_reachable(_,_):- is_release,!.
ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:export(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).


ensure_predicate_reachable(M,C,dbase_t,Ap1):-C=..[_,F|_RGS],A is Ap1 -1, declare_dbase_local_dynamic_really(M,F,A).

% singletons_throw_or_fail(_):- is_release,!,fail.
singletons_throw_or_fail(C):- not_is_release,contains_singletons(C),!,(test_tl(thlocal:adding_from_srcfile) ->dmsg(contains_singletons(C)); trace_or_throw(contains_singletons(C))),fail.
nonground_throw_or_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(thlocal:adding_from_srcfile) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.


into_assertable_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,was_asserted_gaf),!.
into_assertable_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,query_with_pred(was_asserted_gaf)),!.

/*
into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
into_assertable_form(H,G):-into_assertable_form_trans(H,G),!.
into_assertable_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).
*/
% DONT_USE into_mpred_form(was_asserted_gaf(H),G):-!,into_mpred_form(H,G).
% DONT_USE into_mpred_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).


% only place ever should actual game dbase be changed from

:-export(into_mpred_aform/3).
into_mpred_aform(C,CP,CA):-into_mpred_form(C,CP),into_assertable_form(C,CA),!.

:- meta_predicate hooked_asserta(^), hooked_assertz(^), hooked_retract(^), hooked_retractall(^).
:-export((hooked_asserta/1,hooked_assertz/1)).
hooked_asserta(C):- into_mpred_aform(C,CP,CA),hooked_asserta(CP,CA).
hooked_assertz(C):- into_mpred_aform(C,CP,CA),hooked_assertz(CP,CA).
hooked_retract(C):- into_mpred_aform(C,CP,CA),hooked_retract(CP,CA).
hooked_retractall(C):- into_mpred_aform(C,CP,CA),hooked_retractall(CP,CA).

hooked_asserta(CP,_CA):- singletons_throw_or_fail(hooked_asserta(CP)).
hooked_asserta(_CP,CA):- singletons_throw_or_fail(hooked_asserta(CA)).
hooked_asserta(_CP,CA):- clause_asserted(CA),!.
hooked_asserta(CP,CA):- asserta_cloc(CA),run_database_hooks(assert(a),CP).


hooked_assertz(CP,_CA):- singletons_throw_or_fail(hooked_assertz(CP)).
hooked_assertz(_CP,CA):- singletons_throw_or_fail(hooked_assertz(CA)).
hooked_assertz(_CP,CA):- clause_asserted(CA),!.
hooked_assertz(CP,CA):- assertz_cloc(CA),run_database_hooks(assert(z),CP).

hooked_retract(CP,_CA):- nonground_throw_or_fail(hooked_retract(CP)).
hooked_retract(_CP,CA):- once(show_call_failure(clause_asserted(CA))),fail.
hooked_retract(CP,CA):-    copy_term(CP,CCP),
   ignore(retract_cloc(CA)),
   ignore((differnt_assert(CA,CP),retract_cloc(CP))),
   run_database_hooks(retract(one),CCP).

%hooked_retractall(CP,_CA):- nonground_throw_or_fail(hooked_retractall(CP)).
hooked_retractall(_CP,CA):- ground(CA), once(show_call_failure(clause_asserted(CA))),fail.
hooked_retractall(CP,CA):-
   copy_term(CP,CCP),
   retractall_cloc(CA),
   ignore((differnt_assert(CA,CP),retractall_cloc(CP))),
   run_database_hooks(retract(all),CCP).


differnt_assert(G1,G2):- notrace(differnt_assert1(G1,G2)),dmsg(differnt_assert(G1,G2)),ztrace.

differnt_assert1(M:G1,G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,M:G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G1,M1)),G1\=M1,!, differnt_assert1(M1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G2,M2)),G2\=M2,!, differnt_assert1(G1,M2).
differnt_assert1(G1,G2):- not((G1 =@= G2)).

show_cgoal(G):- stack_check(9600,dmsg(warning(maybe_overflow(stack_lvl)))),!,call(G).
show_cgoal(G):- % dmsg(show_cgoal(G)),
               call(G).


% only place ever should actual game database be changed from
asserta_cloc(M:C):-atom(M),!,asserta_cloc(M,C),!.
asserta_cloc( C ):-dbase_mod(M),asserta_cloc(M,C),!.
asserta_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
asserta_cloc(M,C):-singletons_throw_or_fail(M:C).
asserta_cloc(M,C):-clause_asserted(M:C,true),!.
asserta_cloc(M,C):-database_real(asserta,M:C).


assertz_cloc(M:C):-atom(M),!,assertz_cloc(M,C),!.
assertz_cloc( C ):-dbase_mod(M),assertz_cloc(M,C),!.
assertz_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
assertz_cloc(M,C):-singletons_throw_or_fail(M:C).
assertz_cloc(M,C):-clause_asserted(M:C,true),!.
assertz_cloc(M,C):-database_real(assertz,M:C).

retract_cloc(M:C):-atom(M),!,retract_cloc(M,C),!.
retract_cloc( C ):-dbase_mod(M),retract_cloc(M,C),!.
retract_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
%retract_cloc(M,C):-show_call_failure(not(clause_asserted(M:C))),!,fail.
retract_cloc(M,C):- database_real(retract,M:C).

retractall_cloc(M:C):-atom(M),!,retractall_cloc(M,C),!.
retractall_cloc( C ):-dbase_mod(M),retractall_cloc(M,C),!.
retractall_cloc(M,C):-ensure_predicate_reachable(M,C),fail.
%retractall_cloc(M,C):-not(clause_asserted(M:C)),!.
retractall_cloc(M,C):-database_real(retractall,M:C).

database_real(P,C):- 
    copy_term(C,CC),
      ignore((once((into_assertable_form(CC,DB), functor_h(C,CF),functor_h(DB,DBF))),DBF \== CF, 
        dmsg(warn_into_assertable_form(P,C,DB)),show_call_failure(debugOnError(call(P,DB))))),
      show_call_failure(debugOnError(call(P,C))).


% ========================================
% Rescan for consistency
% ========================================
:- export((rescan_dbase_facts/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).

rescan_dbase_facts:-!.
rescan_dbase_facts:-  loop_check_local(rescan_dbase_facts_local).

rescan_dbase_facts_local:-with_no_assertions(thglobal:use_cyc_database,(must_det(rescan_duplicated_facts),must_det(rerun_database_hooks))).

rescan_duplicated_facts:- !, notrace( forall(member(M,[moo,world,hook]), forall((predicate_property(M:H,dynamic),mpred_arity(F,A),functor(H,F,A)), rescan_duplicated_facts(M,H)))).
rescan_duplicated_facts(_M,_H):-!.
rescan_duplicated_facts(M,H):-!,rescan_duplicated_facts(M,H,true).
rescan_duplicated_facts(M,H):-findall(H,(clause_safe(M:H,B),B==true),CL1), once((list_to_set(CL1,CL2),reduce_fact_heads(M,H,CL1,CL2))).
rescan_duplicated_facts(M,H,BB):-notrace(doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause_safe(H,B),B=@=BB,reduce_clause((H:-B),C)),CL1),
                                                                     list_to_set(CL1,CL2),once(reduce_fact_heads(M,H,CL1,CL2))))))).
rerun_database_hooks:-!.
rerun_database_hooks:-timed_call(doall((gather_fact_heads(_M,H),forall(is_asserted(H),run_database_hooks(assert(z),H))))),fail.
rerun_database_hooks:-timed_call(doall((is_asserted(subclass(I,C)),run_database_hooks(assert(z),subclass(I,C))))),fail.
rerun_database_hooks:-timed_call(doall((isa_asserted(I,C),run_database_hooks(assert(z),isa(I,C))))),fail.

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


