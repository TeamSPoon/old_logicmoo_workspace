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

:- include(logicmoo('vworld/moo_header.pl')).

% ========================================
% Shared Preds
% ========================================
:-dynamic_multifile_exported(fskel/7).
:-dynamic_multifile_exported(mpred_prop/2).


% ========================================
% dbase_mod/1
% ========================================

:- dynamic_multifile_exported(dbase_mod/1).
:- dynamic dbase_mod/1.
dbase_mod(user).



user:decl_database_hook(One_or_All,Fact):- expire_tabled_list(all).

:-dynamic_multifile_exported(is_svo_functor/1).
is_svo_functor(Prop):- notrace((atom(Prop),arg(_,svo(svo,prop,valueOf,rdf),Prop))).

:-dynamic_multifile_exported(hilog_functor/1).
hilog_functor(dbase_ttttt).

:-dynamic_multifile_exported(is_holds_true_not_hilog/1).
is_holds_true_not_hilog(HOFDS):-is_holds_true(HOFDS),\+ hilog_functor(HOFDS).

:-dynamic_multifile_exported(is_holds_true/1).
is_holds_true(Prop):- notrace((atom(Prop),is_holds_true0(Prop))),!.

% k,p,..
is_holds_true0(Prop):-arg(_,vvv(holds,holds_t,dbase_t,asserted_dbase_t,assertion_t,assertion,secondOrder,firstOrder),Prop).
is_holds_true0(Prop):-atom_concat(_,'_t',Prop).

:-dynamic_multifile_exported(is_2nd_order_holds/1).
is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

:-dynamic_multifile_exported(is_holds_false/1).
is_holds_false(Prop):-notrace((atom(Prop),is_holds_false0(Prop))).

is_holds_false0(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,aint,assertion_f,asserted_dbase_f,retraction,not_secondOrder,not_firstOrder]).
is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat('int_not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).
%is_holds_false0(Prop):-is_holds_false0(Prop,Stem),is_holds_true0(Stem).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).


:- dynamic(non_assertable/1).
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,notAssertable(Why)):- compound(WW),get_functor(WW,F),mpred_prop(F,notAssertable(Why)),!.
% non_assertable(WW,Why):- db_prop_add

% ========================================
% into_hilog_form/into_mpred_form
% ========================================

:-'$hide'(expanded_different/2).
:-dynamic_multifile_exported(expanded_different/2).

expanded_different(G0,G1):-call(expanded_different_ic(G0,G1)).

expanded_different_ic(G0,G1):-G0==G1,!,fail.
expanded_different_ic(G0,G1):-expanded_different_1(G0,G1),!.
expanded_different_ic(G0,G1):- G0\==G1.

expanded_different_1(NV:G0,G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,NV:G1):-nonvar(NV),!,expanded_different_1(G0,G1).
expanded_different_1(G0,G1):- (var(G0);var(G1)),!,trace_or_throw(expanded_different(G0,G1)).
expanded_different_1(G0,G1):- G0 \= G1,!.


:-dynamic_multifile_exported(into_hilog_form/2).
into_hilog_form(G0,G1):-call(into_hilog_form_ic(G0,G1)).

into_hilog_form_ic(M:X,O):- atom(M),!,into_hilog_form_ic(X,O).
into_hilog_form_ic(X,O):- is_list(X),list_to_dbase_t(X,D),into_hilog_form_ic(D,O).
into_hilog_form_ic(X,O):- X=..[F|A],into_hilog_form(X,F,A,O).

% TODO finish negations
into_hilog_form(X,_,_,hasInstance(C,I)):-was_isa(X,I,C),!.
into_hilog_form(X,F,_A,X):- mpred_prop(F,prologOnly),!.
into_hilog_form(X,F,_A,X):- mpred_prop(F,actProlog),!.
into_hilog_form(X,dbase_t,_A,X).
into_hilog_form(X,mpred_arity,_A,X).
into_hilog_form(X,mpred_prop,_A,X).
into_hilog_form(X,holds_t,_A,X).
into_hilog_form(X,holds_t,_A,X).
into_hilog_form(_X,F,A,Call):-Call=..[dbase_t,F|A].

list_to_dbase_t([P|Fist],DBASE_T):-P==dbase_t -> DBASE_T=..[P|Fist] ; DBASE_T=..[dbase_t,P|Fist].


into_assertable_form_trans_hook(G,Dbase):- functor_catch(G,F,A),into_assertable_form_trans_hook(G,F,A,Dbase).
into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,prologPTTP),!.
into_assertable_form_trans_hook(G,F,_,(G)):- mpred_prop(F,prologOnly),!.
into_assertable_form_trans_hook(G,F,_,Dbase):-mpred_prop(F,prologHybrid),!,into_hilog_form(G,Dbase).
into_assertable_form_trans_hook(G,F,_,Dbase):-mpred_prop(F,is_dbase_t),!,into_hilog_form(G,Dbase).


:-dynamic_multifile_exported(into_assertable_form/2).
into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
% into_assertable_form(B,A):- save_in_dbase_t,!,into_hilog_form(B,A),!.
into_assertable_form(G0,G1):-call(into_assertable_form_ic(G0,G1)).

into_assertable_form_ic(H,G):- call_no_cuts((into_assertable_form_trans_hook(H,G))),expanded_different(H,G),!.
% into_assertable_form_ic(H,GO):-expand_term( (H :- true) , C ), reduce_clause(C,G),expanded_different(H,G),!,into_assertable_form(G,GO),!.
into_assertable_form_ic(X,O):- functor_catch(X,F,A),into_assertable_form_via_mpred(X,F,A,O),!.
into_assertable_form_ic(X,O):- into_assertable_form(dbase_t,X,O),!.

into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,prologHybrid),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- mpred_prop(F,prologOnly),!,X=O.
into_assertable_form_via_mpred(X,F,_A,O):- not(mpred_prop(F,is_dbase_t)),!,X=O.

:-dynamic_multifile_exported(into_assertable_form/3).
into_assertable_form(HFDS,M:X,O):- atom(M),!,into_assertable_form(HFDS,X,O),!.
into_assertable_form(HFDS,X,O):-call((( X=..[F|A],into_assertable_form(HFDS, X,F,A,O)))),!.

% TODO finish negations
into_assertable_form(Dbase_t,X,Dbase_t,_A,X):-!.
into_assertable_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,holds_t,A,Call):-Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,HFDS,A,Call):- is_holds_true(HFDS), Call=..[Dbase_t|A].
into_assertable_form(Dbase_t,_X,F,A,Call):-Call=..[Dbase_t,F|A].

:-moo_hide_childs(into_mpred_form/2).
:-export(into_mpred_form/2).
into_mpred_form(V,VO):-not(compound(V)),!,VO=V.
into_mpred_form(M:X,M:O):- atom(M),!,into_mpred_form(X,O),!.
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H:-B),(HH:-BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H,B),(HH,BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form((H;B),(HH;BB)):-!,into_mpred_form(H,HH),into_mpred_form(B,BB).
into_mpred_form(WAS,mudIsa(I,C)):-was_isa(WAS,I,C),!.
into_mpred_form(dbase_t(P,A,B),O):-atomic(P),!,O=..[P,A,B].
into_mpred_form(dbase_t(P,A,B,C),O):-atomic(P),!,O=..[P,A,B,C].
into_mpred_form(Var,MPRED):- var(Var), trace_or_throw(var_into_mpred_form(Var,MPRED)).
into_mpred_form(I,O):-loop_check(into_mpred_form_lc(I,O),trace_or_throw(into_mpred_form(I,O))).

into_mpred_form_lc([F|Fist],O):-!,G=..[dbase_t|[F|Fist]], into_mpred_form(G,O).
into_mpred_form_lc(G,O):- functor(G,F,A),G=..[F,P|ARGS],!,into_mpred_form(G,F,P,A,ARGS,O),!.

% TODO confirm negations

into_mpred_form(_,F,_,1,[C],O):-alt_calls(F),!,into_mpred_form(C,O),!.
into_mpred_form(_,':-',C,1,_,':-'(C)):-!.
into_mpred_form(C,mudIsa,_,2,_,C):-!.
% into_mpred_form(H,_,_,_,_,GO):- call(once((expand_term( (H :- true) , C ), reduce_clause(C,G)))),expanded_different(H,G),!,into_mpred_form(G,GO),!.
into_mpred_form(_,not,C,1,_,not(O)):-into_mpred_form(C,O),!.
into_mpred_form(G,F,_,_,_,G):-mpred_prop(F,prologOnly),!.
into_mpred_form(G,F,_,1,_,G):-mpred_prop(F,predStubType((prologOnly))),!.
into_mpred_form(G,_,_,1,_,G):-predicate_property(G,number_of_rules(N)),N >0, !.
into_mpred_form(G,F,C,1,_,O):-predicate_property(G,builtin),!,into_mpred_form(C,OO),O=..[F,OO].
into_mpred_form(C,_,_,_,_,mudIsa(I,T)):-was_isa(C,I,T),!.
into_mpred_form(_X,dbase_t,P,_N,A,O):-!,(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
into_mpred_form(_X,H,P,_N,A,O):-is_holds_false(H),(atom(P)->(G=..[P|A],O=not(G));O=..[holds_f,P|A]).
into_mpred_form(_X,H,P,_N,A,O):-is_holds_true(H),(atom(P)->O=..[P|A];O=..[dbase_t,P|A]).
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
props_into_mpred_form(props(Obj,PropVal),MPRED):- PropVal=..[OP,Pred|Val],comparitiveOp(OP),not(comparitiveOp(Pred)),!,OPVAF=..[OP|Val],PropVal2=..[Pred,OPVAF],into_mpred_form(props(Obj,PropVal2),MPRED).
props_into_mpred_form(props(Obj,PropVal),MPRED):- PropVal=..[Prop|Val],not(infix_op(Prop,_)),!,into_mpred_form([dbase_t,Prop,Obj|Val],MPRED).
props_into_mpred_form(props(Obj,PropVal),MPRED):- PropVal=..[Prop|Val],!,trace_or_throw(dtrace),into_mpred_form([dbase_t,Prop,Obj|Val],MPRED).
props_into_mpred_form(PROPS,MPRED):- trace_or_throw(unk_props_into_mpred_form(PROPS,MPRED)).

acceptable_xform(From,To):- From \=@= To,  (To = mudIsa(I,C) -> was_isa(From,I,C); true).


cant_be_col(':-').
cant_be_col('include').
cant_be_col('onSpawn').
cant_be_col('ensure_loaded').
cant_be_col(C):-mpred_prop(C,prologOnly),!.
cant_be_col('declare_load_game').
cant_be_col('user_ensure_loaded').
cant_be_col(C):-user:hasInstance_dyn(tCol,C),!,fail.
cant_be_col(_).
/*
cant_be_col('meta_predicate').
cant_be_col(C):-functor(G,C,1),predicate_property(G,built_in),!.
cant_be_col(C):-current_predicate(C/1),!.
cant_be_col(C):-prolog_side_effects(C/1),!.
cant_be_col(C):-dmsg(cant_be_col(C)),!,asserta_if_new(mpred_prop(C,prologOnly)).
*/

:-dynamic_multifile_exported(was_isa/3).
was_isa(V,_,_):-call(is_ftVar(V)),!,fail.
was_isa(X,I,C):-nonvar(X),!,once(was_isa0(X,I,C)),!.

was_isa0(mudIsa(I,C),I,C).
was_isa0(is_typef(_),_,_):-!,fail.
was_isa0(notrace(_),_,_):-!,fail.
was_isa0(call(_),_,_):-!,fail.
was_isa0(trace(_),_,_):-!,fail.
was_isa0(tCol(I),I,tCol).
was_isa0(ttNotCreatable(I),I,ttNotCreatable).
was_isa0(tChannel(I),I,tChannel).
was_isa0(tAgentGeneric(I),I,tAgentGeneric).
was_isa0(dbase_t(C,I),I,C).
was_isa0(dbase_t(P,I,C),I,C):-P==mudIsa.
was_isa0(isa(I,C),I,C).
% was_isa0(hasInstance(C,I),I,C).
was_isa0(M:X,I,C):-atom(M),!,was_isa(X,I,C).
was_isa0(X,I,C):-compound(X),X=..[C,I],!,is_typef(C).

%was_isa0(dbase_t(C,I),I,C):- nonvar(C),maybe_typep(C/1).

:-dynamic_multifile_exported(prolog_side_effects/1).
prolog_side_effects(G):-var(G),!,fail.
prolog_side_effects(F/A):- ((integer(A);current_predicate(F/A)),functor(G,F,A)), prolog_side_effects(G).
prolog_side_effects(G):-get_functor(G,F),mpred_prop(F,sideEffect),!.
prolog_side_effects(G):-predicate_property(G,number_of_rules(N)),N >0,clause(G,(B,_)),compound(B),!.
prolog_side_effects(G):-predicate_property(G,exported),!.
prolog_side_effects(G):-functor_h(G,F),mpred_prop(F,prologOnly),!.
prolog_side_effects(G):-mpred_prop(G,predStubType(prologOnly)),!.
prolog_side_effects(P):-atom(P),!,prolog_side_effects(P/_).


:-dynamic_multifile_exported(maybe_typep/1).
is_typep(G):- get_functor(G,F),is_typef(F).

is_typef(C):-var(C),!,fail.
is_typef(prologSingleValued).
is_typef(F):- (hasInstance_dyn(macroDeclarer,F);hasInstance_dyn(tCol,F);clause(mpred_prop(F,tCol),true)),!.
is_typef(F):- atom(F),isa_from_morphology(F,TT),!,atom_concat(_,'Type',TT).

maybe_typep(G):- is_typep(G),!.
%maybe_typep(G):-prolog_side_effects(G),!,fail.


foreach_arg(ARGS,_N,_ArgIn,_ArgN,_ArgOut,_Call,ARGS):-not(compound(ARGS)),!.
foreach_arg([ArgIn1|ARGS],ArgN1,ArgIn,ArgN,ArgOut,Call1,[ArgOut1|ARGSO]):-
     copy_term( a(ArgIn1,ArgOut1,ArgN1,Call1), a(ArgIn,ArgOut,ArgN,Call) ),
      call(Call),
      ArgN2 is ArgN + 1,
      foreach_arg(ARGS,ArgN2,ArgIn,ArgN,ArgOut,Call,ARGSO).

transform_functor_holds(_,F,ArgInOut,N,ArgInOut):- once(argIsa_ft(F,N,FT)),FT=ftTerm,!.
transform_functor_holds(Op,_,ArgIn,_,ArgOut):- transform_holds(Op,ArgIn,ArgOut),!.

transform_holds(H,In,Out):- once(transform_holds_3(H,In,Out)),!,ignore((In\=Out,fail,dmsg(transform_holds(H,In,Out)))).

transform_holds_3(_,A,A):-not(compound(A)),!.
transform_holds_3(_,props(Obj,Props),props(Obj,Props)):-!.
transform_holds_3(_,A,A):-functor_catch(A,F,N), predicate_property(A,_),mpred_prop(F,predArity(N)),!.
transform_holds_3(HFDS,M:Term,OUT):-atom(M),!,transform_holds_3(HFDS,Term,OUT).
transform_holds_3(HFDS,[P,A|ARGS],DBASE):- var(P),!,DBASE=..[HFDS,P,A|ARGS].
transform_holds_3(HFDS, ['[|]'|ARGS],DBASE):- trace_or_throw(list_transform_holds_3(HFDS,['[|]'|ARGS],DBASE)).
transform_holds_3(Op,[SVOFunctor,Obj,Prop|ARGS],OUT):- is_svo_functor(SVOFunctor),!,transform_holds_3(Op,[Prop,Obj|ARGS],OUT).
transform_holds_3(_,[P|ARGS],[P|ARGS]):- not(atom(P)),!,dmsg(transform_holds_3),trace_or_throw(dtrace).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- is_holds_true(HOFDS),!,transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(HFDS,[HOFDS,P,A|ARGS],OUT):- HFDS==HOFDS, !, transform_holds_3(HFDS,[P,A|ARGS],OUT).
transform_holds_3(_,HOFDS,mudIsa(I,C)):- was_isa(HOFDS,I,C),!.
transform_holds_3(_,[Type,Inst],mudIsa(Inst,Type)):-nonvar(Type),show_call_failure((tCol(Type))).
transform_holds_3(_,HOFDS,mudIsa(I,C)):- holds_args(HOFDS,[ISA,I,C]),ISA==mudIsa,!.

transform_holds_3(Op,[Fogical|ARGS],OUT):- 
         call(call,logical_functor(Fogical)),!,must(not(is_svo_functor(Fogical))),
         must_det(foreach_arg(ARGS,1,ArgIn,ArgN,ArgOut,transform_functor_holds(Op,Fogical,ArgIn,ArgN,ArgOut),FARGS)),
         OUT=..[Fogical|FARGS].

transform_holds_3(_,[props,Obj,Props],props(Obj,Props)).
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[mudIsa(Type)|PROPS])):- 
                  nonvar(Inst), not(Type=props), hasInstance_dyn(tCol,Type), must_det(not(never_type(Type))),!.
transform_holds_3(_,[Type,Inst|PROPS],props(Inst,[mudIsa(Type)|PROPS])):- 
                  nonvar(Inst), not(Type=props), hasInstance_dyn(macroDeclarer,Type), must_det(not(never_type(Type))),!.

transform_holds_3(_,[P,A|ARGS],DBASE):- atom(P),!,DBASE=..[P,A|ARGS].
transform_holds_3(_,[P,A|ARGS],DBASE):- !, nonvar(P),dumpST,trace_or_throw(dtrace), DBASE=..[P,A|ARGS].
transform_holds_3(Op,DBASE_T,OUT):- DBASE_T=..[P,A|ARGS],!,transform_holds_3(Op,[P,A|ARGS],OUT).

% Warning: argIsa_ft/3, which is referenced by
% Warning:        /devel/logicmoo/src_incoming/dbase/dbase.pl:423:56: 1-st clause of transform_functor_holds/5


holds_args([H|FIST],FISTO):- !, is_holds_true(H),!,FIST=FISTO.
holds_args(HOFDS,FIST):- compound(HOFDS),HOFDS=..[H|FIST],is_holds_true(H),!.
% ========================================
% assert/retract hooks
% ========================================
:- dynamic_multifile_exported user:decl_database_hook/2.
% hooks are declared as
%        user:decl_database_hook(assert(A_or_Z),Fact):- ...
%        user:decl_database_hook(retract(One_or_All),Fact):- ...

run_database_hooks(Type,Hook):- thlocal:noDBaseHOOKS(_),dmsg(noDBaseHOOKS(Type,Hook)),!.

run_database_hooks(change(A,B),Hook):- Change=..[A,B],!,run_database_hooks(Change,Hook).
run_database_hooks(Type,M:Hook):-atom(M),!,run_database_hooks(Type,Hook).
run_database_hooks(Type,HookIn):- into_mpred_form(HookIn,Hook),loop_check_local(run_database_hooks_1(Type,Hook),dmsg(looped_run_database_hooks(Type,Hook))).

run_database_hooks_1(Type,M:Hook):-atom(M),!,run_database_hooks_1(Type,Hook).
run_database_hooks_1(Type,Hook):- loop_check(run_database_hooks_2(Type,Hook),true).
run_database_hooks_2(Type,Hook):- copy_term(Hook,HCopy),doall(call_no_cuts(user:decl_database_hook(Type,HCopy))).



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

cannot_table_functor(mudAtFoc,_).
cannot_table_functor(mudIsa,false).

:-meta_predicate_transparent(fact_loop_checked(+,0)).
fact_loop_checked(Fact,Call):- no_repeats(fact_checked(Fact,loop_check(Call,is_asserted(Fact)))).

% ================================================
% is_asserted/1
% ================================================

had_module(M:X,X):-atom(M).

:-dynamic_multifile_exported(is_asserted/1).
is_asserted(M:G):- atom(M),!,is_asserted(G).
is_asserted(G):-compound(G),functor(G,F,A),!,is_asserted(F,A,G). 
is_asserted(F,G):-must_det(functor(G,F,A)),is_asserted(F,A,G).

:-dynamic_multifile_exported(is_asserted/3).
is_asserted(once,1,once(G)):-!,is_asserted(G),!.
is_asserted(M:F,A,G):- atom(M),!,is_asserted(F,A,G).
is_asserted(F,A,M:G):- atom(M),!,is_asserted(F,A,G).
is_asserted(_,_,G):-was_isa(G,I,G),!,loop_check(isa_asserted(I,G)).

is_asserted(dbase_t,1,dbase_t(G)):-!,dbase_t(G).
is_asserted(dbase_t,_,G):-G=..[_,NA|IST],nonvar(NA),NA=nart(Fist),!,must((nart_to_atomic(Fist,F),atom(F),CC=..[F|IST],is_asserted_mpred(F,CC))).
is_asserted(dbase_t,_,G):-G=..[_,F|IST],atom(F),!,CC=..[F|IST],is_asserted_mpred(F,CC).
is_asserted(Holds,_,G):-is_holds_true(Holds), G=..[_,F|IST],atom(F),!,CC=..[F|IST],is_asserted_mpred(F,CC).
is_asserted(F,_,G):-is_asserted_mpred(F,G).


% nart_to_atomic(F,F):-!,atom(F).
nart_to_atomic(F,F).


:-dynamic_multifile_exported(is_asserted_mpred/1).

is_asserted_mpred(_,G):-var(G),!,trace_or_throw(var_is_asserted_mpred(F,G)).
is_asserted_mpred(mpred_prop,mpred_prop(F,P)):-!,mpred_prop(F,P).
is_asserted_mpred(F,G):-fact_loop_checked(G,(dbase_t(G);fact_always_true(G))).

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
% assert_next(When,C):- nonground_throw_else_fail(C).
assert_next(When,C):- retractall(thglobal:will_call_after(When,logOnFailure(C))),!, assertz_if_new(thglobal:will_call_after(When,logOnFailure(C))).

call_after_next(When,C):- ignore((When,!,do_all_of(When))),assert_next(When,C).


do_all_of_when(When):- ignore((more_to_do(When),When,do_all_of(When))).

:-dynamic_multifile_exported(do_all_of/1).
do_all_of(When):- ignore(loop_check_local(do_all_of_lc(When),true)),!.
do_all_of_lc(When):- not(thglobal:will_call_after(When,_)),!.
do_all_of_lc(When):-  repeat,do_stuff_of_lc(When), not(more_to_do(When)).

more_to_do(When):-predicate_property(thglobal:will_call_after(When,_),number_of_clauses(N)),!,N>0.

do_stuff_of_lc(When):-not(more_to_do(When)),!.
do_stuff_of_lc(When):- thglobal:will_call_after(When,A),!,retract(thglobal:will_call_after(When,A)),!,call(A),!.


% ================================================
% hooked_assert/1 hooked_retract/1
% ================================================
ensure_predicate_reachable(_,_):- fast_mud,!.
ensure_predicate_reachable(M,C):-functor(C,F,A),ensure_predicate_reachable(M,C,F,A),fail.
ensure_predicate_reachable(_,_):- is_release,!.
ensure_predicate_reachable(M,C):-once((predicate_property(C,imported_from(Other)),M\=Other,
                                       context_module(CM),
                                       dmsg(wrong_import_module(M,Other:C,from(CM))),
                                       ignore(delete_import_module(CM,Other)),
                                       '@'((M:dynamic(C),M:dynamic_multifile_exported(C)),M),user:import(M:C))),fail.
ensure_predicate_reachable(_,_).


%ensure_predicate_reachable(M,C,dbase_t,Ap1):-C=..[_,F|_RGS],A is Ap1 -1, dmsg(( ensure_universal_stub(M,F,A))).

% singletons_throw_else_fail(_):- is_release,!,fail.
singletons_throw_else_fail(C):- not_is_release,contains_singletons(C),!,(test_tl(thlocal:adding_from_srcfile) ->dmsg(contains_singletons(C)); trace_or_throw(contains_singletons(C))),fail.
nonground_throw_else_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(thlocal:adding_from_srcfile) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.


into_assertable_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,was_asserted_gaf),!.
into_assertable_form_trans(G,was_asserted_gaf(G)):- functor_catch(G,F,_),mpred_prop(F,predProxyQuery(was_asserted_gaf)),!.

/*
into_assertable_form(M:H,G):-atom(M),!,into_assertable_form(H,G).
into_assertable_form(H,G):-into_assertable_form_trans(H,G),!.
into_assertable_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).
*/
% DONT_USE into_mpred_form(was_asserted_gaf(H),G):-!,into_mpred_form(H,G).
% DONT_USE into_mpred_form(H,G):-expand_term( (H :- true) , C ), reduce_clause(C,G).


% only place ever should actual game dbase be changed from

singletons_throw_else_fail(C):- not_is_release,contains_singletons(C),!,(test_tl(thlocal:adding_from_srcfile) ->dmsg(contains_singletons(C)); trace_or_throw(contains_singletons(C))),fail.
nonground_throw_else_fail(C):- not_is_release,not(ground(C)),!,( (test_tl(thlocal:adding_from_srcfile) ->dmsg(not_ground(C)); trace_or_throw(not_ground(C)))),fail.

% only place ever should actual game dbase be changed from
:- export(into_mpred_aform/3).
into_mpred_aform(C,MP,CA):-into_mpred_form(C,MP),into_assertable_form(C,CA),!.
hooked_op(U,C):-loop_check_term(hooked_op0(U,C),U,true).

hooked_op0(retract(all),C):-!, hooked_retractall(C).
hooked_op0(retract(_One),C):-!, hooked_retract(C).
hooked_op0(assert(a),C):-!, hooked_asserta(C).
hooked_op0(assert(z),C):-!, hooked_assertz(C).
hooked_op0(OP,CA):- trace_or_throw(unkown_hooked_op(OP,CA)), !, database_modify(OP,CA).

hooked_asserta(C):- into_mpred_aform(C,MP,CA),hooked_asserta(MP,CA).
hooked_assertz(C):- into_mpred_aform(C,MP,CA),hooked_assertz(MP,CA).
hooked_retract(C):- into_mpred_aform(C,MP,CA),hooked_retract(MP,CA).
hooked_retractall(C):- into_mpred_aform(C,MP,CA),hooked_retractall(MP,CA).

hooked_asserta(MP,_):- database_check(clause_asserted,MP),!.
hooked_asserta(MP,CA):- asserta_cloc(MP,CA),run_database_hooks(assert(a),MP).

hooked_assertz(MP,_):- database_check(clause_asserted,MP),!.
hooked_assertz(MP,CA):- assertz_cloc(MP,CA),run_database_hooks(assert(z),MP).


hooked_retract(MP,_):- nonground_throw_else_fail(hooked_retract(MP)).
hooked_retract(_,CA):- once(show_call_failure(must(is_asserted(CA)))),fail.
hooked_retract(MP,CA):- copy_term(MP,CMP),
   retract_cloc(MP,CA),   
   ignore((differnt_assert(CA,MP),retractall_cloc(MP))),
   copy_term(MP,CMP),run_database_hooks(retract(one),CMP).

%hooked_retractall(MP,CA):- nonground_throw_else_fail(hooked_retractall(MP)).
hooked_retractall(_,CA):- ground(CA), once(show_call_failure((is_asserted(CA)))),fail.
hooked_retractall(MP,CA):-
   retractall_cloc(MP,CA),
   copy_term(MP,CMP),run_database_hooks(retract(all),CMP).


differnt_assert(G1,G2):- notrace(differnt_assert1(G1,G2)),dmsg(differnt_assert(G1,G2)),ztrace.

differnt_assert1(M:G1,G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,M:G2):-atom(M),!, differnt_assert1(G1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G1,M1)),G1\=M1,!, differnt_assert1(M1,G2).
differnt_assert1(G1,G2):- once(into_mpred_form(G2,M2)),G2\=M2,!, differnt_assert1(G1,M2).
differnt_assert1(G1,G2):- not((G1 =@= G2)).

show_cgoal(G):- stack_check(9600,dmsg(warning(maybe_overflow(stack_lvl)))),!,call(G).
show_cgoal(G):- % dmsg(show_cgoal(G)),
               call(G).


% ========================================
% only place ever should actual game database be changed from
% ========================================
asserta_cloc(C):-   into_mpred_aform(C,MP,CA),asserta_cloc(MP,CA).
assertz_cloc(C):-   into_mpred_aform(C,MP,CA),assertz_cloc(MP,CA).
retract_cloc(C):-   into_mpred_aform(C,MP,CA),retract_cloc(MP,CA).
retractall_cloc(C):- into_mpred_aform(C,MP,CA),retractall_cloc(MP,CA).

asserta_cloc(MP,CA):- singletons_throw_else_fail(asserta_cloc(MP)); singletons_throw_else_fail(asserta_cloc(CA)).
asserta_cloc(_,CA):- database_modify(assert(a),CA).
assertz_cloc(MP,CA):- singletons_throw_else_fail(assertz_cloc(MP)); singletons_throw_else_fail(assertz_cloc(CA)).
assertz_cloc(_,CA):-database_modify(assert(z),CA).
retract_cloc(MP,CA):- show_call_failure((database_check(clause_asserted,MP);database_check(clause_asserted,CA))),fail.
retract_cloc(MP,CA):- singletons_throw_else_fail(assertz_cloc(MP)); singletons_throw_else_fail(assertz_cloc(CA)).
retract_cloc(_,CA):- database_modify(retract(one),CA).
retractall_cloc(MP,CA):-database_modify(retract(all),CA),must(not(is_asserted(MP))).


clause_stored(HB):- database_check(clause_asserted,HB).


% ========================================
% Rescan for consistency
% ========================================
:-dynamic_multifile_exported((rescan_dbase_facts/0, rescan_duplicated_facts/0, rerun_database_hooks/0 , gather_fact_heads/2)).

rescan_dbase_facts:-!.
rescan_dbase_facts:-  loop_check_local(rescan_dbase_facts_local).

rescan_dbase_facts_local:-with_no_assertions(thglobal:use_cyc_database,(must_det(rescan_duplicated_facts),must_det(rerun_database_hooks))).

rescan_duplicated_facts:- !, notrace( forall(member(M,[moo,user,world,hook]), forall((predicate_property(M:H,dynamic),mpred_arity(F,A),functor(H,F,A)), rescan_duplicated_facts(M,H)))).
rescan_duplicated_facts(_M,_H):-!.
rescan_duplicated_facts(M,H):-!,rescan_duplicated_facts(M,H,true).
rescan_duplicated_facts(M,H):-findall(H,(clause_safe(M:H,B),B==true),CF1), once((list_to_set(CF1,CF2),reduce_fact_heads(M,H,CF1,CF2))).
rescan_duplicated_facts(M,H,BB):-notrace(doall((gather_fact_heads(M,H),BB=true,once((findall(C,(clause_safe(H,B),B=@=BB,reduce_clause((H:-B),C)),CF1),
                                                                     list_to_set(CF1,CF2),once(reduce_fact_heads(M,H,CF1,CF2))))))).
rerun_database_hooks:-!.
rerun_database_hooks:-time_call(doall((gather_fact_heads(_M,H),forall(is_asserted(H),run_database_hooks(assert(z),H))))),fail.
rerun_database_hooks:-time_call(doall((is_asserted(mudSubclass(I,C)),run_database_hooks(assert(z),mudSubclass(I,C))))),fail.
rerun_database_hooks:-time_call(doall((isa_asserted(I,C),run_database_hooks(assert(z),mudIsa(I,C))))),fail.

reduce_fact_heads(_M,_H,CF1,CF1):-!. % no change
reduce_fact_heads(M,H,CF1,CF2):- 
 ignore((
   predicate_property(M:H,dynamic),
   length(CF1,F1),length(CF2,F2),
   dmsg(reduce_fact_heads(M,H,from(F1,F2))),
   retractall(M:H),
   % forall(member(C,CF1),retractall(M:C)),
   forall(member(C,CF2),assertz(M:C)))).

gather_fact_heads(M,H):- (nonvar(M)->true; member(M,[dbase,moo,world,user,hook])), current_predicate(M:F/A), mpred_arity(F,A),
  once((once((A>0,atom(F),F\=(:),var(H), debugOnError(functor_catch(H,F,A)))),compound(H),predicate_property(M:H,number_of_clauses(_)),
  not((arg(_,vv(system,bugger,logicmoo_util_dcg,user),M);predicate_property(M:H,imported_from(_)))))).
