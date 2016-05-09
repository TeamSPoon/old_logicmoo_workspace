/* <module>
% =============================================
% File 'system_base.pfc'
% Purpose: Agent Reactivity for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface' 1.0.0
% Revision: $Revision: 1.9 $
% Revised At: $Date: 2002/06/27 14:13:20 $
% =============================================
%
%  PFC is a language extension for prolog.. there is so much that can be done in this language extension to Prolog
%
%
% props(Obj,[height(ObjHt)]) == t(height,Obj,ObjHt) == rdf(Obj,height,ObjHt) == t(height(Obj,ObjHt)).
% pain(Obj,[height(ObjHt)]) == prop_set(height,Obj,ObjHt,...) == ain(height(Obj,ObjHt))
% [pdel/pclr](Obj,[height(ObjHt)]) == [del/clr](height,Obj,ObjHt) == [del/clr]svo(Obj,height,ObjHt) == [del/clr](height(Obj,ObjHt))
% keraseall(AnyTerm).
%
%                      ANTECEEDANT                                   CONSEQUENT
%
%         P =         test nesc true                         assert(P),retract(~P) , enable(P).
%       ~ P =         test nesc false                        assert(~P),retract(P), disable(P)
%
%   ~ ~(P) =         test possible (via not impossible)      retract( ~(P)), enable(P).
%  \+ ~(P) =         test impossiblity is unknown            retract( ~(P))
%   ~ \+(P) =        same as P                               same as P
%     \+(P) =        test naf(P)                             retract(P)
%
% Dec 13, 2035
% Douglas Miles
*/

:- '$set_source_module'(baseKB).

:- file_begin(pfc).

:- set_fileAssertMt(baseKB).

% catching of misinterpreations

:- dynamic(mpred_mark/3).

mpred_mark(pfcPosTrigger,F,A)==>{warn_if_static(F,A)}.
mpred_mark(pfcNegTrigger,F,A)==>{warn_if_static(F,A)}.
mpred_mark(pfcBcTrigger,F,A)==>{warn_if_static(F,A)}.


:- dynamic(marker_supported/2).

:- dynamic(pass2/0).



:- dynamic(mpred_mark_C/1).

:- kb_dynamic(tCol/1).

:- kb_dynamic(subFormat/2).

:- kb_dynamic(singleValuedInArg/2).
:- kb_dynamic(ptReformulatorDirectivePredicate/1).
:- kb_dynamic(support_hilog/2).
:- kb_dynamic(mpred_undo_sys/3).
:- kb_dynamic(arity/2).

:- dynamic(arity/2).
% prologHybrid(arity/2).

arity(apathFn,2).
arity(isKappaFn,2).
arity('$VAR',1).
arity(isInstFn,1).
arity(ftListFn,1).
arity(xyzFn,4).
arity(arity,2).
arity(is_never_type,1).
arity(argIsa, 3).
arity(Prop,1):- cwc, ttPredType(Prop).
arity(meta_argtypes,1).
arity(arity,2).
arity(is_never_type,1).
arity(prologSingleValued,1).
arity('<=>',2).
arity(F,A):- cwc, is_ftNameArity(F,A), current_predicate(F/A),A>1.
arity(F,1):- cwc, is_ftNameArity(F,1), current_predicate(F/1),\+((dif:dif(Z,1), arity(F,Z))).

arity(tSet,1).
arity(argsQuoted,1).
arity(quasiQuote,1).




% this mean to leave terms at EL:  foo('QuoteFn'([cant,touch,me])).

quasiQuote('QuoteFn').

argsQuoted('loop_check_term').
argsQuoted('loop_check_term_key').
argsQuoted('QuoteFn').
argsQuoted('$VAR').

argsQuoted(ain).
argsQuoted(meta_argtypes).
argsQuoted(ttFormated).
argsQuoted(ruleRewrite).
argsQuoted(mpred_action).
argsQuoted(ain).
argsQuoted(mpred_rem).
argsQuoted(added).
argsQuoted(call).
argsQuoted(call_u).
argsQuoted(member).
argsQuoted(=..).
argsQuoted({}).
argsQuoted(second_order).
% argsQuoted((':-')).

nondet.

predicateConventionMt(genlMt,baseKB).
predicateConventionMt(regression_test,lmconf).

% mtExact(Mt)==>{kb_dynamic(Mt)}.



tSet(mtGlobal,comment("mtGlobal(?Mt) states the Mt is always findable during inheritance")).
mtGlobal(baseKB).
mtGlobal(lmcode).
mtGlobal(system).

tCol(tSet).  % = isa(tSet,tCol).

tSet(mtExact,
  comment("mtExact(?Mt) states that all predicates the Mt specified should not inherit past ?Mt.  Thus:  mtExact(Mt)==> ~genlMt(Mt,_)")).
mtExact(lmconf).
mtExact(lmcache).
mtExact(t_l).
mtExact(system).
mtExact(Mt)==> mtGlobal(Mt).
mtExact(Mt)==> ~genlMt(Mt,_).

tSet(mtCore,comment("mtCore(?Mt) states Mt specified is builtin")).
mtCore(user).
mtCore(everythingPCS).
mtCore(inferencePCS).
mtCore(Mt)==>tMicrotheory(Mt).

/*
{module_property(Mt,class(user)),
   (atom_concat('common_logic_',_,Mt);atom_concat('logicmoo_util_',_,Mt);atom_concat('mpred_',_,Mt))} 
    ==>  mtGlobal(Mt).

*/

{module_property(Mt,class(library))} ==> mtGlobal(Mt).

mtGlobal(Mt)==>(mtCore(Mt),~mtLocal(Mt)).

(tMicrotheory(Mt), ~ mtCore(Mt)) <==> mtLocal(Mt).

(genlMt(Mt,baseKB)/(Mt \==baseKB ), \+ mtCore(Mt)) ==> mtLocal(Mt).

mtLocal(Mt)==>{skip_user(Mt),set_prolog_flag(Mt:unknown,warning)},genlMt(Mt,baseKB).
mtGlobal(Mt)==>genlMt(baseKB,Mt).

baseKB:isRegisteredCycPred(apply,maplist,3).


(genlMt(Child,Parent), \+ mtCore(Child)) ==>
   {ignore((system:delete_import_module(Parent,user))),
    ignore((system:delete_import_module(Parent,Child))),
    system:add_import_module(Child,Parent,start)}.


:- dynamic(baseKB:isRegisteredCycPred/3).

({fail,current_module(Mt),
   predicate_property(Mt:P,defined), 
 \+ predicate_property(Mt:P,imported_from(_)),
 functor(P,F,A)})
  ==>baseKB:isRegisteredCycPred(Mt,F,A).


/* prolog_listing:listing */
% :- printAll(baseKB:isRegisteredCycPred/3).

% ~(tCol({})).

:- unload_file(library(yall)).


((prologBuiltin(P)/get_arity(P,F,A),arity(F,A))==>{make_builtin(F/A)}).


meta_argtypes(support_hilog(tRelation,ftInt)).

:- ain(((tPred(F),arity(F,A)/(is_ftNameArity(F,A),A>1), ~prologBuiltin(F)) ==> (~(tCol(F)),support_hilog(F,A)))).

:- kb_dynamic(support_hilog/2).

(((support_hilog(F,A)/(F\='$VAR',is_ftNameArity(F,A),\+ is_static_predicate(F/A), \+ prologDynamic(F)))) ==>
   (hybrid_support(F,A), 
    {% functor(Head,F,A) ,Head=..[F|TTs], TT=..[t,F|TTs],
    %  (CL = (Head :- cwc, call(second_order(TT,CuttedCall)), ((CuttedCall=(C1,!,C2)) -> (C1,!,C2);CuttedCall)))
    CL = arity(F,A)
    },
   (CL))).



%:- kb_dynamic(hybrid_support/2).
%prologBuiltin(resolveConflict/1).

:- dynamic(bt/2).
bt(P,_)==> (P:- mpred_bc_only(P)).

((prologHybrid(F),arity(F,A)/is_ftNameArity(F,A))==>hybrid_support(F,A)).
(hybrid_support(F,A)/is_ftNameArity(F,A))==>prologHybrid(F),arity(F,A).

((mpred_mark(pfcRHS,F,A)/(A\=0)) ==> {kb_dynamic(F/A)}).
% ((mpred_mark(_,F,A)/(A\=0)) ==> {shared_multifile(F/A)}).


(pass2,pfcControlled(X)/get_pifunctor(X,C))==>({kb_dynamic(C),get_functor(C,F,A)},arity(F,A),pfcControlled(F),support_hilog(F,A)).
%pfcControlled(X)/get_pifunctor(X,C)==>({shared_multifile(C),get_functor(C,F,A)},arity(F,A),pfcControlled(F),support_hilog(F,A)).

(pass2,prologHybrid(X)/get_pifunctor(X,C))==>({\+ is_static_predicate(C), shared_multifile(C),get_functor(C,F,A)},arity(F,A),prologHybrid(F)).
%prologHybrid(X)/get_pifunctor(X,C)==>({\+ is_static_predicate(C), kb_dynamic(C),get_functor(C,F,A)},arity(F,A),prologHybrid(F)).


(pass2,prologBuiltin(X)/get_pifunctor(X,C))==>({decl_mpred_prolog(C),get_functor(C,F,A)},arity(F,A),prologBuiltin(F)).

% prologDynamic(X)/get_pifunctor(X,C)==>({kb_dynamic(C),decl_mpred_prolog(C),get_functor(C,F,A)},arity(F,A),prologDynamic(F)).

isa(F,pfcMustFC) ==> pfcControlled(F).



pfcControlled(C)==>prologHybrid(C).

:- dynamic(hybrid_support/2).

mpred_mark(S1, F, A)/(ground(S1),is_ftNameArity(F,A))==>(tSet(S1),arity(F,A),isa(F,S1)).
mpred_mark(pfcPosTrigger,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcNegTrigger,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcBcTrigger,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcRHS,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcCreates,F, A)/(is_ftNameArity(F,A))==>{functor(P,F,A),make_dynamic(P)}.
mpred_mark(pfcCreates,F, A)/(is_ftNameArity(F,A))==>marker_supported(F,A).
mpred_mark(pfcCallCode,F, A)/((is_ftNameArity(F,A)), 
  predicate_is_undefined_fa(F,A))==> marker_supported(F,A).


% (marker_supported(F,A)/is_ftNameArity(F,A))==>(prologHybrid(F),hybrid_support(F,A)).
%mpred_mark(pfcPosTrigger,F,A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A),afterAdding(F,lambda(P,mpred_enqueue(P,(m,m)))).
%mpred_mark(pfcNegTrigger,F,A)/(integer(A),functor(P,F,A)) ==> pfcTriggered(F/A), afterRemoving(F,lambda(P,mpred_enqueue(~P,(m,m)))).

/*
mpred_mark(pfcRHSF,1)/(fail,atom(F),functor(Head,F,1), 
 \+ argsQuoted(F),
 \+ prologDynamic(F),
 \+ ~(tCol(F)),
 \+ specialFunctor(F),
 \+ predicate_property(Head,built_in))==>completelyAssertedCollection(F).
*/
% mpred_mark(Type,F,A)/(integer(A),A>1,F\==arity,Assert=..[Type,F])==>arity(F,A),Assert.

mpred_mark_C(G) ==> {map_mpred_mark_C(G)}.
map_mpred_mark_C(G) :-  map_literals(lambda(P,(get_functor(P,F,A),ain([isa(F,pfcControlled),arity(F,A)]))),G).
mpred_mark(pfcRHS,F,A)/(is_ftNameArity(F,A),F\==arity)==>tPred(F),arity(F,A),pfcControlled(F).

% (hybrid_support(F,A) ==>{\+ is_static_predicate(F/A), must(kb_dynamic(F/A))}).


%:- meta_predicate(mp_test_agr(?,+,-,*,^,:,0,1,5,9)).
%mp_test_agr(_,_,_,_,_,_,_,_,_,_).
%:- mpred_test(predicate_property(mp_test_agr(_,_,_,_,_,_,_,_,_,_),meta_predicate(_))).
% becomes         mp_test_agr(+,+,-,?,^,:,0,1,0,0)


((marker_supported(F,A)/is_ftNameArity(F,A),prologHybrid(F))==>hybrid_support(F,A)).
(hybrid_support(F,A) ==>{ must(kb_dynamic(F/A))}).


% :- with_ukb(baseKB,baseKB:ensure_mpred_file_loaded('system_common_tbox.pfc')).

