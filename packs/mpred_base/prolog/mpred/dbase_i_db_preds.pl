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

% :- registerCycPredPlus2([genlPreds/4,genlInverse/4,localityOfObject/4]).

% ========================================
% Logic Preds Shared
% ========================================

:-export(is_svo_functor/1).
is_svo_functor(Prop):- notrace((atom(Prop),arg(_,svo(svo,prop,valueOf,rdf),Prop))).

:-export(hilog_functor/1).
hilog_functor(dbase_ttttt).

:-export(is_holds_true_not_hilog/1).
is_holds_true_not_hilog(HOFDS):-is_holds_true(HOFDS),\+ hilog_functor(HOFDS).

:-export(is_holds_true/1).
is_holds_true(Prop):- notrace((atom(Prop),is_holds_true0(Prop))),!.

% k,p,..
is_holds_true0(Prop):-arg(_,vvv(holds,holds_t,dbase_t,asserted_dbase_t,assertion_t,assertion,secondOrder,firstOrder),Prop).
is_holds_true0(Prop):-atom_concat(_,'_t',Prop).

:-export(is_2nd_order_holds/1).
is_2nd_order_holds(Prop):- is_holds_true(Prop) ; is_holds_false(Prop).

:-export(is_holds_false/1).
is_holds_false(Prop):-notrace((atom(Prop),is_holds_false0(Prop))).

is_holds_false0(Prop):-member(Prop,[not,nholds,holds_f,dbase_f,aint,assertion_f,asserted_dbase_f,retraction,not_secondOrder,not_firstOrder]).
is_holds_false0(Prop,Stem):-atom_concat('not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat('int_not_',Stem,Prop).
is_holds_false0(Prop,Stem):-atom_concat(Stem,'_f',Prop).
%is_holds_false0(Prop):-is_holds_false0(Prop,Stem),is_holds_true0(Stem).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_not',Prop).
%is_holds_false0(Prop,Stem):-atom_concat(Stem,'_false',Prop).


:- thread_local thlocal:override_hilog/1.

current_hilog(Dbase_t):- thlocal:override_hilog(Dbase_t),!.
current_hilog(dbase_t).



logical_functor_ft(F):-is_logical_functor(F).
logical_functor_ft((':-')).
logical_functor_ft((',')).



:- dynamic(non_assertable/1).
non_assertable(WW,isVar(WW)):- var(WW),!.
non_assertable(_:WW,Why):- !,non_assertable(WW,Why).
non_assertable(WW,notAssertable(Why)):- compound(WW),get_functor(WW,F),user:mpred_prop(F,notAssertable(Why)),!.
% non_assertable(WW,Why):- db_prop_add

is_logical_functor(And):-notrace(is_logical_functor0(And)).
is_logical_functor0(X):-atom(X),member(X,[',',';',xor,'\\+',neg]).
is_logical_functor0(X):-call_if_defined(logical_functor_pttp(X)).
is_logical_functor0(And):-member(And,[(,),(;),('<='),('=>'),('<=>'),(':-'),(and),nop]).



correct_negations(Op,(~({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(-({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(not({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(notz({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(assertable_not({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).
correct_negations(Op,(\+({X})),O):-nonvar(X),wrap_in_neg_functor(Op,X,O).

wrap_in_neg_functor(clause,X,assertable_neg(X)).
wrap_in_neg_functor(mpred,X,not(X)).
wrap_in_neg_functor(callable,X, (\+(X))).


:-export(infix_op/2).
infix_op(Op,_):-comparitiveOp(Op).
infix_op(Op,_):-additiveOp(Op).

:-export(comparitiveOp/1).
comparitiveOp((\=)).
comparitiveOp((\==)).
comparitiveOp((=)).
comparitiveOp((=:=)).
comparitiveOp((==)).
comparitiveOp((<)).
comparitiveOp((>)).
comparitiveOp((=<)).
comparitiveOp((>=)).

:-export(additiveOp/1).
additiveOp((is)).
additiveOp((*)).
additiveOp(+).
additiveOp(-).
additiveOp((/)).






/** <module> dbase_i_mpred_dbase_t
% Provides a prolog dabase in these predicates...
%
%  dbase_t/N
%  hybridRule/2
%  
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- include(dbase_i_header).


:- meta_predicate(tf_result(0,+)).
tf_result(Call,TF):-(Call->TF=true;TF=fail).
:- meta_predicate(if_result(0,0)).
if_result(TF,Call):-(TF->Call;true).

% ========================================
% is_holds_true/is_holds_false
% ========================================

:- dbase_mod(M),export((
          % M:dbase_t/1,
          M:dbase_t/2,
          M:dbase_t/3,
          M:dbase_t/4,
          M:dbase_t/5,
          M:dbase_t/6,
          M:dbase_t/7,
          M:dbase_t/8,
          M:dbase_t/9,
          M:dbase_t/10,
          M:dbase_t/11)).

:- dbase_mod(M),export((
          % M:holds_t/1,
          M:holds_t/2,
          M:holds_t/3,
          M:holds_t/4,
          M:holds_t/5,
          M:holds_t/6,
          M:holds_t/7,
          M:holds_t/8,
          M:holds_t/9,
          M:holds_t/10,
          M:holds_t/11)).


:-export((dbase_t/1,hasInstance/2)).
:- dynamic((
         % dbase_t/1,
          dbase_t/2,
          dbase_t/3,
          dbase_t/4,
          dbase_t/5,
          dbase_t/6,
          dbase_t/7,
          dbase_t/8,
          dbase_t/9,
          dbase_t/10,
          dbase_t/11,
        %  asserted_dbase_t/1,
          asserted_dbase_t/2,
          asserted_dbase_t/3,
          asserted_dbase_t/4,
          asserted_dbase_t/5,
          asserted_dbase_t/6,
          asserted_dbase_t/7,
          assertion_f/1,
          assertion_t/1,
        %  asserted_dbase_f/1,
          asserted_dbase_f/2,
          asserted_dbase_f/3,
          asserted_dbase_f/4,
          asserted_dbase_f/5,
          asserted_dbase_f/6,
          asserted_dbase_f/7,
         % dbase_f/1,
          dbase_f/2,
          dbase_f/3,
          dbase_f/4,
          dbase_f/5,
          dbase_f/6,
          dbase_f/7)).


:-export(into_plist/2).
into_plist(In,Out):-into_plist_arities(2,12,In,Out).

:-export(into_plist_arities/4).
into_plist_arities(Min,Max,PLIST,PLISTO):- var(PLIST),!,between(Min,Max,X),length(PLIST,X),PLISTO=PLIST.
into_plist_arities(_,_,[P|LIST],[P|LIST]):-var(P),!.
into_plist_arities(_,_,[dbase_t|PLIST],PLIST):-!.  % dbase_t is our versuion of '$holds' or call/N
into_plist_arities(_,_,plist(P,LIST),[P|LIST]):-!.
into_plist_arities(_,_,Call,PLIST):-Call=..PLIST. % finally the fallthrue


never_dbase_mpred(user:mpred_prop).
never_dbase_mpred(isa).
never_dbase_mpred(arity).


% ================================================================================
% begin holds_t
% ================================================================================

:-dynamic dbase_t/2.
% dbase_t(C,I):- trace_or_throw(dbase_t(C,I)),hasInstance(C,I). % ,fail,loop_check_term(isa_backchaing(I,C),hasInstance(C,I),fail).

%dbase_t([P|LIST]):- !,dbase_plist_t(P,LIST).
%dbase_t(naf(CALL)):-!,not(dbase_t(CALL)).
%dbase_t(not(CALL)):-!,dbase_f(CALL).
dbase_t(CALL):- into_plist_arities(3,10,CALL,[P|LIST]),dbase_plist_t(P,LIST).
dbase_plist_t(P,[]):-!,dbase_t(P).
dbase_plist_t(P,LIST):-var(P),!,is_list(LIST),CALL=..[dbase_t,P|LIST],debugOnError((CALL)).
dbase_plist_t(dbase_t,[P|LIST]):-!, dbase_plist_t(P,LIST).
dbase_plist_t(user:mpred_prop,[C,A,I]):-!,ground(I:C),user:mpred_prop(C,A,I).
dbase_plist_t(isa,[I,C]):-!,hasInstance(C,I).
dbase_plist_t(P,_):-never_dbase_mpred(P),!,fail.
dbase_plist_t(P,[L|IST]):-is_holds_true(P),!,dbase_plist_t(L,IST).
dbase_plist_t(P,LIST):-is_holds_false(P),!,dbase_f(LIST).
dbase_plist_t(P,LIST):- CALL=..[dbase_t,P|LIST],debugOnError(CALL).

% loop_check_mpred(Call):- current_predicate(ireq/1), loop_check_term(ireq(Call),loop_check_mpred(Call),fail).
loop_check_mpred(Call):- !, fail,not(thlocal:infInstanceOnly(_)),loop_check_local(ireq(Call),loop_check_mpred(Call),fail).
% loop_check_mpred(Call):-loop_check_local(mpred_call(dbase_t,Call),fail).

dbase_t(P,A1,A2):- loop_check_mpred(dbase_t(P,A1,A2)).
dbase_t(P,A1,A2):- mpred_pa_call(P,2,call(P,A1,A2)).
dbase_t(P,A1,A2,A3):- loop_check_mpred(dbase_t(P,A1,A2,A3)).
dbase_t(P,A1,A2,A3):- mpred_pa_call(P,3,call(P,A1,A2,A3)).
dbase_t(P,A1,A2,A3,A4):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4)).
dbase_t(P,A1,A2,A3,A4):- mpred_pa_call(P,4,call(P,A1,A2,A3,A4)).
dbase_t(P,A1,A2,A3,A4,A5):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5)).
dbase_t(P,A1,A2,A3,A4,A5):- mpred_pa_call(P,5,call(P,A1,A2,A3,A4,A5)).
dbase_t(P,A1,A2,A3,A4,A5,A6):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5,A6)).
dbase_t(P,A1,A2,A3,A4,A5,A6):- mpred_pa_call(P,6,call(P,A1,A2,A3,A4,A5,A6)).
dbase_t(P,A1,A2,A3,A4,A5,A6,A7):- loop_check_mpred(dbase_t(P,A1,A2,A3,A4,A5,A6,A7)).
dbase_t(P,A1,A2,A3,A4,A5,A6,A7):- mpred_pa_call(P,7,call(P,A1,A2,A3,A4,A5,A6,A7)).

mpred_pa_call(F,A,Call):-arity(F,A),current_predicate(F/A),call(Call).

mpred_fact_arity(F,A):-arity(F,A),once(mpred_prop(F,prologHybrid);mpred_prop(F,pfcControlled);mpred_prop(F,prologPTTP);mpred_prop(F,prologSNARK)).

prologHybridFact(G):- (var(G)->(mpred_fact_arity(F,A),functor(G,F,A));true),into_mpred_form(G,M),!,no_repeats(mpred_call(M)).

isCycPredArity_ignoreable(F,A):- ignore(user:mpred_prop(F,cycPred(A))),ignore(arity(F,A)).

which_t(dac(d,a_notnow,c,no_fallback)).

holds_t(P,A1,A2,A3,A4,A5,A6,A7):- isCycPredArity_ignoreable(P,7),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6,A7);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,A7,_,_);assertion_t([P,A1,A2,A3,A4,A5,A6,A7])).
holds_t(P,A1,A2,A3,A4,A5,A6):- isCycPredArity_ignoreable(P,6),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5,A6);call_mt_t(DBS,P,A1,A2,A3,A4,A5,A6,_,_)).
holds_t(P,A1,A2,A3,A4,A5):- isCycPredArity_ignoreable(P,5),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4,A5);call_mt_t(DBS,P,A1,A2,A3,A4,A5,_,_)).
holds_t(P,A1,A2,A3,A4):- isCycPredArity_ignoreable(P,4),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3,A4);call_mt_t(DBS,P,A1,A2,A3,A4,_,_)).
holds_t(P,A1,A2,A3):- isCycPredArity_ignoreable(P,3),which_t(DBS),(call_which_t(DBS,P,A1,A2,A3);call_mt_t(DBS,P,A1,A2,A3,_,_)).
%holds_t(P,A1,A2):- hotrace(holds_relaxed_t(P,A1,A2)).
holds_t(P,A1,A2):- isCycPredArity_ignoreable(P,2),which_t(DBS),(call_which_t(DBS,P,A1,A2);call_mt_t(DBS,P,A1,A2,_,_)).
holds_t(P,A1):- isCycPredArity_ignoreable(P,1),which_t(DBS),(call_which_t(DBS,P,A1);call_mt_t(DBS,P,A1,_,_)).


% holds_relaxed_t(P,A1,A2):-var(A1),var(A2),!,dbase_t(P,A1,A2).
holds_relaxed_t(P,A1,A2):-
  isCycPredArity_ignoreable(P,2),which_t(DBS),
      relax_term(P,PR,A1,R1,A2,R2),
         holds_relaxed_0_t(DBS,PR,R1,R2).

holds_relaxed_0_t(DBS,P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).

/*
holds_relaxed_0_t(dac(_,a,_,_),P,A1,A2):- assertion_t([P,A1,A2]).
holds_relaxed_0_t(dac(d,_,_,_),P,A1,A2):- dbase_t(P,A1,A2).
holds_relaxed_0_t(dac(_,_,_,h),P,A1,A2):- call_which_t(DBS,P,A1,A2).
holds_relaxed_0_t(DBS,P,A1,A2):- call_mt_t(DBS,P,A1,A2,_,_).
holds_relaxed_0_t(_DBS,P,A1,A2):- ground((P,A1)), TEMPL=..[P,T1,_],dbase_t(argSingleValueDefault,TEMPL,2,A2),req(isa(A1,T1)),!.
*/

holds_t([AH,P|LIST]):- is_holds_true(AH),!,holds_plist_t(P,LIST).
holds_t([AH,P|LIST]):- is_holds_false(AH),!,holds_f_p2(P,LIST).
holds_t([P|LIST]):- !,holds_plist_t(P,LIST).
holds_t(not(CALL)):- !, holds_f(CALL).
holds_t(CALL):- '=..'(CALL,PLIST),holds_t(PLIST).

holds_plist_t(P,LIST):- apply(holds_t,[P|LIST]).

% ========================================
% decl_mpred_hybrid/1/2/3
% ========================================

:-op(0,fx,((decl_mpred_hybrid))).
:-export((decl_mpred_hybrid)/1).
:-export((decl_mpred_hybrid)/2).
:-export((decl_mpred_hybrid_ilc)/3).
:-export((decl_mpred_hybrid4)/4).

:-op(0,fx,decl_mpred_hybrid).

:-meta_predicate_transparent(decl_mpred_hybrid(+)).
:-meta_predicate_transparent(decl_mpred_hybrid(+,+)).
:-meta_predicate_transparent(decl_mpred_hybrid(+,+,+)).


decl_mpred_hybrid(M):- must(with_pi(M,decl_mpred_hybrid4)).


decl_mpred_hybrid(F,A):- integer(A),!,decl_mpred_hybrid(F/A).
decl_mpred_hybrid(F,Other):- 
     decl_mpred(F,Other),
     get_functor(F,F0),
     must(arity(F0,A)),
     decl_mpred_hybrid(F0/A).

:-export(decl_mpred_hybrid/3).
decl_mpred_hybrid3(M, F, A):- atom(F),integer(A),!,must((functor(PI,F,A),decl_mpred_hybrid3(M,PI,F/A))).
decl_mpred_hybrid3(M,PI,FA):- loop_check(must(decl_mpred_hybrid_ilc(M,PI,FA)),true).


decl_mpred_hybrid_ilc(M,F,F/0):-
    arity(F,A),!,
    must((functor(PI,F,A),
    decl_mpred_hybrid_ilc(M,PI,F/A))).

decl_mpred_hybrid_ilc(M,PI,F/A):-
     must(not(user:mpred_prop(F,prologOnly))),
     assert_arity(F,A),
     must(arity(F,A)),
     must(M=user),
     decl_mpred_mfa(M,F,A),
     decl_mpred_pi(PI),
     must(user:provide_mpred_setup(call(conjecture),F/A,prologHybrid,_OUT)).
     

decl_mpred_hybrid4(_CM,M,PI,F/A):-
   must(decl_mpred_hybrid3(M,PI,F/A)).
decl_mpred_hybrid4(_CM,M,PI,FA):- dtrace,decl_mpred_hybrid3(M,PI,FA).

:-op(1150,fx,decl_mpred_hybrid).







% ================================================
% Naming System
% ================================================
:-export(create_meta/4).
% if SuggestedName was 'food666' it'd like the SuggestedClass to be 'food' and the stystem name will remain 'food666'
% if SuggestedName was 'food' it'd like the SuggestedClass to be 'food' and the stystem name will become a gensym like 'food1'
create_meta(SuggestedName,SuggestedClass,BaseClass,SystemName):-
   must_det(split_name_type(SuggestedName,SystemName,NewSuggestedClass)),
   ignore(SuggestedClass=NewSuggestedClass),   
   assert_subclass_safe(SuggestedClass,BaseClass),
   assert_subclass_safe(NewSuggestedClass,BaseClass),
   assert_isa_safe(SystemName,BaseClass),
   assert_isa_safe(SystemName,NewSuggestedClass),
   assert_isa_safe(SystemName,SuggestedClass).

toUpperCamelcase(Type,TypeUC):-toCamelcase(Type,TypeC),toPropercase(TypeC,TypeUC),!.
:-export(i_name/2).
i_name(OType,IType):-typename_to_iname0('',OType,IOType),!,IOType=IType.
:-export(i_name/3).
i_name(I,OType,IType):-typename_to_iname0(I,OType,IOType),!,IOType=IType.

typename_to_iname0(I,OType,IType):-type_prefix(Prefix,_),atom_concat(Prefix,Type,OType),capitalized(Type),!,typename_to_iname0(I,Type,IType).
typename_to_iname0(I,Type,IType):-nonvar(Type),toUpperCamelcase(Type,UType),atom_concat(I,UType,IType).

:-export(split_name_type/3).
:- '$hide'(split_name_type/3).
split_name_type(Suggest,InstName,Type):- must_det(split_name_type_0(Suggest,NewInstName,NewType)),!,must((NewInstName=InstName,NewType=Type)),!.

split_name_type_0(S,P,C):- string(S),!,atom_string(A,S),split_name_type_0(A,P,C),!.
split_name_type_0(FT,FT,ttFormatType):-hasInstance(ttFormatType,FT),!,dmsg(trace_or_throw(ttFormatType(FT))),fail.
split_name_type_0(T,T,C):- compound(T),functor(T,C,_),!.
split_name_type_0(T,T,C):- notrace((once(atomic_list_concat_safe([CO,'-'|_],T)),atom_string(C,CO))).
split_name_type_0(T,T,C):- notrace((atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catchv(number_codes(_,Digits),_,fail),atom_codes(CC,Type),!,i_name(t,CC,C))).
split_name_type_0(C,P,C):- var(P),atom(C),i_name(i,C,I),gensym(I,P),!.




% =======================================================
% term utils
% =======================================================

:-export(inverse_args/2).
inverse_args([AR,GS],[GS,AR]):-!.
inverse_args([AR,G,S],[S,G,AR]):-!.
inverse_args([A,R,G,S],[S,R,G,A]):-!.
inverse_args([P,A,R,G,S],[S,A,R,G,P]):-!.

:-export(same_vars/2).
same_vars(T1,T2):-term_variables(T1,V1),term_variables(T2,V2),!,V1==V2.


:-moo_hide_childs(replace_arg/4).
replace_arg(C,A,OLD,CC):- 
   C=..FARGS,
   replace_nth(FARGS,A,OLD,FARGO),!,
   CC=..FARGO.

:-moo_hide_childs(replace_nth/4).
replace_nth([],_,_,[]):- !.
replace_nth([_|ARGO],0,OLD,[OLD|ARGO]):- !.
replace_nth([T|FARGS],A,OLD,[T|FARGO]):- 
    A2 is A-1,replace_nth(FARGS,A2,OLD,FARGO).


replace_nth([],_N,_OldVar,_NewVar,[]):- !,trace_or_throw(missed_the_boat).
replace_nth([OldVar|ARGS],1,OldVar,NewVar,[NewVar|ARGS]):- !.
replace_nth([Carry|ARGS],Which,OldVar,NewVar,[Carry|NEWARGS]):- 
 Which1 is Which-1,
 replace_nth(ARGS,Which1,OldVar,NewVar,NEWARGS),!.


:-moo_hide_childs(update_value/4).
update_value(OLD,NEW,NEXT):- var(NEW),!,trace_or_throw(logicmoo_bug(update_value(OLD,NEW,NEXT))).
update_value(OLD,NEW,NEWV):- var(OLD),!,compute_value_no_dice(NEW,NEWV).
update_value(OLD,X,NEW):- is_list(OLD),!,list_update_op(OLD,X,NEW),!.
update_value(OLDI,+X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(OLDI,-X,NEW):- compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD - X,_,fail),!.
update_value(OLDI,X,NEW):- number(X),X<0,compute_value(OLDI,OLD),number(OLD),catch(NEW is OLD + X,_,fail),!.
update_value(_,NEW,NEWV):-compute_value_no_dice(NEW,NEWV),!.


list_update_op(OLDI,+X,NEW):-flatten_append(OLDI,X,NEW),!.
list_update_op(OLDI,-X,NEW):-flatten([OLDI],OLD),flatten([X],XX),!,list_difference_eq(OLD,XX,NEW),!.

compute_value_no_dice(NEW,NEW):- compound(NEW),functor_catch(NEW,ftDice,_),!.
compute_value_no_dice(NEW,NEWV):-compute_value(NEW,NEWV).

compute_value(NEW,NEWV):-catch(NEWV is NEW,_,fail),!.
compute_value(NEW,NEWV):-catch(any_to_value(NEW,NEWV),_,fail),!.
compute_value(NEW,NEW).

insert_into(ARGS,0,Insert,[Insert|ARGS]):- !.
insert_into([Carry|ARGS],After,Insert,[Carry|NEWARGS]):- 
   After1 is After - 1,
   insert_into(ARGS,After1,Insert,NEWARGS).


add_arg_parts_of_speech(_F,_N,[],[]).
add_arg_parts_of_speech(F,N,[A|ARGS0],[ARG|ARGS]):-argIsa_call_or_undressed(F,N,A,ARG),N1 is N+1, add_arg_parts_of_speech(F,N1,ARGS0,ARGS).

argIsa_call_or_undressed(F,N,Obj,fN(Obj,Type)):- argIsa_call_0(F,N,Type),!.
argIsa_call_or_undressed(_F,_N,Obj,Obj).

verb_after_arg(_,_,1).


