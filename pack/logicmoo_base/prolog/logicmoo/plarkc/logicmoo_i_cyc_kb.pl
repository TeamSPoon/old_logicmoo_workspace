/* 
% ===================================================================
% File 'logicmoo_i_cyc_kb.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
% special module hooks into the logicmoo engine allow
% syntax to be recocogized via our CycL/KIF handlers 
%
% Dec 13, 2035
% Douglas Miles
*/
%:- module(tiny_kb,['TINYKB-ASSERTION'/5, 'TINYKB-ASSERTION'/6]).
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/plarkc/logicmoo_i_cyc_kb.pl
/*
:- module(logicmoo_i_cyc_kb,
          [ logicmoo_i_cyc_xform/0,
            addCycL/1,
            addCycL0/1,
            addCycL1/1,
            addTinyCycL/1,
            addTiny_added/1,
            as_cycl/2,
            atom_concatM/3,
            atom_concatR/3,
            call_el_stub/3,
            cycLToMpred/2,
            cycLToMpred0/2,
            % baseKB:cycPrepending/2,
            cyc_to_clif/2,
            cyc_to_clif_notify/2,
            cyc_to_mpred_idiom/2,
            cyc_to_mpred_idiom1/2,
            cyc_to_mpred_idiom_unused/2,
            cyc_to_mpred_sent_idiom_2/3,
            %baseKB:cyc_to_plarkc/2,
            finish_asserts/0,
            expT/1,
            %lmcache:isCycAvailable_known/0,
            %lmcache:isCycUnavailable_known/1,
            isF/1,
            isFT/1,
            isPT/1,
            isRT/1,
            isV/1,
            isVT/1,
            is_better_backchained/1,
            is_simple_arg/1,
            is_simple_gaf/1,
            isa_db/2,
            ist_tiny/2,
            kw_to_vars/2,
            label_args/3,
            list_to_ops/3,
            loadTinyKB/0,
            ltkb1/0,
            ltkb1_complete/0,
            make_el_stub/4,
            make_functor_h/3,
            make_kw_functor/3,
            make_kw_functor/4,
            maybe_ruleRewrite/2,
            mpred_postpend_type/2,
            mpred_prepend_type/2,
         %   baseKB:mpred_to_cyc/2,
            mwkb1/0,
            needs_canoncalization/1,
            needs_indexing/1,
            notFormatType/1,
            print_assertion/3,
            sent_to_conseq/2,
            mtDressedMt/1,
            rtEscapeFunction/1,
            mtUndressedMt/1,
            tinyAssertion/3,
            tinyAssertion0/3,
            tinyKB/1,
            tinyKB/3,
            tinyKB1/1,
            tinyKB2/1,
            tinyKB_All/3,
            tinyKB_wstr/1,
            tiny_support/3,
            wkb0/0,
            wkb01/0,
            wkb02/0,
            wkb2/0,
            wkbe/0
          ]).
*/
:- set_prolog_flag(lm_expanders,false).

:- op(500,fx,'~').
:- op(1050,xfx,('==>')).
:- op(1050,xfx,'<==>').
:- op(1050,xfx,('<-')).
:- op(1100,fx,('==>')).
:- op(1150,xfx,('::::')).
:- 
 system:((
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'))).

:- dynamic(tinyKB9/1).
:- multifile(tinyKB9/1).
:- meta_predicate cwtdl(0,+,+).
:- meta_predicate transfer_predicate(?,0,0).
:- meta_predicate transTiny(?,0).

:- was_dynamic(cwtdl_failed/1).


cwtdl(Goal,DL,TL):- cwc,
  cnotrace((ignore((nortrace,
   (show_failure(why,catch(call_with_time_limit(TL,(((call_with_depth_limit(Goal,DL,DLE),DLE\==depth_limit_exceeded)))),E,(dmsg(E:cwtdl(Goal,DL,TL)),fail)))
     ->true;
    assert(cwtdl_failed(Goal))))))).


%:- in_cmt(doall((filematch(logicmoo('plarkc/mpred_cyc_kb_tinykb.pl'),F),source_file(X,F),predicate_property(X,static),X\='$pldoc'(_G8428,_G8429,_G8430,_G8431),listing(X)))).

% :- file_begin(pfc).

:- must_det(argIsa(genlPreds,2,_)).

transfer_predicate(C,If,Q):-doall((clause(C,true,Ref),If,Q,on_x_log_throw(erase(Ref)))).
transTiny(Template,If):-transfer_predicate(tinyK8(Template),If,once(ain(Template))).


reallyLoadTiny:- transTiny(tCol(X),ground(X)).
reallyLoadTiny:- transTiny(arity(X,Y),ground((X,Y))).
reallyLoadTiny:- transTiny(genls(X,Y),((X\=ftAtomicTerm,ground((X,Y))))).
reallyLoadTiny:- mpred_trace.
reallyLoadTiny:- transTiny(genls(X,Y),((ground((X,Y))))).
%TODO_VERIFY_STILL_UNNEEDED :- retract_all((ftClosedAtomicTerm(A) :- ftAtomicTerm(A))).
%TODO_VERIFY_STILL_UNNEEDED :- mpred_withdraw(genls(ftAtomicTerm,ftClosedAtomicTerm)).
reallyLoadTiny:- transTiny(genlMt(X,Y),writeq((X,Y))).
reallyLoadTiny:- transTiny(ttExpressionType(X),ground(X)).

%TODO_VERIFY_STILL_UNNEEDED :-mpred_withdraw(genls(ftAtomicTerm,ftClosedAtomicTerm)).

%TODO_VERIFY_STILL_UNNEEDED :-retract_all((ftClosedAtomicTerm(A) :- ftAtomicTerm(A))).
reallyLoadTiny:- mpred_notrace.


:- if(false).
:- doall(reallyLoadTiny).
:- endif.


%TODO FIX :-ain((((cycl(X),{must(cyc_to_clif(X,Y))}) ==> clif(Y)))).

:- mpred_notrace.
:- ain((((cycl('$VAR'('X')),{must(cyc_to_clif('$VAR'('X'),'$VAR'('Y')))}) ==> clif('$VAR'('Y'))))).

% ?-listing(cycl).

%TODO FIX :- must(isa(iExplorer2,tHominid)).
%TODO FIX :- must(tHominid(iExplorer2)).

tHominid(iExplorer2).



:- multifile(t/3).
:- multifile(t/4).
:- multifile(t/5).
:- multifile(t/6).
:- multifile(t/7).
:- multifile(t/8).

coreMt(X):-forward(_BaseKB,'CycInternalAnthropacity',isa,X,ID), \+ disabledAssertion(ID).
% coreMt(X):-forward('BaseKB','CycInternalAnthropacity',isa,X,ID), \+ disabledAssertion(ID).

:- meta_predicate freeze_pvars(*,0).
freeze_pvars( _ ,Goal):-!,call(Goal). 
freeze_pvars([ ],Goal):-!,call(Goal).
freeze_pvars([V],Goal):-!,freeze(V,Goal).
freeze_pvars([V|Vs],Goal):-freeze(V,freeze_pvars(Vs,Goal)).

% unt_ify(TGaf,PGaf):- \+ current_prolog_flag(logicmoo_untify,true),!,TGaf=PGaf.
unt_ify(TGaf,PGaf):- notrace(unt_ify_a(TGaf,PGaf)).

unt_ify_a(TGaf,PGaf):- compound(TGaf),functor(TGaf,UR,_),arg(_,v(uU,nartR),UR),!,TGaf=PGaf.
unt_ify_a(TGaf,PGaf):- term_variables(TGaf,TVars),
  freeze_pvars(TVars,unt_ify0(TGaf,PGaf)).
unt_ify0(TGaf,PGaf):- 
 \+ compound(TGaf)->PGaf=TGaf ;
  (is_list(TGaf) -> maplist(t_ify0,TGaf,PGaf);
    ((TGaf=..[t,P|TARGS]) -> (maplist(unt_ify,TARGS,ARGS)-> apply_term(P,ARGS,PGaf) ; PGaf=TGaf))).

% t_ify(PGaf,TGaf):- \+ current_prolog_flag(logicmoo_untify,true),!,TGaf=PGaf.
t_ify(PGaf,TGaf):- 
 notrace((term_variables(PGaf,PVars),
  freeze_pvars(PVars,t_ify0(PGaf,TGaf)))).

t_ify0(PGaf,TGaf):- 
 (\+ compound(PGaf)-> PGaf=TGaf;
  (is_list(PGaf) -> maplist(t_ify0,PGaf,TGaf);
    (PGaf=..[P|ARGS], (P==t -> PGaf=TGaf ; (maplist(t_ify0,ARGS,TARGS)->TGaf=..[t,P|TARGS]))))).

isT(X):- isT(_,X).

isAskableT(Gaf):- var(Gaf),!.
isAskableT(isT(_)):-!,fail.
isAskableT(call_u(_)):-!,fail.
isAskableT(Gaf):-compound(Gaf).

isT(Mt,Gaf):- isAskableT(Gaf), 
 ( \+ compound(Gaf)->
    (isTT(Mt,TGaf)*->(unt_ify(TGaf,Gaf)->true);fail) ;
    (t_ify(Gaf,TGaf)->isTT(Mt,TGaf))).

:- dynamic(disabledAssertion/1).

disabledAssertion(ID):-fbc(_,_,randomChars,_,ID).
disabledAssertion(ID):-fbc(_,_,_,randomChars,ID).
%disabledAssertion(ID):-fbc(_,isa,randomChars,_,ID).
%disabledAssertion(ID):-fbc(_,genls,randomChars,_,ID).


extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'tCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'tFirstOrderCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttSecondOrderCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,gens,B,C,ID),(A=B;A=C).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttVariedOrderCollection',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'tSetOrCollection',ID).

extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttCollectionType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttObjectType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttStuffType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttExistingStuffType',ID).
extra_tcol(Mt,A,ID):- fbc(Mt,isa,A,'ttAtemporalNecessarilyEssentialCollectionType',ID).

specFn(X,Y):-  relToOf(_,Y,X).
superFn(X,Y):-  relToOf(_,X,Y).


relToOf(P,X,Y):-nonvar(X),!,relToOf_v_U(P,X,Y). 
relToOf(P,X,Y):-nonvar(Y),!,chkRelToOf(P,X,Y).
relToOf(P,X,Y):-freeze(Y,relToOf(P,X,Y)).
relToOf_v_U(P,X,Y):-var(Y),!,freeze(X,relToOf(P,X,Y)),freeze(Y,relToOf(P,X,Y)).
relToOf_v_U(P,X,Y):-freeze(X,relToOf(P,X,Y)).

pat(P,A,B):-fbc(_Mt,P,A,B,ID),\+ notrace(disabledAssertion(ID)).

transbin(genlMt).
transbin(genls).

chkRelToOf(P,X,Y):-dif(X,Y),transbin(P),pat(P,X,RelTo2),(Y=RelTo2;(pat(P,RelTo2,RelTo3),(Y=RelTo3;pat(P,RelTo3,Y)))).

% extra_tcol(Mt,A,ID):- is(TT,(Mt,t(genls,A,Other),ID),atom(Other),Other\=A,'Thing'\=Other.
% extra_tcol(Mt,A,ID):- is(TT,(Mt,t(genls,Other,A),ID),atom(Other),Other\=A,'Thing'\=Other.

isTT(Mt,TGaf):- no_repeats(TGaf,((isTT(Mt,TGaf,ID), \+ disabledAssertion(ID)))).

isTT(Mt,t(P,A),ID):-fbc(Mt,P,A,ID).
isTT(Mt,t(tCol,A),ID):- extra_tcol(Mt,A,ID).
isTT(Mt,t(P,A,B),ID):-fbc(Mt,P,A,B,ID).
isTT(Mt,t(P,A,B,C),ID):-fbc(Mt,P,A,B,C,ID).
isTT(Mt,t(P,A,B,C,D),ID):-fbc(Mt,P,A,B,C,D,ID).
isTT(Mt,t(P,A,B,C,D,E),ID):-fbc(Mt,P,A,B,C,D,E,ID).
isTT(Mt,t(P,A,B,C,D,E,F),ID):-fbc(Mt,P,A,B,C,D,E,F,ID).
isTT(_,t(ist,Mt,PAB),ID):- nonvar(Mt),!,isTT(Mt,PAB,ID), \+ arg(1,PAB,ist).




fbc(Mt,P,A,B,ID):-forward(Mt,B,P,A,ID).
fbc(Mt,P,A,B,ID):-backward(Mt,B,P,A,ID).
fbc(Mt,P,A,B,ID):-code(Mt,B,P,A,ID).

fbc(Mt,P,A,ID):-forward(Mt,P,A,ID).
fbc(Mt,P,A,ID):-backward(Mt,P,A,ID).
fbc(Mt,P,A,ID):-code(Mt,P,A,ID).


fbc(Mt,P,A,B,C,ID):-forward(Mt,A,P,B,C,ID).
fbc(Mt,P,A,B,C,ID):-backward(Mt,A,P,B,C,ID).
fbc(Mt,P,A,B,C,ID):-code(Mt,A,P,B,C,ID).


fbc(Mt,P,A,B,C,D,ID):-forward(Mt,A,P,B,C,D,ID).
fbc(Mt,P,A,B,C,D,ID):-backward(Mt,A,P,B,C,D,ID).
fbc(Mt,P,A,B,C,D,ID):-code(Mt,A,P,B,C,D,ID).

fbc(Mt,P,A,B,C,D,E,ID):-forward(Mt,A,P,B,C,D,E,ID).
fbc(Mt,P,A,B,C,D,E,ID):-backward(Mt,A,P,B,C,D,E,ID).
fbc(Mt,P,A,B,C,D,E,ID):-code(Mt,A,P,B,C,D,E,ID).

fbc(Mt,P,A,B,C,D,E,F,ID):-forward(Mt,A,P,B,C,D,E,F,ID).
fbc(Mt,P,A,B,C,D,E,F,ID):-backward(Mt,A,P,B,C,D,E,F,ID).
fbc(Mt,P,A,B,C,D,E,F,ID):-code(Mt,A,P,B,C,D,E,F,ID).

makeRenames:- tell('renames.lisp'), forall(rename(A,B),makeRenameForCyc(B,A)),told.

makeRenameForCyc(B,A):- format('(csetq *const* (find-constant "~w")) (pwhen *const* (rename-constant *const* "~w"))~n',[B,A]).

:- dynamic(rename_rev/2).
:- multifile(rename_rev/2).
rename_rev('SetOrCollection',tSpec).
rename_rev('Collection',tCol).
rename_rev('CollectionType',ttTypeType).
rename_rev('SiblingDisjointCollectionType',tSet).
rename_rev('ObjectType',tSet).
rename_rev('ObjectType',ttValueType).
rename_rev('AspatialThing',vtValue).
rename_rev('RelationshipType',ttRelationType).
rename_rev('Predicate',tPred).
rename_rev('SubLExpressionType',ttExpressionType).

renames_all(C,P):-rename_rev(C,P).
renames_all(C,P):-rename(P,C).


% :- shared_multifile((  argGenl/3,argIsa/3,argQuotedIsa/3)).

:- dynamic((
  
        % argGenl/3,argIsa/3,argQuotedIsa/3,

        addTiny_added/1,
        baseKB:cycPrepending/2,
        baseKB:cyc_to_plarkc/2,
        lmcache:isCycUnavailable_known/1,
        baseKB:mpred_to_cyc/2)).

:- volatile(lmcache:isCycAvailable_known/0).

isa_db(I,C):-clause(isa(I,C),true).

:- dynamic((exactlyAssertedEL/4,exactlyAssertedEL/5,exactlyAssertedEL/6,exactlyAssertedEL/7)).
:- dynamic((exactlyAssertedEL_next/4,exactlyAssertedEL_next/5,exactlyAssertedEL_next/6,exactlyAssertedEL_next/7)).
:- dynamic((exactlyAssertedEL_first/4,exactlyAssertedEL_first/5,exactlyAssertedEL_first/6,exactlyAssertedEL_first/7)).
:- dynamic(assertedTinyKB_implies_first/4).
:- dynamic(assertedTinyKB_not_first/3).
:- dynamic((exactlyAssertedEL_first/5,exactlyAssertedEL_with_vars/5,exactlyAssertedEL_with_vars/6,assertedTinyKB_implies_Already/4)).

:- set_prolog_flag(lm_expanders,true).

:- dynamic(baseKB:cycPrepending/2).
baseKB:cycPrepending(ft,'Atom').
baseKB:cycPrepending(ft,'AtomicAssertion').
baseKB:cycPrepending(ft,'AtomicSentence').
baseKB:cycPrepending(ft,'AtomicTerm').
baseKB:cycPrepending(ft,'Character').
baseKB:cycPrepending(ft,'Constant').
baseKB:cycPrepending(ft,'Expression').
baseKB:cycPrepending(ft,'ExpressionAskable').
baseKB:cycPrepending(ft,'ExpressionAssertible').
%baseKB:cycPrepending(ft,'GenericRelationFormula').
baseKB:cycPrepending(ft,'InferenceDataStructure').
baseKB:cycPrepending(ft,'NonNegativeScalarInterval').
baseKB:cycPrepending(ft,'InferenceSupportedTerm').
baseKB:cycPrepending(ft,'KBDatastructure').
baseKB:cycPrepending(ft,'Keyword').
baseKB:cycPrepending(ft,'List').
baseKB:cycPrepending(ft,'NonAtomicReifiedTerm').
baseKB:cycPrepending(ft,'NonAtomicTerm').
baseKB:cycPrepending(ft,'NonAtomicTerm-Askable').
baseKB:cycPrepending(ft,'NonAtomicTerm-Assertible').
baseKB:cycPrepending(ft,'NonNegativeInteger').
baseKB:cycPrepending(ft,'NonVariableNonKeywordSymbol').
baseKB:cycPrepending(ft,'NonVariableSymbol').
baseKB:cycPrepending(ft,'PositiveInteger').
baseKB:cycPrepending(ft,'PropositionalSentence').
baseKB:cycPrepending(ft,'RealNumber').
baseKB:cycPrepending(ft,'ReifiableDenotationalTerm').
baseKB:cycPrepending(ft,'ReifiableNonAtomicTerm').
baseKB:cycPrepending(ft,'ReifiedDenotationalTerm').
baseKB:cycPrepending(ft,'RepresentedAtomicTerm').
baseKB:cycPrepending(ft,'Sentence').
baseKB:cycPrepending(ft,'String').
baseKB:cycPrepending(ft,'Symbol').

/*
baseKB:cycPrepending(ft,'NonAtomicTerm-ClosedFunctor').
baseKB:cycPrepending(ft,'OpenDenotationalTerm').
baseKB:cycPrepending(ft,'OpenExpression').
baseKB:cycPrepending(ft,'OpenFormula').
baseKB:cycPrepending(ft,'OpenNonAtomicTerm').
baseKB:cycPrepending(ft,'OpenSentence').
baseKB:cycPrepending(ft,'ClosedAtomicSentence').
% baseKB:cycPrepending(ft,'ClosedAtomicTerm').
baseKB:cycPrepending(ft,'ClosedDenotationalTerm').
baseKB:cycPrepending(ft,'ClosedExpression').
baseKB:cycPrepending(ft,'ClosedFormula').
baseKB:cycPrepending(ft,'ClosedNonAtomicTerm').
baseKB:cycPrepending(ft,'ClosedSentence').
*/
/*
baseKB:cycPrepending(ft,'RepresentedTerm').
baseKB:cycPrepending(ft,'RuleAssertion').
baseKB:cycPrepending(ft,'ScalarIntegralValue').
baseKB:cycPrepending(ft,'SentenceAskable').
baseKB:cycPrepending(ft,'SentenceAssertible').
baseKB:cycPrepending(ft,'SupportDatastructure').
baseKB:cycPrepending(ft,'TheTerm').
baseKB:cycPrepending(ft,'AssertedAssertion').
baseKB:cycPrepending(ft,'Assertion').
baseKB:cycPrepending(ft,'DeducedAssertion').
baseKB:cycPrepending(ft,'DenotationalTerm').
baseKB:cycPrepending(ft,'DenotationalTerm-Assertible').
baseKB:cycPrepending(ft,'DocumentationConstant').
baseKB:cycPrepending(ft,'GAFAssertion').
baseKB:cycPrepending(ft,'HLPrototypicalTerm').
baseKB:cycPrepending(ft,'IndeterminateTerm').
baseKB:cycPrepending(ft,'IndexedTerm').
baseKB:cycPrepending(ft,'TruthValueSentence').
*/

baseKB:cycPrepending(v,'SingleEntry').
baseKB:cycPrepending(v,'SetTheFormat').

baseKB:cycPrepending(tt,'TransformationModuleSupportedCollection').
baseKB:cycPrepending(tt,'RemovalModuleSupportedCollection-Generic').

baseKB:cycPrepending(rt,'UnaryPredicate').
baseKB:cycPrepending(rt,'RemovalModuleSupportedPredicate-Generic').
baseKB:cycPrepending(rt,'RemovalModuleSupportedPredicate-Specific').
baseKB:cycPrepending(rt,'InferenceSupportedPredicate').
baseKB:cycPrepending(rt,'SentenceClosedPredicate').
baseKB:cycPrepending(rt,'TransformationModuleSupportedPredicate').
baseKB:cycPrepending(rt,'ArgGenlQuantityBinaryPredicate').
baseKB:cycPrepending(rt,'BinaryFunction').
baseKB:cycPrepending(rt,'UnreifiableFunction').
baseKB:cycPrepending(rt,'UnaryFunction').


baseKB:cycPrepending(v,'Forward-AssertionDirection').
baseKB:cycPrepending(v,'Code-AssertionDirection').
baseKB:cycPrepending(v,'Backward-AssertionDirection').
baseKB:cycPrepending(vt,'AssertionDirection').



baseKB:cycPrepending(tt,'InferenceSupportedCollection').

baseKB:cycPrepending(rt,A):-atom(A),atom_contains(A,'Predicate').


% for SUMO
:- dynamic(baseKB:sumo_to_plarkc/2).
baseKB:sumo_to_plarkc('domain', 'argIsa').
baseKB:sumo_to_plarkc('range', 'resultIsa').
baseKB:sumo_to_plarkc('domainSubclass', 'argGenl').
baseKB:sumo_to_plarkc('rangeSubclass', 'resultGenl').
baseKB:sumo_to_plarkc('instance', 'isa').
baseKB:sumo_to_plarkc(subrelation,genlPreds).
baseKB:sumo_to_plarkc(documentation,comment).
baseKB:sumo_to_plarkc('Class','tSet').
baseKB:sumo_to_plarkc('SetOrClass', 'tCol').


:- dynamic(baseKB:cyc_to_plarkc/2).



baseKB:cyc_to_plarkc('BaseKB', baseKB).
baseKB:cyc_to_plarkc('between', cycBetween).
baseKB:cyc_to_plarkc('forall', cycforAll).

baseKB:cyc_to_plarkc('equals', mudEquals).
% baseKB:cyc_to_plarkc('termOfUnit',skolem ).


baseKB:cyc_to_plarkc(C,P):- renames_all(C,P).
baseKB:cyc_to_plarkc(Was, baseKB):-mtUndressedMt(Was).
baseKB:cyc_to_plarkc('Cyclist', tAuthor).
%baseKB:cyc_to_plarkc('and', '&').
baseKB:cyc_to_plarkc('forAll', 'all').
baseKB:cyc_to_plarkc('thereExists', 'exists').
baseKB:cyc_to_plarkc('thereExistsAtLeast', 'atleast').
baseKB:cyc_to_plarkc('thereExistsAtMost', 'atmost').
baseKB:cyc_to_plarkc('CycLClosedAtomicTerm', 'ftAtomicTerm').
baseKB:cyc_to_plarkc('UnitOfMeasure', 'tUnitOfMeasure').
baseKB:cyc_to_plarkc('CharacterString', 'ftString').
% baseKB:cyc_to_plarkc('SetOrCollection', 'tSpec').
baseKB:cyc_to_plarkc('ScalarInterval', 'tScalarInterval').


%baseKB:cyc_to_plarkc('or', 'v').
baseKB:cyc_to_plarkc('holds', 't').
baseKB:cyc_to_plarkc('dot_holds', 't').


:- dynamic(baseKB:mpred_to_cyc/2).


baseKB:mpred_to_cyc(ftInt,'Integer').
baseKB:mpred_to_cyc(ftSentence,'CycLFormulaicSentence').
baseKB:mpred_to_cyc(ftSentence,'FormulaicSentence').
% baseKB:mpred_to_cyc(ftNonAtomicTerm,'CycLGenericRelationFormula').

baseKB:mpred_to_cyc(ftSentence,'SubLFormulaicSentence').
baseKB:mpred_to_cyc(ftSentence,'icSentenceSentence').
baseKB:mpred_to_cyc(ftVar,'CycLVariable').
baseKB:mpred_to_cyc(ftVar,'Variable').

baseKB:mpred_to_cyc(ttExpressionType,'CycLExpressionType').
baseKB:mpred_to_cyc(ttExpressionType,'ExpressionType').

baseKB:mpred_to_cyc(tAgent,'Agent-Generic').
baseKB:mpred_to_cyc(tCol,'Collection').
baseKB:mpred_to_cyc(tFunction,'Function-Denotational').
baseKB:mpred_to_cyc(tHumanCyclist, 'HumanCyclist').
baseKB:mpred_to_cyc(tKnowledgeBase, 'KnowledgeBase').
baseKB:mpred_to_cyc(tMicrotheory, 'Microtheory').
baseKB:mpred_to_cyc(tPred,'Predicate').
baseKB:mpred_to_cyc(tProblemStore, 'CycProblemStore').
baseKB:mpred_to_cyc(tRuleTemplate, 'RuleTemplate').
baseKB:mpred_to_cyc(tThing, 'Thing').
baseKB:mpred_to_cyc(tIndividual, 'Individual').

baseKB:mpred_to_cyc(vtDayOfWeekType, 'DayOfWeekType').
baseKB:mpred_to_cyc(vtMonthOfYearType, 'MonthOfYearType').
baseKB:mpred_to_cyc(vtFormat, 'Format').
baseKB:mpred_to_cyc(vtInferenceProblemLinkStatus, 'CycInferenceProblemLinkStatus').
baseKB:mpred_to_cyc(vtHLTruthValue, 'CycHLTruthValue').
baseKB:mpred_to_cyc(vtProvabilityStatus, 'CycProvabilityStatus').
baseKB:mpred_to_cyc(vtTruthValue, 'TruthValue').
baseKB:mpred_to_cyc(vtCanonicalizerDirective, 'CanonicalizerDirective').

baseKB:mpred_to_cyc(tColOfCollectionSubsetFn,'CollectionSubsetFn').
baseKB:mpred_to_cyc(vTrue, 'True').
baseKB:mpred_to_cyc(vFalse, 'False').
baseKB:mpred_to_cyc(vGuest, 'Guest').
baseKB:mpred_to_cyc(vAdministrator, 'CycAdministrator').

baseKB:mpred_to_cyc(vIntervalEntry, 'IntervalEntry').
baseKB:mpred_to_cyc(vSingleEntry, 'SingleEntry').


baseKB:mpred_to_cyc(ftAtomicTerm, 'CycLClosedAtomicTerm').
baseKB:mpred_to_cyc(vSetTheFormat, 'SetTheFormat').

baseKB:mpred_to_cyc(vAssertedFalseDefault, 'AssertedFalseDefault').
baseKB:mpred_to_cyc(vAssertedFalseMonotonic, 'AssertedFalseMonotonic').
baseKB:mpred_to_cyc(vAssertedTrueDefault, 'AssertedTrueDefault').
baseKB:mpred_to_cyc(vAssertedTrueMonotonic, 'AssertedTrueMonotonic').
baseKB:mpred_to_cyc(vMonotonicallyFalse, 'MonotonicallyFalse').
baseKB:mpred_to_cyc(vMonotonicallyTrue, 'MonotonicallyTrue').
baseKB:mpred_to_cyc(vDefaultFalse, 'DefaultFalse').
baseKB:mpred_to_cyc(vDefaultTrue, 'DefaultTrue').

baseKB:mpred_to_cyc('vGoodProblemProvabilityStatus', 'Good-ProblemProvabilityStatus').
baseKB:mpred_to_cyc('vNeutralProblemProvabilityStatus', 'Neutral-ProblemProvabilityStatus').
baseKB:mpred_to_cyc('vNoGoodProblemProvabilityStatus', 'NoGood-ProblemProvabilityStatus').
baseKB:mpred_to_cyc('vUnknownHLTruthValue', 'Unknown-HLTruthValue').
baseKB:mpred_to_cyc('vExistentialQuantifierBounded', 'ExistentialQuantifier-Bounded').
baseKB:mpred_to_cyc(vAllowGenericArgVariables, 'AllowGenericArgVariables').
baseKB:mpred_to_cyc(vAllowKeywordVariables, 'AllowKeywordVariables').
baseKB:mpred_to_cyc(vRelaxArgTypeConstraintsForVariables, 'RelaxArgTypeConstraintsForVariables').
baseKB:mpred_to_cyc(vLeaveSomeTermsAtEL, 'LeaveSomeTermsAtEL').
baseKB:mpred_to_cyc(vLeaveSomeTermsAtELAndAllowKeywordVariables, 'LeaveSomeTermsAtELAndAllowKeywordVariables').
baseKB:mpred_to_cyc(vLeaveVariablesAtEL, 'LeaveVariablesAtEL').
baseKB:mpred_to_cyc(vDontReOrderCommutativeTerms, 'DontReOrderCommutativeTerms').

baseKB:mpred_to_cyc(vReformulationBackwardDirection, 'ReformulationBackwardDirection').
baseKB:mpred_to_cyc(vReformulationForwardDirection, 'ReformulationForwardDirection').
baseKB:mpred_to_cyc(vReformulationNeitherDirection, 'ReformulationNeitherDirection').
baseKB:mpred_to_cyc(ftSentenceAssertible, 'CycLSentence-ClosedPredicate').
baseKB:mpred_to_cyc(ftNonAtomicTerm, 'CycLNonAtomicTerm-ClosedFunctor').


baseKB:mpred_to_cyc(vApril, 'April').
baseKB:mpred_to_cyc(vAugust, 'August').
baseKB:mpred_to_cyc(vDecember, 'December').
baseKB:mpred_to_cyc(vFebruary, 'February').
baseKB:mpred_to_cyc(vJanuary, 'January').
baseKB:mpred_to_cyc(vJuly, 'July').
baseKB:mpred_to_cyc(vJune, 'June').
baseKB:mpred_to_cyc(vMarch, 'March').
baseKB:mpred_to_cyc(vMay, 'May').
baseKB:mpred_to_cyc(vNovember, 'November').
baseKB:mpred_to_cyc(vOctober, 'October').
baseKB:mpred_to_cyc(vSeptember, 'September').

baseKB:mpred_to_cyc(vSunday, 'Sunday').
baseKB:mpred_to_cyc(vMonday, 'Monday').
baseKB:mpred_to_cyc(vTuesday, 'Tuesday').
baseKB:mpred_to_cyc(vWednesday, 'Wednesday').
baseKB:mpred_to_cyc(vThursday, 'Thursday').
baseKB:mpred_to_cyc(vFriday, 'Friday').
baseKB:mpred_to_cyc(vSaturday, 'Saturday').

baseKB:mpred_to_cyc(P,C):- loop_check(baseKB:cyc_to_plarkc(C,P)),!.

dehyphenize_pred(PM,PMO):-tokenize_atom(PM,Toks),member(E,Toks),number(E),E<0,!,atomic_list_concat(List,'-',PM),atomic_list_concat(List,'_',PMO),!.
dehyphenize_pred(PM,PMO):-atomic_list_concat(List,'-',PM),atomic_list_concat(List,PMO),!.

:- must(dehyphenize_pred('a-b','ab')).
:- must(dehyphenize_pred('a-2b','a_2b')).

:- forall(baseKB:cycPrepending(AT,A),((atom_concat(AT,A,FT),dehyphenize_pred(FT,FFT),asserta(baseKB:mpred_to_cyc(FFT,A))))).


notFormatType(tThing).
notFormatType(tIndividual).
notFormatType(tInferenceSupportedFunction).

:- forall(notFormatType(NFT),ain(tSet(NFT))).


expT('SubLExpressionType').
expT('SubLExpression').
expT('CycLExpression').
expT('ttExpressionType').


isF(X):- atom_concat(_,'Fn',X).
isF(X):- tinyKB1(resultIsa(X,_)).
isF(X):- tinyKB1(resultQuotedIsa(X,_)).
isF(X):- tinyKB1(resultGenl(X,_)).
isF(X):- tinyKB1(isa(X,C)),atom_contains(C,'Function').

isFT(X):- expT(FT),tinyKB1(isa(X,FT)),!.
isFT(X):- expT(FT),tinyKB1(genls(X,FT)),!.
isFT(X):- tinyKB1(resultIsa(X,FT)),expT(FT),!.
isFT(X):- tinyKB1(resultGenl(X,FT)),expT(FT),!.
isFT(X):- tinyKB1(resultQuotedIsa(X,_)),!.

isV(X):- tinyKB1(isa(X,VT)),isVT(VT).
isVT(X):- tinyKB1(genls(X,'Individual')).

isPT(X):- atom_concat(_,'Predicate',X).
isPT(X):- tinyKB1(genls(X,'tPred')).

isRT(X):- atom_concat(_,'Function',X).
isRT(X):- atom_concat(_,'Relation',X).
isRT(X):- tinyKB1(genls(X,'tRelation')).
isRT(X):- tinyKB1(genls(X,'tFunction')).

:-dynamic(tinyKB0/1).

maybe_ruleRewrite(I,O):-ruleRewrite(I,O),!.
maybe_ruleRewrite(IO,IO).

:- dynamic(baseKB:cyc_to_plarkc/2).

mpred_prepend_type(X,_):- \+ atom(X),!,fail.
mpred_prepend_type(X,PP):- baseKB:cycPrepending(PP,X),!.
mpred_prepend_type(X,PrePend):- mpred_prepend_type_via(X,PrePend).
mpred_prepend_type(X,_):- starts_lower(X),!,fail.



mpred_prepend_type(X,t):- tinyKB1(genls(X,'tMicrotheory')),!.
mpred_prepend_type(X,ft):- isFT(X),!.
mpred_prepend_type(X,_):- isF(X),!,fail.
mpred_prepend_type(X,rt):- isPT(X),!.
mpred_prepend_type(X,rt):- isRT(X),!.
%mpred_prepend_type(X,v):- isV(X),!.
%mpred_prepend_type(X,v):- isVT(X),!.
%mpred_prepend_type(X,tt):- tinyKB1(genls(X,'tCol')),!.
%mpred_prepend_type(X,tt):- tinyKB1(isa(X,'AtemporalNecessarilyEssentialCollectionType')),!.
%mpred_prepend_type(X,t):- tinyKB1(isa(X,'tCol')),!.
%mpred_prepend_type(X,t):- tinyKB1(isa(_,X)),!.
%mpred_prepend_type(X,v):- name(X,[C|_]),char_type(C,upper),!.

mpred_postpend_type(X,_):- starts_lower(X),!,fail.
mpred_postpend_type(C,'Fn'):-isF(C).

mpred_prepend_type_via(C,Pre):-renames_all(C,P),dehyphenize_pred(C,H),atom_concat(Pre,H,P).

tinyKB_wstr(P):-mtUndressedMt(MT),tinyKB(P,MT,_).
tinyKB_wstr(ist(MT,P)):-mtDressedMt(MT),tinyKB(P,MT,_).


mwkb1:- tell(fooooo0),
   forall(tinyKB_wstr(P),((cyc_to_clif(P,SAVE),
     once(must(unnumbervars(SAVE,SAVE2))),
     assert_if_new(tinyKB9(SAVE2)),
   format('~q.~n',[tinyKB9(SAVE)])))),
   told.

ltkb1:- check_clause_counts,
 defaultAssertMt(MT),
 must(logicmoo_i_cyc_kb\==MT),
 with_current_why(mfl(MT, ltkb1, _ ),
 ( MT: must_det_l(( mwkb1,forall(find_and_call(tinyKB0(D)), MT:cycAssert(D)))),
         check_clause_counts,
         finish_asserts,
  ltkb1_complete)).


finish_asserts:- call_u(forall(find_and_call(tinyKB8(Fact)),mpred_post(baseKB:Fact,(tinyKB8(Fact),ax)))).

ltkb1_complete:- 
  finish_asserts,
  doall((filematch(logicmoo('plarkc/logicmoo_i_cyc_kb_tinykb.pfc'),F),
  source_file(X,F),
  predicate_property(X,dynamic),retract(X:-_))).


wkb0:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),format('~q.~n',[tinyKB_All(V,MT,STR)])),
      told.
wkb01:- tell(fooooo0),
      forall(tinyKB_All(V,MT,STR),format('~q.~n',[tinyKB_All(V,MT,STR)])),
      told.


wkbe:- statistics(cputime,S),tell(foof),
   ignore((el_assertions:el_holds_pred_impl(F),between(2,16,A),
          current_predicate(F/A),functor(P,F,A),forall(P,format('~q.~n',[P])),fail)),told,
   statistics(cputime,E),Total is E - S, writeln(Total).

wkb2:- tell(fooooo2),
      ignore(( tinyKB(D,MT,Str),cyc_to_clif(D,KB),format('~N~q.~N',[proof(KB,D,MT,Str)]),fail)),
      told.

:- was_export(cyc_to_mpred_idiom/2).
%cyc_to_mpred_idiom(different,dif).

starts_lower(X):-name(X,[S|_]),char_type(S,lower).

never_idiom((:-)).
never_idiom((,)).
never_idiom(Atom):-atom_length(Atom,Len),Len<3.
never_idiom(A):- upcase_atom(A,U),downcase_atom(A,U).
cyc_to_mpred_idiom(X,_):- \+ atom(X),!,fail.
cyc_to_mpred_idiom(X,X):-never_idiom(X),!.
cyc_to_mpred_idiom(KW,SYMBOL):-name(KW,[58,LETTER|REST]),char_type(LETTER,alpha),!,name(SYMBOL,[LETTER|REST]).
cyc_to_mpred_idiom(KW,'$VAR'(VAR)):-name(KW,[63,LETTER|REST]),char_type(LETTER,alpha),!,name(SYMBOL,[LETTER|REST]),fix_var_name(SYMBOL,VAR),!.
cyc_to_mpred_idiom(C,P):-baseKB:cyc_to_plarkc(C,P),!.
cyc_to_mpred_idiom(C,P):-baseKB:mpred_to_cyc(P,C),!.
cyc_to_mpred_idiom(equiv,(<=>)).
cyc_to_mpred_idiom(implies,(=>)). 
%cyc_to_mpred_idiom(not,(~)).
cyc_to_mpred_idiom(X,Y):- starts_lower(X),renames_all(X,Y),!.
cyc_to_mpred_idiom(X,Y):- starts_lower(X),!,dehyphenize_pred(X,Y).
cyc_to_mpred_idiom(C,PM):- 
  cyc_to_mpred_idiom_did(C,PM),
  C\==PM,
  asserta(baseKB:cyc_to_plarkc(C,PM)),
  asserta(baseKB:mpred_to_cyc(PM,C)),!.
cyc_to_mpred_idiom(C,PM):- renames_all(C,PM),!.
cyc_to_mpred_idiom(C,PM):- transitive_lc(cyc_to_mpred_idiom1,C,PM),!.
cyc_to_mpred_idiom(C,P):- atom_concat(it,C,P).

cyc_to_mpred_idiom_did(C,PM):- atom(C),  transitive_lc(cyc_to_mpred_idiom1,C,M),!,
 m_to_pm(M,C,PM),!.

m_to_pm(M,C,P):- mpred_prepend_type(C,PT), (atom_concat(PT,_,M)-> P=M; atom_concat(PT,M,P)).
% m_to_pm(M,C,P):- mpred_postpend_type(C,PT), (atom_concat(_,PT,M)-> P=M; atom_concat(M,PT,P)).


cyc_to_mpred_idiom1('CycLTerm','CycLExpression').
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLSentence-',Type,C),!,atom_concat('Sentence',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Expression-',Type,C),!,atom_concat('Expression',Type,P).
cyc_to_mpred_idiom1(C,P):-nonvar(C),baseKB:mpred_to_cyc(P,C),!.

% TODO remove these next two simplifcations one day
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLOpen',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycLClosed',P,C).
 

cyc_to_mpred_idiom1(C,P):-atom_concatM('SubL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyclist',Type,C),!,atom_concat('Author',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('CycL',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('Cyc',P,C).
cyc_to_mpred_idiom1(C,P):-atom_concatM('FormulaicSenten',Type,C),!,atom_concat('Senten',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatM('SExpressi',Type,C),!,atom_concat('Expressi',Type,P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Assertible'),!,atom_concat(Type,'Assertible',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'-Askable'),!,atom_concat(Type,'Askable',P).
cyc_to_mpred_idiom1(C,P):-atom_concatR(C,Type,'FormulaicSentence'),!,atom_concat(Type,'Sentence',P).
cyc_to_mpred_idiom1(B,A):-dehyphenize_pred(B,A).

cyc_to_mpred_idiom_unused([Conj|MORE],Out):-fail, not(is_ftVar(Conj)),!,cyc_to_mpred_sent_idiom_2(Conj,Pred,_),
  w_tl(thocal:outer_pred_expansion(Conj,MORE),
    ( maplist(cyc_to_clif,MORE,MOREL), 
       w_tl(thocal:outer_pred_expansion(Pred,MOREL),       
         list_to_ops(Pred,MOREL,Out)))),!.


atom_concatM(L,M,R):-atom(L),nonvar(R),atom_concat(L,M,R),atom_length(M,N),!,N > 1.
atom_concatR(L,M,R):-atom(R),nonvar(L),atom_concat(L,M,R),atom_length(M,N),!,N > 1.

cyc_to_mpred_sent_idiom_2(and,(','),trueSentence).

list_to_ops(_,V,V):-is_ftVar(V),!.
list_to_ops(Pred,[],Out):-cyc_to_mpred_sent_idiom_2(_,Pred,Out),!.
list_to_ops(Pred,In,Out):-not(is_list(In)),!,cyc_to_clif(In,Mid),cyc_to_mpred_sent_idiom_2(_,Pred,ArityOne),Out=..[ArityOne,Mid].
list_to_ops(_,[In],Out):-!,cyc_to_clif(In,Out).
list_to_ops(Pred,[H,T],Body):-!,
    cyc_to_clif(H,HH),
    cyc_to_clif(T,TT),
    (is_list(TT)-> Body=..[Pred,HH|TT]; Body=..[Pred,HH,TT]).

list_to_ops(Pred,[H|T],Body):-!,
    list_to_ops(Pred,H,HH),
    list_to_ops(Pred,T,TT),
    (is_list(TT)-> Body=..[Pred,HH|TT]; Body=..[Pred,HH,TT]).

kw_to_vars(KW,VARS):-subsT_each(KW,[':ARG1'=_ARG1,':ARG2'=_ARG2,':ARG3'=_ARG3,':ARG4'=_ARG4,':ARG5'=_ARG5,':ARG6'=_ARG6],VARS).
make_kw_functor(F,A,CYCL):-make_kw_functor(F,A,CYCL,':ARG'),!.
make_kw_functor(F,A,CYCL,PREFIX):-make_functor_h(CYCL,F,A),CYCL=..[F|ARGS],label_args(PREFIX,1,ARGS).

label_args(_PREFIX,_,[]).
label_args(PREFIX,N,[ARG|ARGS]):-atom_concat(PREFIX,N,TOARG),ignore(TOARG=ARG),!,N2 is N+1,label_args(PREFIX,N2,ARGS).

:- thread_local thocal:outer_pred_expansion/2.

cyc_to_clif_notify(B,A):- cyc_to_clif(B,A) -> B\=@=A, nop(dmsg(B==A)).
%cyc_to_clif_entry(I,O):-fail,cyc_to_clif(I,M),!,must((functor(I,FI,_),functor(M,MF,_),FI==MF)),O=M.

cyc_to_clif(V,V):-is_ftVar(V),!.
cyc_to_clif([],[]):-!.
cyc_to_clif([H],HH):- string(H),convert_to_cycString(H,HH),!.
cyc_to_clif(H,HH):- string(H),convert_to_cycString(H,HH),!.
cyc_to_clif(I,O):- \+ (compound(I)),do_renames(I,O),!.
cyc_to_clif('uSubLQuoteFn'(V),V):-atom(V),!.
% cyc_to_clif(isa(I,C),O):-atom(C),M=..[C,I],!,cyc_to_clif(M,O).
cyc_to_clif(I,O):-ruleRewrite(I,M),I\=@=M,!,cyc_to_clif(M,O).


cyc_to_clif([H|T],[HH|TT]):-!,cyc_to_clif(H,HH),cyc_to_clif(T,TT),!.

cyc_to_clif(HOLDS,HOLDSOUT):-HOLDS=..[F|HOLDSL], stack_check,
  w_tl(thocal:outer_pred_expansion(F,HOLDSL),maplist( cyc_to_clif,[F|HOLDSL],[C|HOLDSOUTL])),!,
  ((is_list([C|HOLDSOUTL]), atom(C))-> must(HOLDSOUT=..[C|HOLDSOUTL]) ; HOLDSOUT=[C|HOLDSOUTL]),!.

:-export(do_renames/2).

fix_var_name(A,B):- atom_concat(':',_,A), atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).
fix_var_name(A,B):- atom_concat('?',QB,A),!,atom_concat('_',QB,B).
fix_var_name(A,B):- atomic_list_concat(AB,'-',A),atomic_list_concat(AB,'_',B).

% rename_atom(A,B):- atom_contains(A,'~'),!,convert_to_cycString(A,B),nb_setval('$has_quote',t),!.
rename_atom(A,B):- atom_contains(A,' '),!,convert_to_cycString(A,B),nb_setval('$has_quote',t),!.
rename_atom(A,B):- must(cyc_to_mpred_idiom(A,B)),!.

do_renames(A,B):- var(A),!,A=B.
do_renames(uU('SubLQuoteFn',A),uSubLQuoteFn(A)):-var(A),!,nb_setval('$has_var',t),!.
do_renames(uU('SubLQuoteFn','$VAR'(A)),uSubLQuoteFn(A)):-!,nb_setval('$has_quote',t),!.
do_renames('$VAR'(A),'$VAR'(B)):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
%do_renames('$VAR'(A),B):- catch((fix_var_name(A,B),!,nb_setval('$has_var',t)),E,(dtrace(dmsg(E)))),!.
do_renames(A,B):- string(A),!,logicmoo_util_strings:convert_to_cycString(A,B).
do_renames(A,B):- atom(A),rename_atom(A,B),!.
do_renames(A,B):- \+ compound(A),!,A=B.
do_renames([A|Rest],[B|List]):- !, do_renames(A,B),do_renames(Rest,List).
do_renames(uN(P,ARGS),B):-!,maplist(do_renames,[P|ARGS],List),compound_name_arguments(B,uT,List).
do_renames(A,B):- compound_name_arguments(A,P,ARGS),maplist(do_renames,[P|ARGS],[T|L]),do_renames_pass2(T,L,B).

compute_argIsa(ARG1ISA,NN,ARGISA):-
  atom(ARG1ISA),
  atom_concat('arg',REST,ARG1ISA),
  member(E,['Genl','Isa','SometimesIsa','Format','QuotedIsa']),atom_concat(N,E,REST),
  atom_number(N,NN),
  atom_concat('arg',E,ARGISA),!.

do_renames_pass2(forward,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(forward,[MT,P,ARGISA,NN,C,ID],OUT).
do_renames_pass2(backward,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(backward,[MT,P,ARGISA,NN,C,ID],OUT).
do_renames_pass2(code,[MT,C,ARG1ISA,P,ID],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),!, 
  do_renames_pass2(code,[MT,P,ARGISA,NN,C,ID],OUT).
do_renames_pass2(t,[ARG1ISA,P,C],OUT):- compute_argIsa(ARG1ISA,NN,ARGISA),  OUT = t(ARGISA,P,NN,C).

do_renames_pass2(P,[],B):-!,do_renames(P,B).
do_renames_pass2(nartR,[P|ARGS],(B)):-atom(P),!,compound_name_arguments(B,P,ARGS).
do_renames_pass2(nartR,ARGS,B):-!,compound_name_arguments(B,nartR,ARGS).
do_renames_pass2(t,[P,I,C],B):- P==isa,atom(C),!,B=..[C,I].
do_renames_pass2(t,[P|IC],B):- intrinsicPred(P),!,B=..[P|IC].
do_renames_pass2(t,ARGS,B):- compound_name_arguments(B,t,ARGS).
do_renames_pass2(uU,ARGS,B):-!,compound_name_arguments(B,u,ARGS).
do_renames_pass2(P,ARGS,B):-!,compound_name_arguments(B,P,ARGS).

intrinsicPred(genlMt).
intrinsicPred(ist).
intrinsicPred(termOfUnit).

:- (current_prolog_flag(lm_expanders,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(lm_expanders,PrevValue))),
   set_prolog_flag(lm_expanders,false).

:- (current_prolog_flag(double_quotes,PrevValue)->true;PrevValue=false),
   call(assert,on_fin(set_prolog_flag(double_quotes,PrevValue))),
   set_prolog_flag(double_quotes,atom).

:- if(current_prolog_flag(logicmoo_simplify_te,true)).
:- (call(asserta,((system:term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- (call(asserta,((user:term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- (call(asserta,((term_expansion(I, (:- true)):- !, I\=(:- _), call(assert,I))),Ref),call(assert,on_fin(erase(Ref)))),!.
:- endif.


/*
:- dynamic(argIsa/3).
:- shared_multifile(argIsa/3).
:- dynamic(argGenl/3).
:- shared_multifile(argGenl/3).
:- dynamic(argQuotedIsa/3).
:- shared_multifile(argQuotedIsa/3).
isa(I,C):-exactlyAssertedEL(isa,I,C,_,_).
genls(I,C):-exactlyAssertedEL(genls,I,C,_,_).
arity(I,C):-exactlyAssertedEL(arity,I,C,_,_).
argIsa(P,N,C):-exactlyAssertedEL(argIsa,P,N,C,_,_).
argGenl(P,N,C):-exactlyAssertedEL(argGenl,P,N,C,_,_).
argQuotedIsa(P,N,C):-exactlyAssertedEL(argQuotedIsa,P,N,C,_,_).
*/
% queuedTinyKB(CycL,MT):- (mtUndressedMt(MT);mtDressedMt(MT)),(STR=vStrMon;STR=vStrDef),  tinyKB_All(CycL,MT,STR),\+ clause(exactlyAssertedEL(CycL,_,_,_),true).
% queuedTinyKB(CycL):-mtUndressedMt(MT),queuedTinyKB(CycL,MT).
% queuedTinyKB(ist(MT,CycL)):-mtDressedMt(MT),queuedTinyKB(CycL,MT).


ist_tiny(MT,P):-tinyKB(P,MT,vStrMon).
ist_tiny(MT,P):-tinyKB(P,MT,vStrDef).

%TODO ADD BACK AFTER OPTIZING
tinyKB(P):- tinyKB(P,_MT,_).
tinyKB(ist(MT,P)):-!,mtDressedMt(MT),tinyKB(P,MT,_).
tinyKB(D):-find_and_call(tinyKB0(D)).

tinyKB1(D):-no_repeats(tinyKB2(D)).
tinyKB2(P):- tinyKB(P,_MT,_).
tinyKB2(D):-tinyKB0(D).
tinyKB2(isa(C1,C3)):-nonvar(C1),!,tinyKB0(isa(C1,C2)),tinyKB2(genls(C2,C3)).
tinyKB2(genls(C1,C3)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB2(genls(C2,C3)).
/*
tinyKB2(genls(C1,C3)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB0(genls(C2,C3)).
tinyKB2(genls(C1,C4)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB0(genls(C2,C3)),tinyKB0(genls(C3,C4)).
tinyKB2(genls(C1,C5)):-nonvar(C1),tinyKB0(genls(C1,C2)),tinyKB0(genls(C2,C3)),tinyKB0(genls(C3,C4)),tinyKB0(genls(C4,C5)).
*/
%TODO ADD BACK AFTER OPTIZING tinyKB(P):-nonvar(P),if_defined(P).

tinyKB(PO,MT,STR):- %fwc,  
  (mtUndressedMt(MT);mtDressedMt(MT)),(STR=vStrMon;STR=vStrDef), 
  tinyKB_All(PO,MT,STR).

tinyKB_All(V,MT,STR):- tinyAssertion(V,MT,STR).
tinyKB_All(PO,MT,STR):- current_predicate(_:'TINYKB-ASSERTION'/5),!,
    tiny_kb_ASSERTION(PLISTIn,PROPS),
        once((sexpr_sterm_to_pterm(PLISTIn,P),
               memberchk(amt(MT),PROPS),
               memberchk(str(STR),PROPS), 
              (member(vars(VARS),PROPS)->(nput_variable_names( []),fixvars(P,0,VARS,PO),nput_variable_names( PO));PO=P ))).

loadTinyKB:-forall(tinyKB(P,MT,STR),((print_assertion(P,MT,STR),ain(P)))).
% ssveTinyKB:-tinyKB_All(tinyKB(P,MT,STR),tell((print_assertion(P,MT,STR),ain(P)))).

print_assertion(P,MT,STR):- P=..PL,append([exactlyAssertedEL|PL],[MT,STR],PPL),PP=..PPL, portray_clause(current_output,PP,[numbervars(false)]).


mtUndressedMt('UniversalVocabularyImplementationMt').
mtUndressedMt('LogicalTruthImplementationMt').
mtUndressedMt('CoreCycLImplementationMt').
mtUndressedMt('UniversalVocabularyMt').
mtUndressedMt('LogicalTruthMt').
mtUndressedMt('CoreCycLMt').
mtUndressedMt('BaseKB').

mtDressedMt('BookkeepingMt').
mtDressedMt('EnglishParaphraseMt').
mtDressedMt('TemporaryEnglishParaphraseMt').

call_el_stub(V,MT,STR):-into_mpred_form(V,M),!,M=..ML,((ML=[t|ARGS]-> true; ARGS=ML)),CALL=..[exactlyAssertedEL|ARGS],!,call(CALL,MT,STR).
make_el_stub(V,MT,STR,CALL):-into_mpred_form(V,M),!,M=..ML,((ML=[t|ARGS]-> true; ARGS=ML)),append(ARGS,[MT,STR],CARGS),CALL=..[exactlyAssertedEL|CARGS],!.

tinyAssertion(V,MT,STR):- 
 nonvar(V) -> call_el_stub(V,MT,STR);
  (tinyAssertion0(W,MT,STR),once(into_mpred_form(W,V))).

tinyAssertion0(t(A,B,C,D,E),MT,STR):-exactlyAssertedEL(A,B,C,D,E,MT,STR).
tinyAssertion0(t(A,B,C,D),MT,STR):-exactlyAssertedEL(A,B,C,D,MT,STR).
tinyAssertion0(t(A,B,C),MT,STR):-exactlyAssertedEL(A,B,C,MT,STR).
tinyAssertion0(t(A,B),MT,STR):-exactlyAssertedEL(A,B,MT,STR).


addTinyCycL(CycLIn):- into_mpred_form(CycLIn,CycL),
  ignore((tiny_support(CycL,_MT,CALL),must(retract(CALL)))),!,
  addCycL(CycL),!.



tiny_support(CycLIn,MT,CALL):- compound(CycLIn),!,into_mpred_form(CycLIn,CycL), CycL=..[F|Args], append(Args,[MT,_STR],WMT),CCALL=..[exactlyAssertedEL,F|WMT],!,
  ((clause(CCALL,true), CCALL=CALL) ; clause(CCALL,(CALL,_))).
tiny_support(CycLOut,MT,CALL):- between(4,7,Len),functor(CCALL,exactlyAssertedEL,Len),CCALL=..[exactlyAssertedEL,F|WMT],append(Args,[MT,_STR],WMT),
 CCALL,(atom(F)->CycL=..[F|Args];append_termlist(F,Args,CycL)),((clause(CCALL,true), CCALL=CALL) ; clause(CCALL,(CALL,_))), fully_expand(CycL,CycLOut).

make_functor_h(CycL,F,A):- length(Args,A),CycL=..[F|Args].

is_simple_gaf(V):-not(compound(V)),!.
is_simple_gaf(V):-needs_canoncalization(V),!,fail.
is_simple_gaf(V):-functor(V,F,A),member(F/A,[isa/2,genls/2,argQuotedIsa/3,afterAdding/2,afterRemoving/2]),!.
is_simple_gaf(V):-needs_indexing(V),!,fail.
is_simple_gaf(_).

needs_indexing(V):-compound(V),arg(_,V,A),not(is_simple_arg(A)),!,fail.

is_simple_arg(A):-not(compound(A)),!.
is_simple_arg(A):-functor(A,Simple,_),rtEscapeFunction(Simple).

% :- dynamic(vtUnreifiableFunction/1).
'rtEscapeFunction'('TINYKB-ASSERTION').
'rtEscapeFunction'('uQuoteFn').
'rtEscapeFunction'(X):- clause_b('rtUnreifiableFunction'(X)).

needs_canoncalization(CycL):-is_ftVar(CycL),!,fail.
needs_canoncalization(CycL):-functor(CycL,F,_),isa_db(F,'SentenceOperator').
needs_canoncalization(CycL):-needs_indexing(CycL).

is_better_backchained(CycL):-is_ftVar(CycL),!,fail.
is_better_backchained(CycL):-functor(CycL,F,_),isa_db(F,'SentenceOperator').
is_better_backchained(V):-unnumbervars(V,FOO),(((each_subterm(FOO,SubTerm),nonvar(SubTerm),isa_db(SubTerm,tAvoidForwardChain)))),!.


as_cycl(VP,VE):-subst(VP,('-'),(~),V0),subst(V0,('v'),(or),V1),subst(V1,('exists'),(thereExists),V2),subst(V2,('&'),(and),VE),!.


:- dynamic(addTiny_added/1).
addCycL(V):-addTiny_added(V),!.
addCycL(V):-into_mpred_form(V,M),V\=@=M,!,addCycL(M),!.
addCycL(V):-defunctionalize('implies',V,VE),V\=@=VE,!,addCycL(VE).
addCycL(V):-cyc_to_clif(V,VE),V\=@=VE,!,addCycL(VE).
addCycL(V):-is_simple_gaf(V),!,addCycL0(V),!.
addCycL(V):-kif_to_boxlog(V,VB),boxlog_to_pfc(VB,VP),V\=@=VP,!,as_cycl(VP,VE),show_call(why,addCycL0(VE)).
addCycL(V):-addCycL0(V),!.

addCycL0(V):-addCycL1(V).

addCycL1(V):-into_mpred_form(V,M),V\=@=M,!,addCycL0(M),!.
addCycL1(V):-cyc_to_clif(V,VE),V\=@=VE,!,addCycL0(VE).
addCycL1((TRUE=>V)):-is_true(TRUE),addCycL0(V),!.
addCycL1(<=(V , TRUE)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V :- TRUE)):-is_true(TRUE),addCycL0(V),!.
addCycL1((V :- A)):- show_call(why,addCycL0((A => V))).
addCycL1((A => (V1 , V2))):-not(is_ftVar(V1)),!,show_call(why,addCycL0((A => V1))) , show_call(why,addCycL0((A => V2))).
addCycL1((V1 , V2)):-!,addCycL0(V1),addCycL0(V2),!.
addCycL1([V1 | V2]):-!,addCycL0(V1),addCycL0(V2),!.
addCycL1(V):-addTiny_added(V),!.
addCycL1(V):-asserta(addTiny_added(V)),unnumbervars(V,VE),
  % must(nop(remQueuedTinyKB(VE))),
  ain(VE).


sent_to_conseq(CycLIn,Consequent):- into_mpred_form(CycLIn,CycL), ignore((tiny_support(CycL,_MT,CALL),retract(CALL))),must(cycLToMpred(CycL,Consequent)),!.

:- dynamic(addTiny_added/1).

cycLToMpred(V,CP):-into_mpred_form(V,M),V\=@=M,!,cycLToMpred(M,CP),!.
cycLToMpred(V,CP):-cyc_to_clif(V,VE),V\=@=VE,!,cycLToMpred(VE,CP).
cycLToMpred(V,CP):-is_simple_gaf(V),!,cycLToMpred0(V,CP),!.
cycLToMpred(V,CP):-defunctionalize('implies',V,VE),V\=@=VE,!,cycLToMpred(VE,CP).
cycLToMpred(V,CP):-kif_to_boxlog(V,VB),boxlog_to_pfc(VB,VP),V\=@=VP,!,as_cycl(VP,VE),show_call(why,cycLToMpred0(VE,CP)).
cycLToMpred(V,CP):-cycLToMpred0(V,CP),!.

cycLToMpred0(V,CP):-into_mpred_form(V,M),V\=@=M,!,cycLToMpred0(M,CP),!.
cycLToMpred0(V,CP):-cyc_to_clif(V,VE),V\=@=VE,!,cycLToMpred0(VE,CP).
cycLToMpred0((TRUE => V),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V <=> TRUE),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V :- TRUE),CP):-is_true(TRUE),cycLToMpred0(V,CP),!.
cycLToMpred0((V :- A),CP):- show_call(why,cycLToMpred0((A => V),CP)).
cycLToMpred0((A => (V1 , V2)),CP):-not(is_ftVar(V1)),!,cycLToMpred0((A=> (V1/consistent(V2))),V1P),cycLToMpred0((A=> (V2/consistent(V1))),V2P) ,!,conjoin(V1P,V2P,CP).
cycLToMpred0((V1 , V2),CP):-!,cycLToMpred0(V1,V1P),cycLToMpred0(V2,V2P),!,conjoin(V1P,V2P,CP).
cycLToMpred0([V1 | V2],CP):-!,cycLToMpred0(V1,V1P),cycLToMpred0(V2,V2P),!,conjoin(V1P,V2P,CP).
cycLToMpred0(V,V).

%  cycLToMpred( (grandparent('$VAR'('G'),'$VAR'('C')) => thereExists('$VAR'('P'), and(parent('$VAR'('G'),'$VAR'('P')),parent('$VAR'('P'),'$VAR'('C'))))),O).



% :-onEachLoad(loadTinyAssertions2).

% ============================================
% DBASE to Cyc Predicate Mapping
% ============================================
/*
arity('abbreviationString-PN', 2).

typical_mtvars([_,_]).

% arity 1 person
make_functorskel(Person,1,fskel(Person,t(Person,A),Call,A,[],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[Person,A],Call2=..[Person,A|MtVars]. 
% arity 2 likes
make_functorskel(Likes,2,fskel(Likes,t(Likes,A,B),Call,A,B,MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Likes,A,B],Call2=..[Likes,A,B|MtVars]. 
% arity 3 between
make_functorskel(Between,3,fskel(Between,t(Between,A,B,C),Call,A,[B,C],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Between,A,B,C],Call2=..[Between,A,B,C|MtVars]. 
% arity 4 xyz
make_functorskel(Xyz,4,fskel(Xyz,t(Xyz,I,X,Y,Z),Call,I,[X,Y,Z],MtVars,Call2)):- typical_mtvars(MtVars),Call=..[Xyz,I,X,Y,Z],Call2=..[Xyz,I,X,Y,Z|MtVars]. 
% arity 5 rxyz
make_functorskel(RXyz,5,fskel(RXyz,t(RXyz,I,R,X,Y,Z),Call,I,[R,X,Y,Z],MtVars,Call2)):-typical_mtvars(MtVars),Call=..[RXyz,I,R,X,Y,Z],Call2=..[RXyz,I,R,X,Y,Z|MtVars]. 
% arity >6 
make_functorskel(F,N,fskel(F,DBASE,Call,I,NList,MtVars,Call2)):-typical_mtvars(MtVars),functor(Call,F,N),Call=..[F,I|NList],DBASE=..[t,F,I|NList],append([F,I|NList],MtVars,CALL2List),Call2=..CALL2List.

*/

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

:- dynamic(lmcache:isCycUnavailable_known/1).
:- dynamic(lmcache:isCycAvailable_known/0).

/*
:- was_export(isCycAvailable/0).
isCycAvailable:-lmcache:isCycUnavailable_known(_),!,fail.
isCycAvailable:-lmcache:isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:- was_export(isCycUnavailable/0).
isCycUnavailable:-lmcache:isCycUnavailable_known(_),!.
isCycUnavailable:-lmcache:isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:- was_export(checkCycAvailablity/0).
checkCycAvailablity:- (lmcache:isCycAvailable_known;lmcache:isCycUnavailable_known(_)),!.
checkCycAvailablity:- catchv((current_predicate(invokeSubL/2),ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(lmcache:isCycAvailable_known);assert_if_new(lmcache:isCycUnavailable_known(R)))),E,assert_if_new(lmcache:isCycUnavailable_known(E))),!.
*/

% :- gripe_time(60,qcompile(logicmoo(plarkc/'logicmoo_i_cyc_kb_tinykb.pfc'))).
:- baseKB:disable_mpred_expansion.
:- set_prolog_flag(lm_expanders,false).
:- ensure_loaded(logicmoo(plarkc/'logicmoo_i_cyc_kb_preds.pfc')).
:- ensure_loaded(logicmoo(plarkc/'logicmoo_i_cyc_kb_tinykb.pfc')).
:- baseKB:enable_mpred_expansion.
:- set_prolog_flag(lm_expanders,true).

logicmoo_i_cyc_xform:- dmsg("Compiling tinyKB should take under a minute"),
                      gripe_time(60,ensure_loaded(logicmoo(plarkc/'logicmoo_i_cyc_xform.pfc'))).

:- dmsg("Dont forget to ?- logicmoo_i_cyc_xform.").

:- set_prolog_flag(gc,false).
%:- trace,(cyc_to_clif("a",_X)).
%:- break.
:- must(predicate_property(tinyKB9(_),number_of_clauses(_))).
:- if((predicate_property(tinyKB9(_),number_of_clauses(N)),N==0)).
% :- rtrace.
:- mwkb1.
:- wdmsg("Made tinyKB").
:- endif.

end_of_file.



% :-prolog.
