%<?
% ===================================================================
% File 'e2c.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2005/06/06 15:43:15 $
% from Bratko chapter 17 page 455. This comes from Pereira and Warren paper, AI journal, 1980 
/*

THIS IS NOT LOADED DIRRECTLY - INSTEAD USE logicmoo_nl_sem.pl

*/
% ===================================================================

:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- set_prolog_flag(double_quotes, string).
:-set_prolog_flag(double_quotes,string).

dcgIfElse(If,Else,[],[]):-phrase(Else,[],[]),!.
dcgIfElse(If,Else,S,E):- phrase(If,S,E);(phrase(Else,S,E),!).

deftest_p2c("affects it",verb_phrase).
deftest_p2c("to it",verb_phrase).
deftest_p2c("fond of it",verb_phrase).
deftest_p2c("are on the bridge",verb_phrase).
deftest_p2c("appears in at least one movie directed by Sergio Leone",(verb_phrase(A,B,C,D),dcgWordList(List))).
% =======================================================
% Verb Phrase
% =======================================================
verb_phrase(Time,Subj,Obj,Event,CycL9,[],_):-!,fail.

verb_phrase(Time,Subj,Obj,Event,CycLOut,S,E):-verb_phrase0(Time,Subj,Obj,Event,CycLOut,S,E).

or_verb_phrase(Time,Subj,Obj,Event,CycLOut,S,E):-
   findall(CycLO,verb_phrase0(Time,Subj,Obj,Event,CycLO,S,E),List),!,leastOne(List),
   verb_phrase0(Time,Subj,Obj,Event,_CycL,S,E),!,
   CycLOut =.. [or|List].

noun_unit_once(Type,Event,Obj,PrepCycL,VerbObjectCycL)-->noun_unit(Type,Event,Obj,PrepCycL,VerbObjectCycL),{!}.

diTransitiveFrame(Event,Pos,Prep,TemplateOut)-->np_start,{!,fail},dcgConsumeRest.
diTransitiveFrame(Event,Pos,Prep,TemplateOut)--> verb_unit_premods(Time,Event,Template,TemplateOut), diTransitiveFrameNext(Event,Pos,Prep,Template).
diTransitiveFrameNext(Event,Pos,Prep,Template)--> theFrame(Event,['PPCompFrameFn','DitransitivePPFrameType',Prep],Pos,Template).
diTransitiveFrameNext(Event,Pos,Prep,Template)--> theFrame(Event,'DitransitiveNP-NPFrame', Pos,Template).
%TODO? diTransitiveFrameNext(Event,Pos,Prep,Template)--> theFrame(Event,['PPCompFrameFn','TransitivePPFrameType',Prep],Pos,Template).
%TODO? diTransitiveFrameNext(Event,Pos,Prep,Template)--> theFrame(Event,['ParticleCompFrameFn','TransitiveParticleNPFrameType',Prep],Pos,Template)

% Three - Frame DiTransitive
verb_unit_trans(3,Time,Subj,Target,Event,debug(ditrans,Template),Verb,Prep)--> diTransitiveFrame(Event,Verb,Prep,Template).
verb_unit_trans(N,Time,Subj,Target,Event,debug(verb_unit,Template),Verb,Prep)--> verb_unit(N,Time,Subj,Target,Event,Template).

% Three - Frame DiTransitive
/*verb_phrase0(Time,Subj,Obj,Event,debug(ditrans,CycLO)) --> 
   % NEVER
   % QUICKLY
   verb_unit_trans(N,Time,Subj,Target,Event,Template,Verb,Prep),  % GAVE
   verb_postmods(Event,CycL,CycLO),  % QUICKLY
   dcgIfElse(noun_unit_once(Type1,Event,Obj0,VerbObjectCycL,CycL),{VerbObjectCycL=CycL}),  % SOME PEANUTS
   % SLOWLY
   dcgIfElse((dcgAnd(thePOS('Preposition'),theWord(Prep)),{Obj=Obj0,Target=Target0}),{Obj=Target0,Target=Obj0}), % TO
   dcgIfElse(noun_unit_once(Type2,Event,Target0,VerbCycL,VerbObjectCycL),{VerbCycL=VerbObjectCycL}),  % ME
   % ONCE
   {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target,':INDIRECT-OBJECT'-Target],VerbCycL)}. 
*/

verb_phrase0(Time,Subj,Obj,Event,debug(N,CycLO)) --> 
   % NEVER
   % QUICKLY
   verb_unit_trans(N,Time,Subj,Target,Event,Template,Verb,Prep),  % GAVE
   verb_postmods(Event,CycL,CycLO),  % QUICKLY
   %%dcgIfElse((dcgAnd(thePOS('Preposition'),theWord(Prep)),{Obj=Obj0,Target=Target0}),{Obj=Target0,Target=Obj0}), % ON
   dcgIfElse(noun_unit_once(Type1,Event,Obj0,VerbObjectCycL,CycL),{VerbObjectCycL=CycL}),  % SOME PEANUTS
   % SLOWLY
   dcgIfElse((dcgAnd(thePOS('Preposition'),theWord(Prep)),{Obj=Obj0,Target=Target0}),{Obj=Target0,Target=Obj0}), % TO
   dcgIfElse(noun_unit_once(Type2,Event,Target0,VerbCycL,VerbObjectCycL),{VerbCycL=VerbObjectCycL}),  % ME
   % ONCE
   {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target,':INDIRECT-OBJECT'-Target],VerbCycL)}. 

/*
% Verb resort Three - DiTransitive
verb_phrase0(Time,Subj,Obj,Event,debug(res3,PREPL)) --> 
   verb_unit(2,Time,Subj,Target,Event,PrepCycL),
   noun_unit_once(Type1,Event,Obj,VerbTargetCycL,VerbObjectCycL),
   dcgOnce(dcgOptional(thePOS(Prep,'Preposition'))),
   noun_unit_once(Type2,Event,Target,PrepCycL,VerbTargetCycL),
   {prepl(Event,VerbObjectCycL,Prep,PREPL)}.

% Verb resort Two - Transitive
verb_phrase0(Time,Subj,Obj,Event,debug(res2,PREPL)) --> 
   verb_unit(2,Time,Subj,Obj,Event,PrepCycL),
   dcgOnce(dcgOptional(thePOS(Prep,'Preposition'))),
   noun_unit_once(Type,Event,Obj,PrepCycL,VerbObjectCycL),
   {prepl(Event,VerbObjectCycL,Prep,PREPL)}.

% Verb resort One - Intransitive
verb_phrase0(Time,Subj,Obj,Event,debug(resort1,PREPL)) --> 
   verb_unit(2,Time,Subj,Target,Event,PrepCycL),
   dcgOnce(dcgOptional(thePOS(Prep,'Preposition'))),
   {prepl(Event,PrepCycL,Prep,PREPL)}.
*/

verb_phrase0(Time,Subj,Obj,Event,debug(pp,CycLO))--> prepositional_noun_phrase(Event,Obj,Target,NPCycL,Prep).


prepl(Event,VerbObjectCycL,Prep,and(isa(Event,'PrepositionFn'(Prep,Event)),VerbObjectCycL)):-nonvar(Prep),!.
prepl(Event,VerbObjectCycL,Prep,VerbObjectCycL).

deftest_p2c("wont do it",(aux_adv_pop,dcgWordList(List))).
deftest_p2c("will do it",(aux_adv_pop,dcgWordList(List))).
aux_adv_pop --> theCycPOS('BeAux'),{!},aux_adv_pop.
aux_adv_pop --> theCycPOS('DoAux'),{!},aux_adv_pop.
aux_adv_pop --> theCycPOS('AuxVerb'),{!},aux_adv_pop.
aux_adv_pop --> theCycPOS('Adverb'),{!},aux_adv_pop.
aux_adv_pop --> [].

verb_form(Perfect) --> dcgAnd(thePOS('Verb'),theForm(Perfect)).
verb_form(Perfect) --> thePOS('Adverb'),verb_form(Perfect).
verb_form(Perfect) --> thePOS('BeAux'),verb_form(Perfect).


% =======================================================
% Verb Units
% =======================================================
verb_unit(_,Time,Subj,Obj,Event,CycL9,[],_):-!,fail.
verb_unit(_,Time,Subj,Obj,Event,CycL9) --> np_start,{!,fail},dcgConsumeRest.

verb_unit(N,Time,Subj,Obj,Event,CycL9) --> [['DO'|More]],{must(phrase(verb_unit(N,Time,Subj,Obj,Event,CycL9),More)),!}.

verb_unit(N,Time,Subj,Obj,Event,CycL9,[S,T|ART],[Prep|Rest]) :- 
   once(scanDcgUntil(np_start,Before,[T|ART],[Prep|Rest])),
      phrase(verb_unit(N,Time,Subj,Obj,Event,CycL9),[S|Before],[]).

% never ... vp
verb_unit(N,Time,Subj,Obj,Event,never(CycLO))--> theWord('Never-TheWord'),verb_unit(N,Time,Subj,Obj,Event,CycLO).

% was eating
verb_unit(N,Time,Subj,Obj,Event,debug(be,CycLO))--> dcgAnd(thePOS('BeAux'),dcgIgnore(theTense(Time))),
   dcgStartsWith(verb_form('presentParticiple')),verb_unit(N,Time,Subj,Obj,Event,CycLO).

% is on
verb_unit(N,Time,Subj,Obj,Event,debug(be,CycLO))--> dcgAnd(thePOS('BeAux'),dcgIgnore(theTense(Time))),
   dcgStartsWith(thePOS('Preposition')),verb_unit(N,Time,Subj,Obj,Event,CycLO).

% had eaten/written
verb_unit(N,Time,Subj,Obj,Event,CycLO)--> dcgAnd(theWord('Have-TheWord'),dcgIgnore(theTense(Time)),theData(HAVE)),
   dcgStartsWith(verb_form('perfect')),verb_unit(N,Time,Subj,Obj,Event,CycLO).

deftest_p2c("do the can can",verb_unit).
verb_unit(1,Time,Subj,Obj,Event,CycLO)--> dcgAnd(theWord('Do-TheWord'),dcgIgnore(theTense(Time))),
   dcgStartsWith(thePOS('Determiner')),noun_unit_once(Type,Event,Obj,frequentPreformer(Subj,Obj),CycLO).

deftest_p2c("does see",verb_unit). % (1,A,B,C,X,Y)
verb_unit(N,Time,Subj,Obj,Event,CycLO)--> dcgAnd(theWord('Do-TheWord'),dcgIgnore(theTense(Time))),
   dcgStartsWith(dcgAnd(thePOS('Verb'),dcgIgnore(theTense(Time)))),verb_unit(N,Time,Subj,Obj,Event,CycLO).

% appears in
verb_unit(2,Time,Subj,Obj,Event,debug(prepositional,PrepCycL))-->
   prepositional(Event,Subj,Obj,PrepCycL,Prep).

% ...
verb_unit(2,Time,Subj,Obj,Event,debug(prepositional,PrepCycL))-->
   prepositional(Event,Subj,Obj,PrepCycL,Prep).


% sanity test
verb_unit(1,Time,Subj,Obj,Event,VerbObjectCycL)-->{fail},theWord('Sit-TheWord'),theWord('On-TheWord'),
   noun_unit_once(Type,Event,Obj,sittingOn(Subj,Obj),VerbObjectCycL).


%   verb_unit(2,Time,Subj,Obj,Event1,CycL1),theText([(,)]),verb_unit(2,Time,Subj,Obj,Event,CycL2).
%verb_unit(N,Time,Subj,Obj,Event,adv(Mod,CycL9)) -->[['ADVP'|Mod]],verb_unit(N,Time,Subj,Obj,Event,mod(Mod,CycL9)).

deftest_p2c("is good",(verb_unit(N,X,Y,Z,A,B),dcgWordList(List))).
verb_unit(1,Time,Subj,Obj,Event,debug(bea,CycL9)) --> 
   dcgAnd(thePOS('BeAux'),dcgIgnore(theTense(Time))),adjectives_phrase(Event,Subj,occursDurring(Event,Time),CycL9),{!}.

%textCached([fond], [frame, adjSemTrans, 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), 'Adjective', feelsTowardsObject(':NOUN', ':OBLIQUE-OBJECT', 'Affection', positiveAmountOf)]).
%textCached([fond], [frame, adjSemTrans, 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), 'Adjective', feelsTowardsObject(':NOUN', ':OBLIQUE-OBJECT', 'Affection', positiveAmountOf)]).
%textCached('Fond-TheWord', [frame, 'Fond-TheWord', 'Adjective', 'RegularAdjFrame', isa(':NOUN', 'ThingDescribableAsFn'('Fond-TheWord', 'Adjective-Gradable')), adjSemTrans]).
%textCached('Fond-TheWord', [frame, 'Fond-TheWord', 'Adjective', 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), feelsTowardsObjectToDegree(':NOUN', ':OBLIQUE-OBJECT', 'Affection', positiveAmountOf), adjSemTrans]).
%textCached('Fond-TheWord', [frame, 'Fond-TheWord', 'Adjective-Gradable', 'RegularAdjFrame', isa(':NOUN', 'ThingDescribableAsFn'('Fond-TheWord', 'Adjective-Gradable')), adjSemTrans]).
%textCached('Fond-TheWord', [frame, 'Fond-TheWord', 'Adjective-Gradable', 'PPCompFrameFn'('TransitivePPFrameType', 'Of-TheWord'), feelsTowardsObjectToDegree(':NOUN', ':OBLIQUE-OBJECT', 'Affection', positiveAmountOf), adjSemTrans]).

%textCached([notice], [frame, verbSemTrans, 'TransitiveFiniteClauseFrame', 'Verb', and(':CLAUSE', notices(':SUBJECT', ':CLAUSE'))]).


% =======================================================
% Verb PRE Modifiers
% =======================================================
      
verb_unit_premods(Time,Event,ScopeIn,VerbCycL) --> theFrame(Pred,'VerbPhraseModifyingFrame',Adverb,Template),
   {substEach(Template,[':SUBJECT'-Subj,':SCOPE'-Scope,':ACTION'-Event,':NOUN'-Subj,':OBJECT'-Obj],VerbCycL)},
   verb_unit_premods(Time,Event,ScopeIn,Scope).

verb_unit_premods(Time,Event,ScopeIn,not(Scope)) --> theWord('Not-TheWord'),{!},
   verb_unit_premods(Time,Event,ScopeIn,Scope).

verb_unit_premods(Time,Event,ScopeIn,never(Scope)) --> theWord('Never-TheWord'),{!},
   verb_unit_premods(Time,Event,ScopeIn,Scope).

verb_unit_premods(Time,Event,ScopeIn,beIs(Scope)) --> dcgAnd(theWord('BeAux'),dcgIgnore(theTense(Time))),{!},
   verb_unit_premods(Time,Event,ScopeIn,Scope).

verb_unit_premods(Time,Event,ScopeIn,CycLO) --> dcgAnd(theWord('Do-TheWord'),dcgIgnore(theTense(Time))),
   dcgStartsWith(dcgAnd(thePOS('Verb'),dcgIgnore(theTense(Time)))),{!},
   verb_unit_premods(Time,Event,ScopeIn,CycLO).

verb_unit_premods(Time,Event,ScopeIn,and(VerbCycL,Scope)) --> aux_phrase(Time,Subj,Obj,Event,VerbCycL),{!},
   verb_unit_premods(Time,Event,ScopeIn,Scope).

verb_unit_premods(Time,Event,Scope,Scope)-->[].


% =======================================================
% AuxVerbs & Adverbs
% =======================================================
% naux_phrase(Time,Subj,Obj,Event,occursDurring(Event,Time)) --> dcgStartsWith(thePOS('Adverb')).
aux_phrase(Time,Subj,Obj,Event,_)-->np_start,{!,fail},dcgConsumeRest.
aux_phrase(Time,Subj,Obj,Event,occursDurring(Event,Time)) --> dcgAnd(theWord('Have-TheWord'),dcgIgnore(theTense(Time))).
aux_phrase(Time,Subj,Obj,Event,occursDurring(Event,Time)) --> dcgAnd(thePOS('BeAux'),dcgIgnore(theTense(Time))).
aux_phrase(Time,Subj,Obj,Event,occursDurring(Event,Time)) --> dcgAnd(theWord('Be-TheWord'),dcgIgnore(theTense(Time))).
aux_phrase(Time,Subj,Obj,Event,and(occursDurring(Event,Time),bodilyDoer(Subj,Event))) --> dcgAnd(theWord('Do-TheWord'),dcgIgnore(theTense(Time))). 
aux_phrase(Time,Subj,Obj,Event,behavourCapable(Subj,Event)) -->  dcgAnd(theWord('Can-TheModal'),dcgIgnore(theTense(Time))).
%must be modal: aux_phrase(Time,Subj,Obj,Event,behavourCapable(Subj,Event)) -->  dcgAnd(theWord('Can-TheWord'),dcgIgnore(theTense(Time))).
aux_phrase('Past',Subj,Obj,Event,holdsIn('Past',behavourCapable(Subj,Event))) -->  theWord('Could-TheWord').
aux_phrase(Time,Subj,Obj,Event,behavourCapable(Subj,Event)) -->  dcgAnd(thePOS('Modal'),dcgIgnore(theTense(Time))).
%textCached([hardly], [denotation, Pos, 'AlmostNever', 'NonNumericQuantity', 'Frequency', 'Individual']).
aux_phrase(Time,Subj,Obj,Event,isa(Event,Term)) -->dcgAnd(thePOS('Adverb'),theIsa(Event,Term,'Individual')).
aux_phrase(Time,Subj,Obj,Event,isa(Event,'AdverbFn'(Word))) --> dcgAnd(thePOS('Adverb'),theWord(Word)).

% =======================================================
% Verb POST Modifiers
% =======================================================
% cut right before determiner
verb_unit_postmods(Event,CycL,CycL) --> dcgStartsWith( thePOS('Determiner')),[],{!}.
verb_unit_postmods(Event,CycL,and(Truth,CycL)) -->aux_phrase(Time,Subj,Obj,Event,Truth).
verb_unit_postmods(Event,CycL,implies(occursDuring(Event,Mod),holdsIn(Event,CycL))) --> time_phrase(Event,Mod).
verb_unit_postmods(Event,CycL,CycL) --> [].

verb_postmods(Event,CycL,CycLO)-->verb_unit_postmods(Event,CycL,CycLO).
%verb_postmods(Event,CycL,CycLO)-->dcgAnd(dcgNot(thePOS('Determiner')),verb_unit_postmods(Event,CycL,CycLO)).

% Today
time_phrase(Event,Mod) --> theIsa(Event,Mod,'TimePoint').
% Monday
time_phrase(Event,Mod) --> theIsa(Event,Mod,'CalendarDay').
time_phrase(Event,Mod) --> theIsa(Event,Mod,'TemporalObjectType').
time_phrase(Event,Mod) --> theIsa(Event,Mod,'TimeInterval').


%time_phrase(Event,Mod) --> 


% =======================================================
% Verbs Expression Resorts
% =======================================================
%% findall(Y,p2c("go to it",verb_unit(2,A,B,C,X,Y)),L),length(L,S).  ==> S = 353

% =======================================================
% Verb+Prep Phrases
% =======================================================
         
% Two
verb_prep(2,Event,Subj,Obj,PTAG,Target,VerbObjectCycLO)-->
        theFrame(Event,['PPCompFrameFn','TransitivePPFrameType',Prep],Verb,Template),
      {phrase(thePrepWord(Prep),PTAG)},
     verb_postmods(Event,VerbCycL,VerbObjectCycLO),
     {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}.

deftest_p2c("leaned",theFrame(Event,['PPCompFrameFn','DitransitivePPFrameType',Prep],'Verb',Template)).
% Three
verb_prep(3,Event,Subj,Obj,PTAG,Target,VerbObjectCycLO)-->
        {phrase(thePrepWord(Prep),PTAG)},
     theFrame(Event,['PPCompFrameFn','DitransitivePPFrameType',Prep],Verb,Template),%{trace},
     verb_postmods(Event,VerbCycL,VerbObjectCycLO),
     {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}.


% Three
verb_prep(3,Event,Subj,Obj,PTAG,Target,and(CycL,VerbCycL))-->
        theFrame(Event,'DitransitiveNP-InfinitivePhraseFrame','Verb',Template),%{true},
        verb_postmods(Event,VerbObjectCycL,VerbObjectCycLO),
     {phrase(thePrepWord(Prep),PTAG)},
        {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}.


verb_prep(2,Event,Subj,Obj,PTAG,Target,VerbObjectCycL)-->
         theWord('Sit-TheWord'),theWord('On-TheWord'),
         noun_unit_once(Type,Event,Obj,sittingOn(Subj,Obj),VerbObjectCycL).

% more frames UnderstoodReciprocalObjectFrame MiddleVoiceFrame 

verb_unit(N,Time,Subj,Obj,Event,debug(verb_prep,VerbObjectCycLO))--> 
           dcgStartsWith(dcgMapOne(dcgAnd(thePOS('Preposition'),theGData(PTAG)))),
           %{trace},
           verb_prep(N,Event,Subj,Obj,PTAG,Target,VerbObjectCycLO).

% =======================================================
% Intransitive Verbs + verb_unit
% =======================================================
intrans_modifiers(CycL,CycL) --> [].
verb_unit_not_yet(1,Time,Subj,Obj,Event,and(VerbObjectCycL,CycL)) --> 
     verb_intransitive(Time,Subj,Obj,Event,CycL),
     optionalVerbGlue(_AND_OR),
     verb_unit(2,Time,Target,Obj,Event,PrepCycL),
     noun_unit_once(Type,Event,Obj,PrepCycL,VerbObjectCycL).

% Intransitive Verbs One
verb_unit(1,Time,Subj,Obj,Event,debug(intrans,CycLO)) --> 
     verb_intransitive(Time,Subj,Obj,Event,CycL),
        verb_postmods(Event,CycL9,CycLO),
     intrans_modifiers(CycL,CycL9).


verb_intransitive(Time,Subj,Obj,Event,and(preActors(Event,Subj),CycL)) --> 
      theFrame(Event,'MiddleVoiceFrame','Verb',Template),
      {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event],CycL)}.

verb_intransitive(Time,Subj,Obj,Event,and(preActors(Event,Subj),CycL)) --> 
      theFrame(Event,'ParticleCompFrameFn'('IntransitiveParticleFrameType',Prep),Verb,Template),
      thePrepWord(Prep),{!,substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event],CycL)}.

verb_intransitive(Time,Subj,Obj,Event,and(preActors(Event,Subj),CycL)) --> 
      theFrame(Event,'IntransitiveVerbFrame','Verb',Template),
      {!,substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event],CycL)}.


% =======================================================
% Verbs Resorts
% =======================================================

%verb_unit(2,Time,Subj,Obj,Target,Event,and(hasVAttributes(Event,AVP),CycL5)) --> auxV(AVP),verb_unit(2,Time,Subj,Obj,Target,Event,CycL5).
%verb_unit(2,Time,Subj,Obj,Target,Event,and(hasVAttributes(Event,AVP),CycL5)) --> adv(AVP),verb_unit(2,Time,Subj,Obj,Target,Event,CycL5).

% verb_unit(2,Time,Subj,Obj,Event,Scope) --> 
%       verb_unit1(Time,Subj,Obj,Target,Event,Scope).

verb_unit(2,Time,Subj,Obj,Event,debug(unit1,Scope)) --> 
      verb_unit_premods(Time,Event,ScopeIn,Scope),
       verb_unit1(Time,Subj,Obj,Target,Event,ScopeIn).

verb_unit(3,Time,Subj,Obj,Event,debug(unit2,VerbCycL))-->diTransitiveFrame(Event,'Verb',Prep,Template),
          {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target,':INDIRECT-OBJECT'-Target],VerbCycL)}. 

% eaten by (switch around subject/object)
verb_unit1(Time,Subj,Obj,Target,Event,CycLO)-->
            verb_resort1(2,Time,Obj,Subj,Target,Event,CycL),theWord('By-TheWord'),{!},
                    verb_postmods(Event,CycL,CycLO).

verb_unit1(Time,Subj,Obj,Target,Event,CycLO)-->
            verb_resort1(2,Time,Subj,Obj,Target,Event,CycL),
                    verb_postmods(Event,CycL,CycLO).

verb_unit1(Time,Subj,Obj,Target,Event,CycLO)-->
            verb_resort1(2,Time,Subj,Obj,Target,Event,CycL1),
            verb_unit(2,Time,Obj,Target,Event,CycL2),
                    verb_postmods(Event,and(CycL1,CycL2),CycLO).


verb_unit1(Time,Subj,Obj,Target,Event,and(isa(Event,Isa),subjObjSwitchError)) --> dcgBoth((event_verb(Event,Isa),thePOS('BeAux')),theName(Event)).
verb_unit1(Time,Subj,Obj,Target,Event,isa_event_verb(Event,Isa)) --> dcgBoth(event_verb(Event,Isa),theName(Event)).

event_verb(Event,Collection,[],_):-!,fail.
event_verb(Event,Collection) --> np_start,{!,fail},dcgConsumeRest.
event_verb(Event,Collection) --> theIsa(Event,Collection,'EventType'),{!}.
event_verb(Event,'VerbAPFn'(V,P))--> thePOS(V,'Adverb'),thePOS(P,'Preposition'),{!}.
event_verb(Event,'VerbVPFn'(V,P))--> thePOS(V,'Verb'),thePOS(P,'Preposition').
event_verb(Event,'VerbFn'(V))-->thePOS(V,'Verb'),{!}.

verb_resort1(_,Time,Subj,Obj,Target,Event,CycL) --> thePOS('Determiner'),{!,fail},dcgConsumeRest.

verb_resort1(N,Time,Subj,Obj,Target,Event,VerbCycL) --> theWord('To-TheWord'),
      verb_resort1(N,Time,Subj,Obj,Target,Event,VerbCycL).

% Two
verb_resort1(2,Time,Subj,Obj,Target,Event,VerbCycL) --> 
           theFrame(Event,['ParticleCompFrameFn','TransitiveParticleNPFrameType',Prep],'Verb',Template),
           thePrepWord(Prep), %{trace},
     verb_postmods(Event,VerbCycL,VerbObjectCycLO),
     {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}.

% Three
hide_verb_resort1(2,Time,Subj,Obj,Target,Event,VerbCycL) --> 
        theFrame(Event,'DitransitiveNP-NPFrame','Verb',Template),%{true},
        {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}. 

% Two
verb_resort1(2,Time,Subj,Obj,Target,Event,and(isa(Event,'Event'),VerbCycL)) --> 
     theFrame(Event,'TransitiveNPFrame','Verb',Template),
     {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}.
                                     
verb_resort1(2,Time,Subj,Obj,Target,Event,'is-Underspecified'(Subj,Obj)) --> dcgOr(thePOS('BeAux'),theWord('Am-TheWord'),theWord('Be-TheWord')).

verb_resort1(2,Time,Subj,Obj,Target,Event,and(isa(Event,EventType),occursDuring(Event,When),preActors(Event,Subj),actors(Event,Obj))) -->  
      dcgAnd(theIsa(Event,EventType,'DurativeEventType'),theTense(When),theName(Event)).

verb_resort1(2,Time,Subj,Obj,Target,Event,and(occursDuring(Event,When),holdsIn(Event,[Pred,Subj,Obj]))) --> 
      dcgAnd(theIsa(Event,Pred,'TruthFunction'),theTense(When),theName(Event)).

/* UNCOMMENT */
verb_resort1(2,Time,Subj,Obj,Target,Event,holdsIn(Event,PrepCycL)) --> prepositional(Event,Subj,Obj,PrepCycL,Prep).



% Three
verb_resort1(2,Time,Subj,Obj,Target,Event,and(VerbCycL,frameType(Event,TemplateType))) --> 
        theFrame(Event,TemplateType,'Verb',Template), %%{writeq(TemplateType),not(member(TemplateType,[denotation])),trace},
        {substEach(Template,[':SUBJECT'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Target,':OBLIQUE-OBJECT'-Target],VerbCycL)}. 

verb_resort1(2,Time,Subj,Obj,Target,Event,debug(intrans,CycL)) -->verb_intransitive(Time,Subj,Obj,Event,CycL).

verb_resort1(2,Time,Subj,Obj,Target,Event,implies(isa(Event,'VerbFn'(Word)),eventSOT(Event,Subj,Obj,Time))) --> 
     dcgAnd(thePOS(Word,'Verb'),theName(Event),theTense(Time)).

optionalVerbGlue(and) --> theText([and]).
optionalVerbGlue(or) --> theText([or]).
optionalVerbGlue(and) --> theText([,]).
%optionalVerbGlue(and) --> [].


%>
