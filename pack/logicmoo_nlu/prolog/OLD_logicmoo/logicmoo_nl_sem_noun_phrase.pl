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

:- ensure_loaded(logicmoo_nl_sem_rel_clause).

% =======================================================
% Nouns Phrases
% =======================================================

theRestText(Text) -->theText(Text1),theRestText1(Text2),{flatten([Text1,Text2],Text),!}.
theRestText1(Text) -->theText(Text1),theRestText1(Text2),{flatten([Text1,Text2],Text),!}.
theRestText1([]) -->[].

% =======================================================
% Nouns Units Each
% =======================================================
:-dcg_CannotMatchEmpty(noun_phrase(Event,Subj,CycLIn,CycLO)).

%noun_phrase(Event,Subj,CycLIn,and(CycL0M,CycLO,subEvents(Event,Event2)))-->
%   noun_unit(Type,Event,Subj,CycLIn,CycLO),theWord('That-TheWord'),verb_unit(Time,Event2,Subj,Obj,CycL0M).
noun_phrase(Event,Subj,CycLIn,CycLO)-->noun_unit(Type,Event,Subj,CycLIn,CycLO).

or_noun_phrase(Event,Subj,CycLIn,CycLOut):-
   findall(CycLO,noun_unit(Type,Event,Subj,CycLIn,CycLO),List),!,leastOne(List),
   noun_unit(Type,Event,Subj,CycLIn,_CycLO),!,
   CycLOut =.. [or|List].


%%%%%% theNounFrame(Subj,GenitiveFrame,CountNoun,Template)--> dcgStartsWith(theWord('Enter-TheWord')),{trace,!,fail}.
theNounFrame(Subj,GenitiveFrame,CountNoun,Template)-->cant_be_adjnoun,{!,fail}.
theNounFrame(Subj,GenitiveFrame,CountNoun,Template)--> theFrame(Subj,GenitiveFrame,CountNoun,Template).

%%% the next when when uncommented stops the "two books on the shelf" :(
%%%theNounIsa(Subj,AttribProp,FirstOrderCollection)-->cant_be_adjnoun,{!,fail}.

%%theNounIsa(Subj,AttribProp,FirstOrderCollection) --> dcgStartsWith(theWord('Enter-TheWord')),{trace,!,fail}.
theNounIsa(Subj,AttribProp,FirstOrderCollection) --> theIsa(Subj,AttribProp,FirstOrderCollection).

:-dcg_CannotMatchEmpty(noun_unit(Type,Event,Subj,CycLIn,CycLO)).
noun_unit(Type,Event,Subj,CycLIn,CycLO,[S|TART],E):-
   noun_unit_int(Type,Event,Subj,CycLIn,CycLO,[S|TART],E),
   phrase(dcgTextList(List),[S|TART],[]),flatten(List,ListName),
   suggestVar(ListName,Subj),
   %suggestVar('Noun-Unit-Event',Event),suggestVar('Noun-Unit-CycLIn',CycLIn),suggestVar('Noun-Unit-Type',Type),
   true.


deftest_p2c("$ 1",noun_unit(Type,Event,Subj,CycLIn,CycLOut)).
deftest_p2c("my $ 1",noun_unit(Type,Event,Subj,CycLIn,CycLOut)).
deftest_p2c("my car",noun_unit(Type,Event,Subj,CycLIn,CycLOut)).
deftest_p2c("I",noun_unit(Type,Event,Subj,CycLIn,CycLOut)).
deftest_p2c("me",noun_unit(Type,Event,Subj,CycLIn,CycLOut)).
deftest_p2c("large number of birds",noun_unit(X,Y,Z,A,B)).
deftest_p2c("United States of America",noun_unit(X,Y,Z,A,B)).
deftest_p2c("United States of Americana",noun_unit(X,Y,Z,A,B)).

%asNounUnit(ObjType,T1,D1,W1,Event,Subj,CycLO,CycL):-append([A|Djs],[Noun],D1),
%         asNounQualifiers([A|Djs],Event,Subj,CycLQ),
%         asNoun([Noun],Event,Subj,CycLN).

asNounUnit(_ObjType,T1,D1,_W1,Event,Subj,CycLO,CycL):-phrase(noun_unit_int(T1,Event,Subj,CycLO,CycL),D1),!.
asNounUnit(_ObjType,_T1,_D1,W1,_Event,Subj,CycLO,and(CycLO,'doom:descriptionStrings'(Subj,string(W1)))):-!.

:-dcg_CannotMatchEmpty(noun_unit_int(Type,Event,Subj,CycLIn,CycLO)).


deftest_p2c("the United States of America",noun_unit(X,Y,Z,A,B)).
noun_unit_int(Type,Event,Subj,CycLIn,and(denotationRelated(Subj,Term),CycLIn),[S,T|ART],E):-
   getText(S,[Txt1]),getText(T,[Txt2]),
   textCached(Txt1, [multiwordText, [Txt1, Txt2|MORE],Term]),
   phrase(dcgTextList(MORE),ART,E).
   
deftest_p2c("books about men",(noun_unit(X,Y,Z,A,B),dcgWordList(List))).


%textCached('Book-TheWord', [wsframe, 'Book-TheWord', 'TheList'(string('"about"'), string('"men"')), 'RegularNounFrame', 'TheOneOf'(':NOUN', isa(':NOUN', 'TextualMaterialAboutFn'('AdultMaleHuman'))), 'CountNoun']).
/*
noun_unit_int(Type,Event,Subj,CycLIn,and(equals(Subj,CycLO),CycLIn),START,E):-
   theWord(TheWord,START,ART),
   textCached(TheWord, [wsframe, TheWord, TheList, 'RegularNounFrame', Template, 'CountNoun']),
   once(theListTemplate(TheList,TheListTemplate)),
   phrase(dcgSeq(TheListTemplate),ART,E),
   substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Obj,':OBLIQUE-OBJECT'-Obj],CycLO).
*/


noun_unit_int(Type,Event,Subj,CycLIn,and(equals(Subj,CycLO),CycLIn))-->
   theNounFrame(Event,'RegularNounFrame','CountNoun',Template),
   {substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Obj,':OBLIQUE-OBJECT'-Obj],CycLO)}.


% this prefers to cut noun units shorter than "offered" at least once "not presentParticiple" because we want relatinal clauses
% however its currently cutting things *too short* (So I added hide_)
hide_noun_unit_int(Type,Event,Subj,CycLIn,CycLO,S,[Verb|Rest]) :- 
   scanDcgUntil(dcgAnd( thePOS('Verb'),dcgNot(theForm('presentParticiple'))),Before,S,[Verb|Rest]),
   phrase(noun_unit_int(Type,Event,Subj,CycLIn,CycLO),Before,[]).

deftest_p2c("outside of it",noun_unit(A,B,C,X,Y)).
deftest_p2c("inside of it",noun_unit(A,B,C,X,Y)).
deftest_p2c("next to it",noun_unit(A,B,C,X,Y)).
deftest_p2c("at least three cars drove south",(noun_unit_int(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("at least three cars driving south",(noun_unit_int(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("the boy than whom I am smaller",(noun_unit_int(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("the boy whose daughter is ill",(noun_unit_int(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("the boy to whom I have written",(noun_unit_int(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("they that were being good",(noun_unit(Subj,CycL9,More,'True',B))).

noun_unit_int(Type,Event,Subj,CycLIn,CycLO,[S,T|ART],E) :- 
   scanDcgUntil(relationl_clause_head,Before,[T|ART],[Verb|Rest]),
   phrase(noun_unit_int(Type,Event,Subj,CycLIn,CycLO1),[S|Before],[]),
   (rel_clause(Event2,Subj,CycLO2,[Verb|Rest],E)
     ->
     CycLO = and(CycLO1,CycLO2,subEvents(Event,Event2))
     ;
     (CycLO = CycLO1,E=[Verb|Rest])).

% det noun
noun_unit_int(Type,Event,Subj,CycLIn,CycL9,[S|Data],E) :- phrase(np_start,[S]),det_noun_unit(Type,Event,Subj,CycLIn,CycL9,[S|Data],E),!.

%noun_unit_int(Type,Event,Subj,CycLIn,_)-->dcgAnd(thePOS('Verb'),dcgNot(theForm('presentParticiple'))),{!,fail}.


deftest_p2c("the can would not do",(noun_unit(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("the can can not do",(noun_unit(X,Y,Z,A,B),dcgWordList(List))).
deftest_p2c("Clint Eastwood appears in at least one movie directed by Sergio Leone",(noun_phrase(A,B,C,D),dcgWordList(List))).


noun_unit_int(_Adjective,Event,Subj,CycLIn,CycLO) --> 
         theNounFrame(Event, 'PPCompFrameFn'('TransitivePPFrameType',Prep), 'Adjective',Template),thePrepWord(Prep),
         {substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event,':OBJECT'-Obj,':INF-COMP'-Obj,':OBLIQUE-OBJECT'-Obj],VerbCycL)},
         noun_unit_int(_Type,Event,Obj,and(CycLIn,VerbCycL),CycLO).

deftest_p2c("two books on a shelf",(noun_unit(X,Y,Z,A,B),dcgWordList(List))).
hide_for_redo_noun_unit_int(Type,Event,Subj,CycLIn,CycLO,[S,T|ART],E) :- 
   scanDcgUntil(thePOS('Preposition'),Before,[T|ART],[Prep|Rest]),
   phrase(noun_unit_int(Type,Event,Subj,CycLIn,CycLO1),[S|Before],[]),
   (prepositional_noun_phrase(Event,Subj,_Target,CycLO2,_PrepWord,[Prep|Rest],E) 
     ->
     CycLO = and(CycLO1,CycLO2)
     ;
     (CycLO = CycLO1,E=[Prep|Rest])).
   

%noun_unit_int('DET',Event,Subj,CycLIn,CycLO) --> 


%noun_unit_int(Type,Event,Subj,CycL5,and(possessiveRelation(DET,Subj),CycL9)) -->thePOS(DET,'Possessive'),{!},noun_unit_int(Type,Event,Subj,CycL5,CycL9).

possessor_phrase(Owner,Owned,CycL5,CycL) --> dcgStartsWith(thePOS('Possessive')),subject_noun(Event,Owner,CycL5,CycL).
possessor_phrase(Owner,Owned,CycL5,CycL) --> subject_noun(Event,Owner,CycL5,CycL),theGText('\''),theGText('s').

noun_unit_int(Type,Event,Subj,CycL5,and(possessiveRelation(Owner,Subj),CycL9)) -->possessor_phrase(Owner,Subj,CycL8,CycL9),noun_unit_int(Type,Event,Subj,CycL5,CycL8).

% inside/outside [of] it
deftest_p2c("inside of it",(noun_unit(A,B,C,X,Y))).


% $100
noun_unit_int(Type,Event,Subj,CycLIn,and(equals(Subj,'DollarFn'(Num)),CycLIn)) --> dollarSign,decimalNumber(Num).
% non number pronoun "I" 
noun_unit_int(Type,Event,Subj,CycLIn,CycL9,[S|Data],E) :- phrase(thePOS('Pronoun'),[S]),not(phrase(theNounIsa(Subj,_,'Number-General'),[S])), pronoun_unit(Type,Event,Subj,CycLIn,CycL9,[S|Data],E),!.
% he/BillClinton/joe's her  
noun_unit_int(Type,Event,Subj,CycLIn,implies(CycL,CycLIn)) --> nondet_noun_unit(Type,Event,Subj,CycL).
% green thingy-like substance
noun_unit_int(Type,Event,Subj,CycLIn,implies(CycL,CycLIn)) --> adjective_noun_unit(Type,Event,Subj,CycL).
   
%noun_unit_int(Type,Event,Subj,CycLIn,and(equals(Subj,'DollarFn'(Num)),CycLIn)) --> {!,fail}.

%noun_unit_int(Type,Event,Subj,CycL5,whp(DET,CycL9)) -->thePOS(DET,'WHPronoun'),{!},vp(Event,Subj,Obj,CycL5,CycL9).
%& TODO noun_unit_int(Type,Event,Subj,CycL5,implies(thereExists(Event2,and(actorsIn(Event2,DET,Subj),CycL9)),CycL5)) -->thePOS(DET,'WHPronoun'),vp(Event2,Subj,Obj,CycL9).
%noun_unit_int(Type,Event,Subj,CycL5,and(isa(Subj,'QuantifierFn'(DET)),CycL9)) -->thePOS(DET,'Quantifier'),{!},noun_unit_int(Type,Event,Subj,CycL5,CycL9).
noun_unit_int(Type,Event,Subj,CycL5,forAll(Subj,CycL9)) -->thePOS(DET,'Quantifier'),noun_unit_int(Type,Event,Subj,CycL5,CycL9).
%noun_unit_int(Type,Event,Subj,CycL5,and(isa(Subj,'NumberFn'(DET)),CycL9)) -->thePOS(DET,'Number'),{!},noun_unit_int(Type,Event,Subj,CycL5,CycL9).
noun_unit_int(Type,Event,Subj,CycL5,thereExistExactly(DET,Subj,CycL9)) -->thePOS(DET,'Number'),noun_unit_int(Type,Event,Subj,CycL5,CycL9).
%noun_unit_int(Type,Event,Subj,CycL5,and(isa(Subj,'DeterminerFn'(DET)),CycL9)) -->thePOS(DET,'Determiner'),{!},noun_unit_int(Type,Event,Subj,CycL5,CycL9).
noun_unit_int(Type,Event,Subj,CycL5,thereExists(Subj,CycL9)) -->thePOS(DET,'Determiner'),noun_unit_int(Type,Event,Subj,CycL5,CycL9).
%noun_unit_int(Type,Event,Subj,CycL5,and(isa(Subj,'AdjectiveFn'(DET)),CycL9)) -->thePOS(DET,'Adjective'),{!},noun_unit_int(Type,Event,Subj,CycL5,CycL9).
noun_unit_int(Type,Event,Subj,CycL5,and('isa'(Subj,'AdjectiveFn'(DET)),CycL9)) -->thePOS(DET,'Adjective'),noun_unit_int(Type,Event,Subj,CycL5,CycL9).
noun_unit_int(Type,Event,Subj,CycL5,CycL) -->subject_noun(Event,Subj,CycL5,CycL).
noun_unit_int(Type,Event,Subj,CycL5,CycL) -->prepn_nunit(Event,Subj,CycL5,CycL).%{trace}.

%noun_unit_int(Type,Event,Subj,CycLIn,CycL9) --> dcgAnd(scanNounUnit,noun_phrase_units(Event,Subj,CycL0,CycL9)),{!},rel_clause(Event,Subj,CycLIn,CycL0),{!}.

%%%%%%% subject_noun(Event,Subj,CycL5,_)--> cant_be_adjnoun,{!,fail}.

%subject_noun(Event,Subj,CycL5,and(isa(Subj,'PronounFn'(DET)),CycL5)) -->thePOS(DET,'Pronoun'),{!}.
%subject_noun(Event,Subj,CycL5,and(isa(Subj,'ProperNounFn'(DET)),CycL9)) -->thePOS(DET,'ProperNoun'),{!},noun_unit_int(Type,Event,Subj,CycL5,CycL9).
subject_noun(Event,Subj,CycL5,and(isa(Subj,'ProperNounFn'(DET,PN)),CycL5)) -->thePOS(DET,'ProperNoun'),dcgAnd(thePOS(PN,'Noun'),theName(Subj)).
%subject_noun(Event,Subj,CycL5,and(isa(Subj,'NounFn'(PN)),CycL5)) -->thePOS(PN,'Noun').

subject_noun(Event,Subj,CycL5,np_and(CycL,CycL5)) -->dcgAnd(dcgTemplate('NPTemplate',VarName,CycL),theName(Subj)).

% last resort
subject_noun(Event,Subj,CycL5,and(isa(Subj,'NounFn'(PN)),CycL5)) -->dcgBoth(thePOS(PN,'Noun'),theName(Subj)).


% =======================================================
% Proper Noun Phrases as nondet_noun_unit
% =======================================================      
%%%%%%% nondet_noun_unit(Type,Event,Subj,CycL9)-->cant_be_adjnoun,{!,fail}. 
% his thing
nondet_noun_unit(Type,Event,Subj,CycL9) --> 
         dcgSeq(dcgAnd(thePOS('Possessive'),theName(HIS)),noun_unit_int(Type,Event,Subj,possesiveRelation(HIS,Subj),CycL9)).

% President Clinton
nondet_noun_unit(Type,Event,Subj,and(CycL1,CycL2)) --> person_title(Subj,CycL1),proper_name(Subj,CycL2).

% Clinton
nondet_noun_unit(Type,Event,Subj,CycL) --> proper_name(Subj,CycL).

% =======================================================
% Proper Noun Phrases as proper_name
% =======================================================      

% Bill Clinton
proper_name(Subj,equals(Subj,'BillClinton')) --> theTerm('BillClinton').

% Texas
proper_name(Subj,equals(Subj,CycLTerm)) -->theNounIsa(Subj,CycLTerm,'Agent-Generic').

% Adjectives that are capitalized
% fail USS Enterprise 
% proper_name(Subj,_)-->thePOS('Adjective'),{trace,!,fail}.

% Van Morison
proper_name(Subj,properNameString(Subj,string(Name))) --> dcgSeq(2,[capitalized(Text1),capitalized(Text2)]),{suggestVar(Text1,Subj),flatten([Text1,Text2],Name)}.
% Fido
proper_name(Subj,properNameString(Subj,string(Name))) --> capitalized(Name),{suggestVar(Name,Subj)}.

% president
person_title(Subj,Term)--> theNounIsa(Subj,Term,'PersonTypeByActivity').

% =======================================================
% Pronoun Phrases as noun_unit_int
% =======================================================

% Her His Your Our Their
pronoun_unit(Type,Event,Subj,CycL,and(controls(HIS,Subj),CycL9)) --> %                   {true}
      dcgSeq(dcgAnd(thePOS('Possessive'),thePOS('Pronoun'),pronoun(HIS,CycLI,CycL9)),noun_unit_int(Type,Event,Subj,CycL,CycLI)).
% My 
pronoun_unit(Type,Event,Subj,CycL,and(controls(HIS,Subj),CycL9)) --> %                   {true}
      dcgSeq(dcgAnd(theWord('My-TheWord'),pronoun(HIS,CycLI,CycL9)),noun_unit_int(Type,Event,Subj,CycL,CycLI)).

pronoun_unit(Type,Event,Subj,CycLIn,CycL9,[S|Data],E) :- not(phrase(thePOS('Postdeterminer'),[S])),!,nonpostdet_pronoun_unit(Type,Event,Subj,CycLIn,CycL9,[S|Data],E).

% Him She They
nonpostdet_pronoun_unit(Type,Event,Subj,CycL,thereExists(Subj,CycL9)) --> pronoun(Subj,CycL,CycL9).


pronoun_unit(Type,Event,Subj,CycL,thereExists(Subj,CycL9)) --> pronoun(Subj,CycL,CycL9).

%%%%%%% pronoun(Subj,CycL,_) --> cant_be_adjnoun,{!,fail}.

pronoun(Subj,CycL,and(isa(Subj,'Person'),CycL))  --> theWord('I-TheWord'),{suggestVarI('Speaker',Subj)}.
pronoun(Subj,CycL,and(isa(Subj,'Person'),CycL))  --> theWord('You-TheWord'),{suggestVarI('TargetAgent',Subj)}.
%pronoun(Subj,CycL,and(isa(Subj,'Person'),CycL))  --> theWord('They-TheWord'),{suggestVarI('TargetAgent',Subj)}.
pronoun(Subj,CycL,and(isa(Subj,'Male'),CycL))  --> theWord('He-TheWord'),{suggestVarI('he',Subj)}.
pronoun(Subj,CycL,and(isa(Subj,'Female'),CycL))  --> theWord('She-TheWord'),{suggestVarI('she',Subj)}.
pronoun(Subj,CycL,and(isa(Subj,'Artifact-NonAgentive'),CycL))  --> theWord('It-TheWord'),{suggestVarI('TargetThing',Subj)}.
%pronoun(Subj,CycL,and(Constraints,CycL)) --> dcgAnd(thePOS(Word,'IndefinitePronoun'),theConstraints(Subj,Constraints)).
pronoun(Subj,CycL,and(Constraints,CycL)) -->  theText([Text]),{pronounConstraints([Text],Subj,Constraints)}.

% who/he/them
pronoun_hide(Subj,CycL,and(equals(Subj,NPTemplate),CycL)) --> theWord(TheWord), {
                     textCached(TheWord, [denotation, _SubjectPronoun,Template|_]),member(_SubjectPronoun,['SubjectPronoun','ObjectPronoun','AgentiveNoun']),
                     subst(Template,':NOUN',Subj,NPTemplate)}.

pronoun(Subj,CycL,and(NPTemplate,CycL)) --> theWord(TheWord), {
                     textCached(TheWord, [frame, TheWord, 'WHPronoun', 'RegularNounFrame',Template|_]),
                     subst(Template,':NOUN',Subj,NPTemplate)}.

pronounConstraints(Text,Subj,equals(Subj,NartEq)):-textCached(Text,[denotation,Pos,NartEq|Types]),suggestVarI(Text,Subj),!.
pronounConstraints(Text,Subj,CycL):-constraintsOnIsa(Text,Subj,CycL),!.

theConstraints(Subj,isa(Subj,ColType)) --> theText([Text]),{constraintsOnIsa([Text],Subj,ColType)}.

constraintsOnIsa(Text,Subj,CycL):-
      suggestVarI(Text,Subj),
      findall(and(isa(Subj,Eq),isa(Subj,Cols)),(textCached(Text,[denotation,Pos,Eq|Types]),joinCols('CollectionUnionFn',Types,Cols)),[IS|AS]),
      CycL=..['#$or'|[IS|AS]].


% =======================================================
% Count and Mass Nouns as det_noun_unit
% =======================================================      

          
/*


% det_noun_unit(Type,Event,Subj,CycL,thereExists(Subj,and(isa(Subj,CycL0),CycL))) --> dcgAnd(scanNounUnit,theNounIsa(Subj,CycL0,'SpatialThing-Localized'),theNounIsa(Subj,CycLTerm,'Collection')).

% the green party
det_noun_unit(Type,Event,Subj,equals(Subj,CycLTerm)) --> dcgOptional(theWord('The-TheWord')),proper_noun(CycLTerm).
% the usa, the green party
proper_noun(CycLTerm)--> dcgAnd(scanNounUnit,theNounIsa(Subj,CycLTerm,'SpatialThing-Localized'),theNounIsa(Subj,CycLTerm,'Individual')).
% the President
det_noun_unit(Type,Event,Subj,isa(Subj,CycLTerm)) --> dcgOptional(theWord('The-TheWord')),person_title(Subj,CycLTerm).

%% dog, person    AuxVerb
det_noun_unit(Type,Event,Subj,CycL,and(CycL,isa(Subj,Type))) --> theNounIsa(Subj,Type,'StuffType').

*/

%det_noun_unit(Type,Event,Subj,CycLVerbInfo,('thereExists'(Subj,'and'(AttribIsa,CycLVerbInfo)))) -->  
%   adjectives_phrase(Event,Subj,AttribIsa1,AttribIsa),collection_noun(Event,Subj,CycL0),adjectives_phrase(Event,Subj,CycL0,AttribIsa1).
%det_noun_unit(Type,Event,Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop12,CycL1,CycL),collection_noun(Event,Subj,Prop1),rel_clause(Event,Subj,Prop1,Prop12).
%det_noun_unit(Type,Event,Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop1,CycL1,CycL),collection_noun(Event,Subj,Prop1).

det_noun_unit(Type,Event,Subj,CycLIn,CycL9) --> %{trace},
         quant_phrase(Subj,AttribIsa,CycLIn,CycL9),{!},
         adjective_noun_unit(Type,Event,Subj,AttribIsa).

%adjective_noun_unit(Type,Event,Subj,AttribIsa) --> collection_noun(Event,Subj,IsaInfo),thePOS('Preposition-Of'),noun_unit_int(Type,Event,Subj,IsaInfo,AttribIsa).

%%%%%%% adjective_noun_unit(Type,Event,Subj,AttribIsa)--> cant_be_adjnoun,{!,fail}.
adjective_noun_unit(Type,Event,Subj,AttribIsa) --> {true}, 
            adjectives_phrase(Event,Subj,IsaInfo,AttribIsa),
            collection_noun(Event,Subj,IsaInfo).

% =======================================================
% Adjective Phrases
% =======================================================
%textCached([of], [frame, prepSemTrans, 'Post-NounPhraseModifyingFrame', 'Preposition', possessiveRelation(':OBJECT', ':NOUN')]).

cant_be_adjnoun --> thePOS('PossessivePronoun-Pre'),{!,fail}.
cant_be_adjnoun --> dcgStartsWith(cant_be_adjnoun0),{!},dcgConsumeRest.
cant_be_adjnoun0 --> thePOS('Preposition').
cant_be_adjnoun0 --> theCycPOS('Determiner').
%cant_be_adjnoun0 --> thePOS('WHDeterminer').
cant_be_adjnoun0 --> theWord('The-TheWord').
cant_be_adjnoun0 --> theWord('Enter-TheWord').
cant_be_adjnoun0 --> dcgAnd(thePOS('Verb'),theForm('presentParticiple')).

conjuntive_and --> theText([and]).
conjuntive_and --> theText([(',')]).

% dont chase Determiners
%%%%%%% adjectives_phrase(_Event,_Subj,_CycL,_CycL9) --> cant_be_adjnoun,{!,fail}.

adjectives_phrase(Event,Subj,CycL,CycL9) --> dcgAnd(thePOS('Adjective'),adjective_word(Subj,CycL,CycLO1)),conjuntive_and,adjective_word(Subj,CycLO1,CycL9).
adjectives_phrase(Event,Subj,CycL,CycL9) --> adjective_word(Subj,CycL,CycL1),adjectives_phrase(Event,Subj,CycL1,CycL9).
adjectives_phrase(Event,Subj,CycL,CycL) --> [].
%% (prepCollocation Contiguous-TheWord Adjective-Gradable To-TheWord)

%%%%%%% adjective_word(Subj,CycL,_)-->cant_be_adjnoun,{!,fail},dcgWordList(_).

%adjective_word(Subj,CycL,and(CycL,equals(':POSSESSOR',PronounIsa),controls(PronounIsa,Subj))) --> dcgAnd(dcgAnd(thePOS('PossessivePronoun'),thePOS('Pronoun')),theNounIsa(Pro,PronounIsa,'Individual')).
adjective_word(Subj,CycL,and(CycL, isa(Subj,AttribProp))) --> theNounIsa(Subj,AttribProp,'ChromaticColor').
adjective_word(Subj,CycL,and(CycL, isa(Subj,AttribProp))) --> theNounIsa(Subj,AttribProp,'FirstOrderCollection').
adjective_word(Subj,CycL,and(CycL,NPTemplate)) --> theNounFrame(_,_,'Adjective',Template),{substEach(Template,[':NOUN'-Subj,':REPLACE'-Subj,':SUBJECT'-Subj],NPTemplate)}.
adjective_word(Subj,CycL,and(CycL,controls('?Speaker',Subj))) --> theWord('My-TheWord').
adjective_word(Subj,CycL,and(CycL,controls(PronounIsa,Subj))) --> dcgAnd(thePOS('PossessivePronoun'),theNounIsa(Pro,PronounIsa,'Individual')).
adjective_word(Subj,CycL,and(CycL, isa(Subj,AttribProp))) --> dcgAnd(thePOS('Possessive'),theNounIsa(Subj,AttribProp,'Collection')).
adjective_word(Subj,CycL,and(CycL, isa(Subj,AttribProp))) --> dcgAnd(thePOS('Adjective'),theNounIsa(Subj,AttribProp,'Collection')).


% =======================================================
% Qualified Noun
% =======================================================

% dont chase prepositions/dets
%%%%%%% collection_noun(Event,Subj,CycL) --> cant_be_adjnoun,{!,fail}.

%':POSSESSOR'
% the eaters of the dead
collection_noun(Event,Subj,and(HowDoes,occursBefore(PreEvent,Event))) --> dcgAnd(theNounFrame(Subj,'GenitiveFrame','AgentiveNoun',Template),theName(Subj)),
  {substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event],HowDoes),suggestVar(['GenitiveFrame'],PreEvent),
     ignore(sub_term(performedBy(PreEvent,Subj),HowDoes))}.

% the jugglers
collection_noun(Event,Subj,NPTemplate) --> theNounFrame(Subj,'RegularNounFrame','WHPronoun',Template),
    {subst(Template,':NOUN',Subj,NPTemplate)}. %numbervars(NPTemplate,_,_)

%collection_noun(Event,Subj,Event,'isa'(Subj,CycLCollection)) --> dcgAnd(collection_type(Subj,CycLCollection),scanNounUnit).

% the eaters of the dead
collection_noun(Event,Subj,CycLO) --> 
    theNounFrame(Subj,'GenitiveFrame','CountNoun',Template),
   {substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event,':POSSESSOR'-Subj2],HowDoes)},
   theWord('Of-TheWord'),
   noun_unit_int(Type,Event,Subj2,HowDoes,CycLO).

% the men
collection_noun(Event,Subj,and(isa(Subj,CycLCollection1),isa(Subj,CycLCollection2))) --> 
%      dcgSeq(dcgAnd(collection_type(Subj,CycLCollection1),theName(Subj)),collection_type(Subj,CycLCollection2)).
      dcgSeq(collection_type(Subj,CycLCollection1),collection_type(Subj,CycLCollection2)),{suggestVar([CycLCollection1,CycLCollection2],Subj)}.

collection_noun(Event,Subj,isa(Subj,CycLCollection)) --> dcgAnd(collection_type(Subj,CycLCollection),theName(Subj)),{!}.

collection_noun(Event,Subj,equals(Subj,Type)) --> theTerm(Type).

collection_type(Subj,Type) --> cant_be_adjnoun,{!,fail},dcgWordList(_).
collection_type(Subj,Type) --> theWord('The-TheWord'),{trace,!,fail}.
collection_type(Subj,Type)--> theNounIsa(Subj,Type,'StuffType').
collection_type(Subj,Type)--> theNounIsa(Subj,Type,'ClarifyingCollectionType').
collection_type(Subj,Type)--> theNounIsa(Subj,Type,'SentenceSubjectIndexical').
collection_type(Subj,'Person') --> theText([person]).
collection_type(Subj,Type)--> theNounIsa(Subj,Type,'Collection').
collection_type(Subj,'NounFn'(Word)) --> theWord(Word,'Noun'),{!}.
collection_type(Subj,'HypothesizedAdjectiveSenseFn'(Word,'Adjective-Gradable')) --> theWord(Word,'Adjective'),{!}.
collection_type(Subj,'HypothesizedWordSenseFn'(Word,POS)) --> theWord(Word,POS),{!}.
collection_type(Subj,'Thing') --> [].
                             %   Individual
%collection_type(Subj,'InstanceNamedFn'(string(Word),'WnNoun')) --> thePOS(Word,'WnNoun').
%collection_type(Subj,'InstanceNamedFn'(string(SType),'Collection')) --> [Type],{flatten([Type],SType)}.

%phraseNoun_each(Eng,CycL):-posMeans(Eng,'SimpleNoun',Form,CycL).
%phraseNoun_each(Eng,CycL):-posMeans(Eng,'MassNoun',Form,CycL).
%phraseNoun_each(Eng,CycL):-posMeans(Eng,'AgentiveNoun',Form,CycL).
%phraseNoun_each(Eng,CycL):-posMeans(Eng,'Noun',Form,CycL).
%phraseNoun_each(Eng,CycL):-posMeans(Eng,'QuantifyingIndexical',_,CycL).


deftest_p2c("outside of it",prepn_nunit(A,B,C,Y)).
deftest_p2c("inside of it",prepn_nunit(A,B,C,Y)).
deftest_p2c("next to it",prepn_nunit(A,B,C,Y)).
deftest_p2c("offspring of it",prepn_nunit(A,B,C,Y)).

prepn_nunit(Event,Subj,CycLIn,and(insideOf(Subj,Subj2),CycLOut))--> theWord('Inside-TheWord'),theWord('Of-TheWord'),{!},
   noun_unit(Type,Event,Subj2,CycLIn,CycLO).

prepn_nunit(Event,Subj,CycLIn,and(wff_todo(prepMeaning(Word,Prep,Subj,Subj2)),CycLOut))--> theWord(Word,POS),
   {holdsWithQuery(prepCollocation,Word,Pos,Prep)},
   theWord(Prep),{!},
   noun_unit(Type,Event,Subj2,CycLIn,CycLO).


%>
