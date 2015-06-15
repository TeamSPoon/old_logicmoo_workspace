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


% (asdf:operate 'asdf:load-op :asdf-binary-locations)
% (load "start.lisp")


% =======================================================
% Preposition
% =======================================================
prepositional_noun_phrase(Event,Obj,Target,NPCycL,Prep) -->[['PP'|SText]],{must(phrase(prepositional_noun_phrase(Event,Obj,Target,NPCycL,Prep),SText))}.

prepositional_noun_phrase(Event,Obj,Target,NPCycL,Prep) -->
      prepositional(Event,Obj,Target,PrepCycL,Prep),
      noun_unit(Type,Event,Target,PrepCycL,NPCycL).

%textCached([about], [frame, prepSemTrans, 'Post-NounPhraseModifyingFrame', 'Preposition', topicOfInfoTransfer(':ACTION', ':OBLIQUE-OBJECT')]).
prepositional(Event,Obj,Target,PrepCycL,Prep) --> %dcgAnd(thePOS(Prep,'Preposition'),theFrame(Pred,_,'Preposition',Template)),
    dcgAnd(thePrepWord(Prep),theFrame(Pred,O,'Preposition',Template)),{not(member(O,[denotation]))},
   {substEach(Template,[':SUBJECT'-Obj,':NOUN'-Obj,':ACTION'-Event,':OBJECT'-Target,':OBLIQUE-OBJECT'-Target],PrepCycL)}.


% =======================================================
% Particles
% =======================================================
particle_np(Event,Subj,Obj,VerbCycL,VerbObjectCycL,Prep) --> partical_expression(Prep),{!},noun_unit(Type,Event,Obj,VerbCycL,VerbObjectCycL).
particle_np(Event,Subj,Obj,VerbCycL,VerbObjectCycL,Prep) --> noun_unit(Type,Event,Obj,VerbCycL,VerbObjectCycL),partical_expression(Prep).
%e2c("she took off clothing").

partical_expression(Prep) --> [['PRT'|FORM]],{(phrase(partical_expression_sub(Prep),FORM)),!}.
partical_expression(Prep) --> partical_expression_sub(Prep).

partical_expression_sub(Prep) --> thePrepWord(Prep).



% =======================================================
% Rel Clauses 
%  see http://en.wikipedia.org/wiki/Relative_clause 
% =======================================================
whomThatWhich(_,[],_):-!,fail.
whomThatWhich(Word)-->dcgAnd(dcgOr([theWord('Who-TheWord'),
                            theWord('Whom-TheWord'),theWord('Which-TheWord'),
                            theWord('Whose-TheWord'),theWord('That-TheWord') /*,
                            theTerm('Backreference-ClassB-NLAttr'),thePOS('ExpletivePronoun') */]),
                            theWord(Word)),{!}.

% kill driving
notPresentParticiple --> dcgAnd(thePOS('Verb'),(theForm('presentParticiple'))),{!,fail}.
% drove
notPresentParticiple --> dcgAnd(thePOS('Verb'),dcgNot(theForm('presentParticiple'))).
% is driving
notPresentParticiple --> thePOS('BeAux'),presentParticiple.
% can drive
notPresentParticiple --> thePOS('Modal'),notPresentParticiple.
% the driver
notPresentParticiple --> thePOS('Determiner-Central'),thePOS('AgentiveNoun').
notPresentParticiple --> thePOS('regularAdverb'),notPresentParticiple.

% kill drove
presentParticiple --> dcgAnd(thePOS('Verb'),dcgNot(theForm('presentParticiple'))),{!,fail}.
% driving
presentParticiple --> dcgAnd(thePOS('Verb'),theForm('presentParticiple')).
% that [ drove | is driving ]
presentParticiple --> whomThatWhich(_Word),notPresentParticiple.
% quickly driving
presentParticiple --> thePOS('regularAdverb'),presentParticiple.
% from Nantucket
presentParticiple --> thePOS('Proposition').

% DCG circuit
relationl_clause_head(_,[],_):-!,fail.

% driving
% that drove
% that is driving
% that can drive
% who is driving
% whom drives
% the driver
% who is the driver
deftest_p2c("who is the driver",(rel_clause(X,Y,Z),dcgWordList(List))).
% that is from dover
% that ..
relationl_clause_head --> whomThatWhich(_Word),{!,true}.
relationl_clause_head --> presentParticiple.

% named Bilbo
%  ... todo ...
% whose daughter is ill
% than whom I am smaller

deftest_p2c("to whom I have written",(rel_clause(X,Y,Z),dcgWordList(List))).
% to whom I have never written

deftest_p2c("with hats",rel_clause(X,Y,Z)).

% =======================================================
% BEGIN RELATIVE CLAUSE
% =======================================================

% DCG circuit
rel_clause(_Event,_Subj,_SubjObjectCycL,[],_):-!,fail.

% sitting on the shelf / kissed it
rel_clause(Event,Subj,debug(rc_vp,'and'(VPCycL, timeOf(Event,Time)))) --> dcgStartsWith(theForm('presentParticiple')),
      verb_phrase(Time,Subj,_Obj,Event,VPCycL).

% named Benji
rel_clause(_Event,Subj,ProperNameCycL) --> theWord('Name-TheWord'),proper_name(Subj,ProperNameCycL).

% to whom I have written 
rel_clause(Event,Subj,and_to(CycL)) --> theWord('To-TheWord'),rel_clause(Event,Subj,CycL).

% that Verb Phrase
rel_clause(Event,Subj,CycL9) --> theTerm('Backreference-ClassB-NLAttr'),verb_phrase(_Time,Subj,_Obj,Event,CycL9).

rel_clause(Event,Subj,CycL9) --> theWord('That-TheWord'),verb_phrase(_Time,Subj,_Obj,Event,CycL9).

% of stuff
% on a shelf with a ball
rel_clause(Event,Subj,SubjObjectCycL) --> 
    theFrame(Event,'Post-NounPhraseModifyingFrame',_,Template),
      noun_unit_int(_Type,Event,Obj,HowDoes,SubjObjectCycL),
   {substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event,':OBJECT'-Obj,':OBLIQUE-OBJECT'-Obj],HowDoes)}.


% on the shelf
rel_clause(Event,Subj,'rpand'(SubjIsaCycL, preactorPrep(Event,Obj,Prep))) --> prepositional_noun_phrase(Event,Subj,Obj,SubjIsaCycL,Prep).


% whom I have written
rel_clause(Event,Subj,debug(whm,and(whom(Subj),More))) -->  dcgAnd(theWord('Whom-TheWord'),pronoun(Subj,CycL9,More)),rel_clause(Subj_I,Event,CycL9).

% I/we/they have written
rel_clause(Event,Subj,debug(rc_p_vp,More)) --> pronoun(I_Subj,CycL,More),verb_phrase(Time,I_Subj,Subj,Event,CycL).

% I have written
rel_clause(Event,Subj,debug(i_vp,CycL)) --> theWord('I-TheWord'),verb_phrase(Time,I_Subj,Subj,Event,CycL).

% who Verb Phrase
rel_clause(Event,Subj,and(More,CycL9)) --> dcgAnd(thePOS('wps'),pronoun(Subj,CycL9,More)),verb_phrase(_Time,Subj,_Obj,Event,CycL9).

% needs p2c("kissed it",(verb_phrase(X,Y,Z,A,B),dcgWordList(List))).
% needs p2c("driving southward",(verb_phrase(X,Y,Z,A,B),dcgWordList(List))).

% of Dover
%rel_clause(Event,Subj,CycL9)-->theWord('Of-TheWord'),noun_unit_int(Type,Event2,Subj,thereExists(Event2,and(isa(Event2,'Situation'),actorIn(Event2,Subj))),CycL9),{!}.
rel_clause(Event,Subj,'QueryFn'(That,CycL2))-->thePOS(That,'WHAdverb'),{!},utterance(Event,Subj,CycL2),{!}.
%rel_clause(Event,Subj,andDet(whp(DET),Type,V)) -->thePOS(DET,'WHPronoun'),{!},verb_unit(Type,V).
%rel_clause(Event,Subj,'WhDeterminerFn'(Subj,Obj,CycL9))-->thePOS(That,'Determiner'),vp1(Event,Subj,Obj,CycL9),{!}.
%rel_clause(Event,Subj,and(isa(Event2,That),'ThatFn'(That,Subj,Obj,CycL9)))-->thePrep(Event2,That),vp1(Event2,Subj,Obj,CycL9),{!}.


%verb_intransitive(Time,Subj,Obj,Event,'and'('bodilyDoer'(Subj,Event),'isa'(Event,actOf(paint)))) --> [paints].

%rel_clause(Event,Subj,CycL0,'and'(CycL0, HowDoes)) --> theWord('That-TheWord'),verb_phrase_premods(Time,Subj,Obj,Event,Does,HowDoes),verb_phrase(Time,Subj,Obj,Event,Does).



% =======================================================
% Quantities (DET Phrases)     
% =======================================================
decimalNumber(Num) --> wholeNumber(Subj,Num1),dotSign,wholeNumber(Subj,Num2),{concat_atom([Num1,'.',Num2],Atom),atom_number(Atom,Num)}.
decimalNumber(Num) --> wholeNumber(Subj,Num).
wholeNumber(Subj,Num) --> theText([Num]),{number(Num),!}.
wholeNumber(Subj,2) --> theText([two]).
wholeNumber(Subj,2) --> theText([two]).
wholeNumber(Subj,1) --> theText([one]).
wholeNumber(Subj,1) --> theText([a]).
wholeNumber(Subj,Num) --> dcgOr(theIsa(Subj,Num,'Numeral'),theIsa(Subj,Num,'Number-General')),{must(number(Num))}.
dollarSign --> thePOS('$').
dotSign --> thePOS('.').

% =======================================================
% Quantification (DET Phrases)     
%
deftest_p2c("all cars",noun_unit(X,Y,Z,A,B)).
deftest_p2c("many cars",noun_unit(X,Y,Z,A,B)).
deftest_p2c("large numbers of cars",noun_unit(X,Y,Z,A,B)).
deftest_p2c("a large number of cars",noun_unit(X,Y,Z,A,B)).
deftest_p2c("two cars",noun_unit(X,Y,Z,A,B)).
deftest_p2c("at least two cars",noun_unit(X,Y,Z,A,B)).
deftest_p2c("all",quant_phrase(Subj,Prop1,CycL1,CycL)).
%
% TODO Negations (no fewer than)
% =======================================================
%quant_phrase(Subj,Pre,Post,CycL) --> quant_phrase1(Subj,Pre,Mid,CycL),quant_phrase(Subj,Mid,Post,CycL).


quant_phrase(Subj,Restr,Scope,CycL9)--> dcgOr(dcgOr(existential_words,universal_words(_)),dcgNone), quant_phraseN(Subj,Restr,Scope,CycL9).



quant_phrase(Subj,Restr,Scope,'thereExists'(Subj,'and'(Restr , Scope))) --> existential_words.

quant_phrase(Subj,Restr,Scope,'forAll'(Subj,'implies'(Restr , Scope))) --> universal_words(_).


quant_phrase(Subj,Restr,Scope,CycL9) --> theFrame(_,'QuantifierFrame',_,Template),
      {substEach(Template,[':SCOPE'-Scope,':NOUN'-Subj,':RESTR'-Restr],CycL9)}.

quant_phrase(Subj,Restr,Scope,and(Restr,Scope)) --> [].

quant_phraseN(Subj,Restr,Scope,'thereExistExactly'(Num,Subj,'and'(Restr , Scope))) -->  wholeNumber(Subj,Num).


quant_phraseN(Subj,Restr,Scope,'thereExistAtLeast'(Num,Subj,'and'(Restr , Scope))) --> 
         dcgSeq(at_least_words(N),wholeNumber(Subj,Num1)),{Num is N+Num1}.

%p2c("large number of",quant_phrase(Subj,Prop1,CycL1,CycL)).
quant_phraseN(Subj,Restr,Scope,and(largeNumber(Restr),Scope)) --> theText('large'),theWord('Number-TheWord'),theText('of').

% at least
at_least_words(0) --> theWord('At-TheWord'),theWord('Little-TheWord').
at_least_words(1) --> theWord('More-Than-MWW').
at_least_words(1) --> theWord('Greater-Than-MWW').

quant_phraseN(Subj,Restr,CycL1,'thereExistAtMost'(Num,Subj,'and'(Restr , CycL1))) --> 
         dcgSeq(at_most_words(N),wholeNumber(Subj,Num1)),{Num is N+Num1}.

at_most_words(0) --> dcgSeq(theWord('At-TheWord'),dcgOr(theWord('Most-TheWord'),theTerm('Most-NLAttr'))).
at_most_words(-1) --> theWord('Less-Than-MWW').
at_most_words(-1) --> theWord('Few-TheWord'),theWord('Than-TheWord').


existential_words --> existential_word,existential_word.
existential_words --> existential_word.
existential_word --> theWord(W), {member(W,['A-TheWord','An-TheWord','The-TheWord','Some-TheWord']),!}.
% there is,there are
existential_word --> theWord('There-TheWord'),dcgOr(theWord('Be-TheWord'),theWord('Exist-TheWord')). 

universal_words(S) --> universal_word(_),universal_word(S).
universal_words(S) --> universal_word(S).

universal_word(plural) --> theWord('All-TheWord'),dcgOptional(existential_word).
% every
universal_word(singular) --> theTerm('Every-NLAttr').
universal_word(singular) --> theWord('Every-TheWord');theWord('Each-TheWord').
universal_word(plural) --> theWord('For-TheWord'),theWord('All-TheWord').
universal_word(plural) --> theText([forAll]).
universal_word(plural) --> theText([forall]).

np_start --> dcgStartsWith(thePOS('PossessivePronoun-Pre')),{!}.
np_start --> dcgStartsWith(thePOS('Pronoun')),{!}.
np_start --> dcgStartsWith(dcgOr(existential_word,universal_word(S))),{!}.

%>
