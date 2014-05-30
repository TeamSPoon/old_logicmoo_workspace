% ===================================================================
% File 'parser_e2c.pl'
% Purpose: Attempto Controlled English to CycL conversions from SWI-Prolog  
% This implementation is an incomplete proxy for CycNL and likely will not work as well
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'parser_e2c.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================
:-module(parser_e2c,[
         e2c/1,
         e2c/2,
         getVarAtom/2, 
         idGen/1, 
	testE2C/0]).


% :- ensure_loaded(logicmoo('logicmoo_util/logicmoo_util_all.pl')).

:- meta_predicate do_dcg(?,?,?,?,?).
:- meta_predicate isPOS(?,?,?,?,?).

:- moodb:register_module_type(utility).

% posm_cached(CycL, Phrase,POS,Form,CycL)
:-dynamic lex/3,  lexMap/3.

idGen(X):-flag(idGen,X,X+1).

%:- ensure_loaded(logicmoo('vworld/dbase.pl')).
:- moodb:begin_transform_moo_preds.

:- retractall(moodb:prevent_transform_moo_preds).

% Semantic Interpretation
/* from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980 */

/* 
   when using sentence we need to pass 3 arguments, 
   the first will match CycL in the head of the DGC clause
   the second is the list containing the words in the sentence
   the third is the empty list.
*/

:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
% :- style_check(-string).

e2c(English):-
      e2c(English,CycLOut),
      'fmt'('~w~n',[CycLOut]).

e2c(English,CycLOut):-
   to_word_list(English,[Eng|Lish]),!,
   string_list_to_atom_list([Eng|Lish],Atoms),!,
   e2c_list(Atoms,CycLOut).

string_list_to_atom_list([],[]):-!.
string_list_to_atom_list([Eng|Lish],[E|Atoms]):-
 to_aword(Eng,E),!,
 string_list_to_atom_list(Lish,Atoms).

to_aword(Eng,Eng):-var(Eng),!.
to_aword(Eng,E):-string(Eng),!,atom_string(E0,Eng),to_aword(E0,E).
to_aword(Eng,N):-atom(Eng),atom_number(Eng,N),!.
to_aword(Eng,Eng):-atom(Eng),!.
to_aword(Eng,Eng).


e2c_list([Eng|Lish],CycLOut):- 
   length(Lish,M1),!,
   once(((
   between(0,M1,Slack),
   length(Rest,Slack),
   try_e2c(_CycL,[Eng|Lish],Rest)
   ))),
   % now may iterate out with rest pinned down
   try_e2c(CycLMid,[Eng|Lish],Rest),
   toCycApiExpression(CycLMid,CycLOut).

   
try_e2c(CycL,English,Rest):-sentence(CycL,English,Rest),!.
try_e2c(CycL,English,Rest):-noun_phrase('?Var','?SomeRelation',CycL,English,Rest),!.


% =======================================================
% sentence(CycL, [every,man,that,paints,likes,monet],[]) 
% =======================================================

sentence(CycL) --> declaritive_sentence(CycL).
sentence(CycL) --> imparitive_sentence(CycL).
sentence(CycL) --> inquiry(CycL).
	
imparitive_sentence(CycL) --> verb_phrase('?TargetAgent','?ImparitiveEvent',CycL).
declaritive_sentence(CycL) --> noun_phrase(Subj,CycL1,CycL),verb_phrase(Subj,Event,CycL1).
declaritive_sentence(CycL) --> 
      [the],trans2_verb(Subj,Event,Obj,VProp),possible_prep,
      noun_phrase(Obj,VProp,CycL1),[is],noun_phrase(Subj,CycL1,CycL).

declaritive_sentence(CycL) --> 
      noun_phrase(Obj,VProp,CycL1),
      [the],trans2_verb(Subj,Event,Obj,VProp),possible_prep,
      noun_phrase(Obj,VProp,CycL1),[is],noun_phrase(Subj,CycL1,CycL).

possible_prep-->[of];[].

inquiry('thereExists'(Actor,CycL)) --> wh_pronoun(Actor), verb_phrase(Actor,'?QuestionEvent',CycL),[?].
inquiry(CycL) --> inv_sentence(CycL),[(?)].

inv_sentence(CycL) --> aux, declaritive_sentence(CycL).

wh_pronoun('?Who') --> [who].
wh_pronoun('?What') --> [what].

aux --> [does].

% =======================================================
% Quantification (DET Phrases)
% =======================================================

quant_phrase(Subj,Prop,CycL1,'thereExists'(Subj,'and'(Prop , CycL1))) --> existential_words.
quant_phrase(Subj,Prop,CycL1,'forAll'(Subj,'implies'(Prop , CycL1))) --> universal_words.

existential_words --> existential_word,existential_word.
existential_words --> existential_word.
existential_word --> [a];[an];[the];[some];[there,is];[there,are];[there,exists].

universal_words --> universal_word,universal_word.
universal_words --> universal_word.
universal_word --> [every];[all];[forall];[each];[for,all].

% =======================================================
% Adjective Phrases
% =======================================================

adjectives_phrase(Subj,IsaDoes,'and'(IsaDoes, AttribProp)) --> adj_phrase(Subj,AttribProp).
adjectives_phrase(_,IsaDoes,IsaDoes) --> [].

adj_phrase(Subj,Formula) -->  [A,B,C],{phrase_meaning_adj([A,B,C],Subj,Formula)}.
adj_phrase(Subj,Formula) -->  [A,B],{phrase_meaning_adj([A,B],Subj,Formula)}.
adj_phrase(Subj,Formula) -->  [A],{phrase_meaning_adj([A],Subj,Formula)}.


%'adjSemTrans'('Cloud-TheWord', 0, 'RegularAdjFrame', ['weather', ':NOUN', 'Cloudy']).
phrase_meaning_adj(Phrase,Subj,CycL):-
	 pos(CycWord,Phrase,'Adjective',Form),      
	 'adjSemTrans'(CycWord, _, _, Formula),
	 Repl='CollectionOfFn'(Subj),
	 %posMeans(Phrase,'Verb',POS,WordMeaning),
	 subst(Formula,':SUBJECT',Subj,Formula21),
	 subst(Formula21,':REPLACE',Repl,Formula2),
	 subst(Formula2,':NOUN',Subj,Formula3),
	 subst(Formula3,':ACTION','?ACTION',Formula4),
	 subst(Formula4,':OBJECT','?OBJECT',Formula6),
	 list_to_term(Formula6,CycL).

phrase_meaning_adj(Phrase,Subj,'hasAttributeOrCollection'(Subj,CycL)):-
      posMeans(Phrase,'Adjective',Form,CycL),not(lowerCasePred(CycL)).

% =======================================================
% Nouns Phrases
% =======================================================

noun_phrase(Subj,CycLIn,CycLOut) --> noun_expression(Subj,Isa,CycLOut),rel_clause(Subj,CycLIn,Isa).
%noun_phrase(Subj,CycL1,CycL) --> noun_expression(Subj,CycL1,CycL),[and],noun_phrase(Subj,CycL1,CycL)
   
noun_expression(PN,CycL,CycL) --> pronoun(PN).
noun_expression(PN,CycL,CycL) --> proper_noun_phrase(PN).
noun_expression(Subj,CycLVerb,CycLOut) -->  
   quant_phrase(Subj,AttribIsa,CycLVerb,CycLOut),
   adjectives_phrase(Subj,Isa,AttribIsa),
   collection_noun_isa(Subj,Isa).
noun_expression(Subj,CycLVerb,('thereExists'(Subj,'and'(AttribIsa,CycLVerb)))) -->  
   adjectives_phrase(Subj,AttribIsa1,AttribIsa),
   collection_noun_isa(Subj,Isa),
   adjectives_phrase(Subj,Isa,AttribIsa1).

%noun_expression(Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop12,CycL1,CycL),collection_noun_isa(Subj,Prop1),rel_clause(Subj,Prop1,Prop12).
%noun_expression(Subj,CycL1,CycL) -->  quant_phrase(Subj,Prop1,CycL1,CycL),collection_noun_isa(Subj,Prop1).

% =======================================================
% Conjunctions
% =======================================================

conj_word --> [X],{conj_word(X)}.
conj_word --> [X],{connective_word(X)}.
disj_word --> [X],{disj_word(X)}.

conj_word(and). conj_word(also). disj_word(or).  
% and	but	or	yet	for	nor	so


common_Subordinating_Conjunctions([
after,although,as,[as,if],[as,long,as],[as,though],because,before,[even,if],[even,though],if,
[if,only],[in,order,that],[now,that],once,[rather,than],since,[so,that],than,that,though,till,unless,until,when,whenever,where,whereas,wherever,while]).

% =======================================================
% Rel Clauses
% =======================================================

connective_word(that). connective_word(who). connective_word(which). 

rel_clause(Subj,Isa,'and'(Isa, HowDoes)) --> [X],{connective_word(X)},adverbs_phrase(Event,Does,HowDoes),verb_phrase(Subj,Event,Does).
rel_clause(_,Isa,Isa) --> [].


% =======================================================
% Qualified Noun
% =======================================================
collection_noun_isa(Subj,'isa'(Subj,CycLCollection)) --> collection_noun(Subj,CycLCollection).

collection_noun(Subj,CycLCollection) --> [A,B,C,D],{phraseNoun([A,B,C,D],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A,B,C],{phraseNoun([A,B,C],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A,B],{phraseNoun([A,B],Form,Subj,CycLCollection)}.
collection_noun(Subj,CycLCollection) --> [A],{phraseNoun([A],Form,Subj,CycLCollection)}.
collection_noun(Subj,'AdultMalePerson') --> [man].

collection(M)--> collection_noun('?Subj',CycLCollection).

phraseNoun(Eng,Form,Subj,CycLCollection):-
      phraseNoun_each(Eng,Form,CycLCollction),
      eng_subj(Eng,Subj).

eng_subj(Eng,Subj):-var(Subj),getVarAtom(Subj,Atom),concat_atom([?|Eng],'',T),atom_concat(T,Atom,Subj).
eng_subj(Eng,Subj):-!.

getVarAtom(Value,Name):-var(Value),!,term_to_atom(Value,Vname),atom_codes(Vname,[95,_|CODES]),atom_codes(Name,CODES),!.
getVarAtom('$VAR'(VNUM),Name):-concat_atom([VNUM],Name),!.


phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'SimpleNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'MassNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'AgentiveNoun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'Noun',Form,CycL).
phraseNoun_each(Eng,Form,CycL):-posMeans(Eng,'QuantifyingIndexical',_,CycL).
							 

% =======================================================
% Pronoun Phrases
% =======================================================
%'nounPrep'('Address-TheWord', 'Of-TheWord', ['pointOfContactInfo', ':OBLIQUE-OBJECT', 'ContactLocation', 'addressText', ':NOUN']).


pronoun('?Speaker') --> ['I'];[i];[me].
pronoun('?TargetAgent') --> ['you'];['You'].
pronoun(CycLTerm) --> [A,B,C],{lex_pronoun([A,B,C],CycLTerm)}.
pronoun(CycLTerm) --> [A,B],{lex_pronoun([A,B],CycLTerm)}.
pronoun(CycLTerm) --> [A],{lex_pronoun([A],CycLTerm)}.

lex_pronoun(W,Fixed):-
      lex_pronoun2(W,T),
      fix_pronoun(W,T,Fixed).

fix_pronoun(W,[null],Fixed):-!,concat_atom(['?'|W],Fixed).
fix_pronoun(W,Fixed,Fixed).

lex_pronoun2(Words,CycLTerm):-posMeans(Words,'WHPronoun-Subj',_,CycLTerm).
lex_pronoun2(Words,CycLTerm):-posMeans(Words,'Pronoun',_,CycLTerm).
lex_pronoun2(Words,CycLTerm):-posMeans(Words,'ObjectPronoun',_,CycLTerm).

% =======================================================
% Proper Noun Phrases
% =======================================================

proper_noun_phrase(CycLTerm) --> [the],proper_noun(CycLTerm).
proper_noun_phrase(CycLTerm) --> proper_noun(CycLTerm).

proper_noun(CycLTerm) --> [A,B,C],{lex_proper_noun_cached( [A,B,C],CycLTerm)}.
proper_noun(CycLTerm) --> [A,B],{lex_proper_noun_cached( [A,B],CycLTerm)}.
proper_noun(CycLTerm) --> [A],{lex_proper_noun_cached( [A],CycLTerm)}.

lex_proper_noun_cached(Words,CycLTerm):-posMeans(Words,'ProperNoun',_,CycLTerm).

% =======================================================
% Verbs/Verb Phrases
% =======================================================


verb_phrase(Subj,Event,CycL) --> 
	adverbs_phrase(Event,VProp,CycL), 
	intrans_verb(Subj,Event,VProp).

verb_phrase(Subj,Event,CycL) -->
      adverbs_phrase(Event,VProp,EventProp),
      trans2_verb(Subj,Event,Obj,VProp),
      noun_phrase(Obj,EventProp,CycL).

verb_phrase(Subj,Event,CycL) -->
      adverbs_phrase(Event,VProp,EventProp),
      trans3_verb(Subj,Event,Obj,Prep,Target,VProp),
      noun_phrase(Obj,EventProp,ObjPropEvent),
      prepositional_noun_phrase(Target,ObjPropEvent,Prep,CycL).


trans3_verb(Subj,Event,Obj,Prep,Target,VProp) --> [A,B,C], {lex_trans3_verb([A,B,C],Subj,Event,Obj,Prep,Target,VProp)}.
trans3_verb(Subj,Event,Obj,Prep,Target,VProp) --> [A,B], {lex_trans3_verb([A,B],Subj,Event,Obj,Prep,Target,VProp)}.
trans3_verb(Subj,Event,Obj,Prep,Target,VProp) --> [A], {lex_trans3_verb([A],Subj,Event,Obj,Prep,Target,VProp)}.

lex_trans3_verb(VerbPhrase,Subj,Event,Obj,Prep,Target,CycL):-
      verb_frame(VerbPhrase,CycWord,3,CycPred,Formula),
      apply_frame(Formula,Subj,Event,Obj,Target,CycL).

lex_trans3_verb(VerbPhrase,Subj,Event,Obj,Prep,Target,VProp):-
      posMeans(VerbPhrase,'Verb',Form,CycLPred),
     nonvar(CycLPred),
      ignore(Event='?ACTION'),
      lex_trans3_verb2(VerbPhrase,CycLPred,Subj,Event,Obj,Prep,Target,VProp).
      
lex_trans3_verb2(VerbPhrase,CycLPred,Subj,Event,Obj,Prep,Target,VProp):-
      lowerCasePred(CycLPred) -> 
      VProp=..[CycLPred,Subj,Obj,Target] ; 
      VProp = 'and'('isa'(Event,CycLPred),'doneBy'(Event,Subj),'eventOccursAt'(Event,Obj),'constituentInSituation'(Event,Target)).

   
      

      
% =======================================================
% Proposition
% =======================================================

prepositional_noun_phrase(Target,ObjPropEvent,Prep,CycL) -->
      proposition(Prep),noun_phrase(Target,ObjPropEvent,CycL).
prepositional_noun_phrase(Target,ObjPropEvent,'NIL',CycL) -->
      noun_phrase(Target,ObjPropEvent,CycL).


proposition(Prep) --> [PrepWord],{proposition_lex(PrepWord,Prep)}.

proposition_lex(X,X):-proposition_lex(X).
proposition_lex(to). proposition_lex(from). proposition_lex(of).


% =======================================================
% Adverbs
% =======================================================

adverbs_phrase(Event,IsaDoes,'and'(IsaDoes, AttribProp)) --> adv_phrase(Event,AttribProp).
adverbs_phrase(_,IsaDoes,IsaDoes) --> [].

adv_phrase(Event,Formula) -->  [A,B,C],{lex_adverb([A,B,C],Event,Formula)}.
adv_phrase(Event,Formula) -->  [A,B],{lex_adverb([A,B],Event,Formula)}.
adv_phrase(Event,Formula) -->  [A],{lex_adverb([A],Event,Formula)}.

lex_adverb(Phrase,Event,'hasAttributeOrCollection'(Event,Trait)):-
      posMeans(Phrase,'Adverb',Form,Trait).

% =======================================================
% Transitive 2 Verbs
% =======================================================

%trans2_verb(Subj,Y,like(Subj,Y)) --> [likes].
trans2_verb(Subj,Event,Obj,CycL) --> [A,B,C,D,E],{lex_verb_meaning([A,B,C,D,E],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A,B,C,D],{lex_verb_meaning([A,B,C,D],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A,B,C],{lex_verb_meaning([A,B,C],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A,B],{lex_verb_meaning([A,B],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,Event,Obj,CycL) --> [A],{lex_verb_meaning([A],CycL,Subj,Event,Obj)}.
trans2_verb(Subj,admire(Subj,Obj),Obj,admire(Subj,Obj)) --> [admires].

% =======================================================
% Intransitive Verbs
% =======================================================

intrans_verb(Subj,Event,'and'('bodilyDoer'(Subj,Event),'isa'(Event,actOf(paint)))) --> [paints].

% ============================================================================
% Verb CycL Tense
% ============================================================================

%   'verbSemTrans'('Fancy-TheWord', 0, 'TransitiveNPCompFrame', ['likesObject', ':SUBJECT', ':OBJECT']).
lex_verb_meaning(Phrase,MeaningTerm,Subj,Event,Obj):-
   tensed_lex_verb_meaning(Phrase,MeaningTerm,Subj,Event,Obj,Tense).

lex_verb_meaning(Phrase,MeaningTerm,Subj,Event,Obj):-
   lex_trans2_verb2(Phrase,MeaningTerm,Subj,Event,Obj).

% rewrites
tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,now):-
   atom(Words),atom_concat(Phrase,'s',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,past):-
   atom(Words),atom_concat(Phrase,'d',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,past):-
   atom(Words),atom_concat(Phrase,'ed',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).

tensed_lex_verb_meaning([Words],MeaningTerm,Subj,Event,Obj,nowing):-
   atom(Words),atom_concat(Phrase,'ing',Words),
   lex_trans2_verb2([Phrase],MeaningTerm,Subj,Event,Obj).


% ============================================================================
% lex_trans2_verb2 Templates
% ============================================================================

lex_trans2_verb2(VerbPhrase,CycL,Subj,Event,Obj):-
      verb_frame(VerbPhrase,CycWord,2,CycPred,Formula),
      apply_frame(Formula,Subj,Event,Obj,'?OBLIQUE-OBJECT',CycL).

verb_frame([is,the,subclass,of],CycWord,Arity,CycPred,['genls',':SUBJECT',':OBJECT']).
verb_frame([is,a,subclass,of],CycWord,Arity,CycPred,['genls',':SUBJECT',':OBJECT']).
verb_frame([is,a],CycWord,Arity,CycPred,['isa',':SUBJECT',':OBJECT']).

verb_frame(VerbPhrase,CycWord,Arity,CycPred,Formula):-
      pos(CycWord,VerbPhrase,'Verb',Form),      
      'verbSemTrans'(CycWord, _, 'TransitiveNPCompFrame', Formula),
      (contains_obliqe(Formula) -> Arity=3;Arity=2).

verb_frame([is,the,Verb,Phrase],CycWord1,2,CycPred,Formula2):-!,
      pos(CycWord1,[Verb],_,_),      
      pos(CycWord2,[Phrase],_,_),
      'nounPrep'(CycWord1,CycWord2, Formula),
      subst(Formula,':NOUN',':SUBJECT',Formula1),
      subst(Formula1,':OBLIQUE-OBJECT',':OBJECT',Formula2).

verb_frame([is,Verb,Phrase],CycWord1,2,CycPred,Formula2):-!,
      pos(CycWord1,[Verb],_,_),      
      pos(CycWord2,[Phrase],_,_),
      'nounPrep'(CycWord1,CycWord2, Formula),
      subst(Formula,':NOUN',':SUBJECT',Formula1),
      subst(Formula1,':OBLIQUE-OBJECT',':OBJECT',Formula2).


/*
the start of Obleec is Noun

Obleec is start of noun

'nounPrep'('Address-TheWord', 'Of-TheWord', ['pointOfContactInfo', ':OBLIQUE-OBJECT', 'ContactLocation', 'addressText', ':NOUN']).
'nounPrep'('Retail-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
'nounPrep'('Market-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
'nounPrep'('Start-TheWord', 'Of-TheWord', ['startingPoint', ':OBLIQUE-OBJECT', ':NOUN']).
*/

apply_frame(Formula,Subj,Event,Obj,Target,CycL):-
      ignore(Event='?ACTION'),
      subst(Formula,':SUBJECT',Subj,Formula2),
      subst(Formula2,':ACTION',Event,Formula3),
      subst(Formula3,':OBJECT',Obj,Formula4),
      subst(Formula4,':EVENT',Event,Formula5),
      subst(Formula5,':OBLIQUE-OBJECT',Target,Formula6),
      subst(Formula6,':ARG1',Subj,Formula7),
      subst(Formula7,':ACTION',Event,Formula8),
      subst(Formula8,':ARG2',Obj,Formula9),
      subst(Formula9,':EVENT',Event,Formula10),
      subst(Formula10,':ARG3',Target,Formula11),
      list_to_term(Formula11,CycL).

contains_obliqe(Formula):-flatten(Formula,Flat),member(':OBLIQUE-OBJECT',Flat).


lex_trans2_verb2(VerbPhrase,CycL,Subj,Event,Obj):-
      posMeans(VerbPhrase,'Verb',Form,CycLPred),
      ignore(Event='?ACTION'),
      atom(CycLPred),
      (lowerCasePred(CycLPred) -> 
	 CycL =..[CycLPred,Subj,Obj] ;
	 CycL = 'and'('isa'(Event,CycLPred),'doneBy'(Event,Subj),'eventOccursAt'(Event,Obj))).


% uses genFormat
lex_trans2_verb2(Phrase,MeaningTerm,Subj,Event,Obj):-
   nonvar(Phrase),
   append(Phrase,['~a'],Rest),!,
   not(memberchk('~a',Rest)),
   'genFormat'(Pred,['~a'|Rest],Format),
   do_genformat(Format,Pred,Subj,Obj,MeaningTerm).

do_genformat(['NIL'],Pred,Subj,Obj,MeaningTerm):-MeaningTerm =..[Pred,Subj,Obj].
do_genformat([P1,P2],Pred,Subj,Obj,MeaningTerm):-fp(P1,1),fp(P2,2),!, MeaningTerm =..[Pred,Subj,Obj].
do_genformat([P2,P1],Pred,Subj,Obj,MeaningTerm):-fp(P1,1),fp(P2,2),!, MeaningTerm =..[Pred,Obj,Subj].

fp(N,N).
fp([N|_],N).


/*
'nonCompositionalVerbSemTrans'('End-TheWord', 'Agreement', ['and', ['isa', ':ACTION', 'EndingAnAgreement'], ['performedBy', ':ACTION', ':SUBJECT'], ['objectActedOn', ':ACTION', ':OBJECT']]).

'lightVerb-TransitiveSemTrans'('Do-TheWord', 'CommercialActivity', ['and', ['isa', ':ACTION', 'CommercialActivity'], ['performedBy', ':ACTION', ':SUBJECT']]).
'multiWordStringDenotesArgInReln'([service], 'Provide-TheWord', 'AgentiveNoun', 'providerOfService', 2).
'nounSemTrans'('Hire-TheWord', 0, 'RegularNounFrame', ['and', ['isa', '?HIRE', 'EmployeeHiring'], ['objectActedOn', '?HIRE', ':NOUN']]).
'multiWordStringDenotesArgInReln'([service], 'Provide-TheWord', 'AgentiveNoun', 'providerOfService', 2).
'headMedialString'([intended], 'Recipient-TheWord', [of, communication], 'SimpleNoun', 'communicationTarget').
'agentiveNounSemTrans'('Emit-TheWord', 0, 'RegularNounFrame', ['emitter', '?X', ':NOUN']).
'adjSemTrans'('Cloud-TheWord', 0, 'RegularAdjFrame', ['weather', ':NOUN', 'Cloudy']).
'relationIndicators'('abbreviationForMultiWordString', 'Form-TheWord', 'Verb').
%'genNatTerm-compoundString'('TransportViaFn', 'Transport-TheWord', [via], 'MassNoun', 'singular').
%'genTemplate'('transferredThing', ['ConcatenatePhrasesFn', ['TermParaphraseFn-NP', ':ARG2'], ['BestHeadVerbForInitialSubjectFn', 'Be-TheWord'], ['BestNLPhraseOfStringFn', 'transferred in'], ['TermParaphraseFn-NP', ':ARG1']]).
%'lightVerb-TransitiveSemTrans'('Do-TheWord', 'CommercialActivity', ['and', ['isa', ':ACTION', 'CommercialActivity'], ['performedBy', ':ACTION', ':SUBJECT']]).
%'genTemplate-Constrained'('isa', ['quotedCollection', ':ARG2'], ['NPIsNP-NLSentenceFn', ['BestCycLPhraseFn', ':ARG1'], ['BestDetNbarFn-Indefinite', ['TermParaphraseFn', ':ARG2']]]).

*/

dm1:-e2c("I see two books sitting on a shelf").
dm2:-e2c("AnyTemplate1 affects the NPTemplate2").
dm3:-e2c("AnyTemplate1 at AnyTemplate2").

mostSpec(TTT,'NLWordForm',TTT).
mostSpec(TTT,TT,TT).

nodeTrans(v,'Verb').
nodeTrans(a,'Adjective').
nodeTrans(n,'Noun').
nodeTrans(p,'Pronoun').
nodeTrans(pn,'ProperNoun').
nodeTrans('-','NLWordForm').
nodeTrans(np,'NounPhrase').
nodeTrans(v,'Verb').
nodeTrans(vp,'VerbPhrase').
nodeTrans(state,'TemporallyExistingThing').
nodeTrans(property,'Role').
nodeTrans(event,'Situation').
nodeTrans(action,'Action').
nodeTrans(thing,'SpatialThing').
nodeTrans(person,'Person').
nodeTrans(location,'SubcollectionOfWithRelationFromTypeFn'('EnduringThing-Localized',toLocation,'Translocation')).
nodeTrans(destination,'Location-Underspecified').


nodeTrans(ss,'TheSentenceSubject').
nodeTrans(pp,'PrepositionalPhrase').

nodeTrans(s,'NLSentence').
nodeTrans(whnp,'WHPronoun').

nodeTrans(P,string([P])).


% ============================================================================
% posMeans
% ============================================================================
posMeans(Phrase,POS,Form,CycL):- !, posm_c( Phrase,POS,Form,CycL).

posMeans(Phrase,POS,Form,CycL):-
      posm_cached,!,
      posm_cached(_CycWord,Phrase,POS,Form,CycL).


posMeans(Phrase,POS,Form,CycL):-
      cache_the_posm,
      asserta(posm_cached),
      posMeans(Phrase,POS,Form,CycL).



cache_the_posm:-
      posm_c( Phrase,POS,Form,CycL),
      CycWord = _,
      assert_if_new(posm_cached(CycWord,Phrase,POS,Form,CycL)),%write(.),flush,
      %fmt('~q~n',[posm_cached(Phrase,POS,Form,CycL)]),
      fail.
   
cache_the_posm.



% ============================================================================
% General Parts Of Speech and Meanings
% ============================================================================

%'multiWordString'([health, care], 'Organize-TheWord', 'SimpleNoun', 'MedicalCareOrganization').
posm_c( Phrase,POS,Form,CycL):-'multiWordString'(Words, CycWord, POS, CycL),
	 pos(CycWord,Eng,_POS,Form),append(Words,Eng,Phrase).

%'genPhrase'('MedicalCareProvider', 'AgentiveNoun', 'agentive-Sg', [health, care, provider]).
posm_c( Phrase,POS,Form,CycL):-'genPhrase'(CycL, POS,Form, Phrase).

%'headMedialString'([dimensionless], 'Unit-TheWord', [of, measure], 'SimpleNoun', 'DimensionlessUnitOfMeasure').
posm_c( Phrase,POS,Form,CycL):-'headMedialString'(WordsBef,CycWord,WordsAft,POS, CycL),
	 pos(CycWord,Eng,_POS,Form),append(WordsBef,Eng,PhrasingLeft),append(PhrasingLeft,WordsAft,Phrase).

%'compoundString'('Movement-TheWord', [of, fluid], 'MassNoun', 'FluidFlowEvent').
posm_c( Phrase,POS,Form,CycL):-'compoundString'(CycWord,Words,POS, CycL),
	 pos(CycWord,Eng,_POS,Form),append(Eng,Words,Phrase).

%'prepCollocation'('Beset-TheWord', 'Adjective', 'By-TheWord').      
posm_c( Phrase,POS,Form2,'PrepCollocationFn'(CycWord1,POS,CycWord2)):-'prepCollocation'(CycWord1,POS, CycWord2),
	 pos(CycWord1,Eng1,_POS,Form1),pos(CycWord2,Eng2,_POS,Form2),append(Eng1,Eng2,Phrase).


%TODO 'abbreviationForString'([scatology], [scat]).  'abbreviationForMultiWordString'([political], 'Science-TheWord', 'massNumber', [poli, sci]).
%'abbreviationForLexicalWord'('Kilogram-TheWord', 'singular', [kg]).


%'initialismString'('CodeOfConduct', [coc]).
posm_c( Term,'SimpleNoun',normal,Proper) :- 
      'initialismString'(Proper,Term);
      'formerName'(Proper, Term);
      'scientificName'(Proper, Term);
      'termStrings-GuessedFromName'(Proper, Term);
      'nameString'(Proper, Term).

%'abbreviationString-PN'('India', ['IND']).
posm_c( Term,'ProperNoun',normal,Proper) :- 
      'initialismString'('CodeOfConduct',Term);
      'abbreviationString-PN'(Proper, Term);
      'preferredNameString'(Proper, Term);
      'countryName-LongForm'(Proper, Term);
      'countryName-ShortForm'(Proper, Term).

posm_c( Eng,POS,Form,CycL):-
	 pos(CycWord,Eng,POS,Form),
	 posm_build(CycWord,Eng,POS,Form,CycL).

% posm_c( Eng,POS,Form,CycL):-posTT(CycWord,Eng,POS,Form),'$$TTPred-denotation'(CycWord, POS, _, CycL).


%'denotation'('Capacity-TheWord', 'SimpleNoun', 0, 'Volume').
posm_build(CycWord,Eng,POS,Form,CycL):-'denotation'(CycWord, POS, _, CycL).

%'preferredGenUnit'('on-Physical', 'Preposition-Directional-Telic', 'On-TheWord').
posm_build(CycWord,Eng,POS,Form,CycL):-'preferredGenUnit'(CycL, POS, CycWord).

%'denotationRelatedTo'('Can-TheWord', 'Verb', 0, 'PreservingFood').
%posm_build(CycWord,Eng,POS,Form,'DenotationRelatedToFn'(CycL)):-'denotationRelatedTo'(CycWord, POS, _, CycL).
posm_build(CycWord,Eng,POS,Form,(CycL)):-'denotationRelatedTo'(CycWord, POS, _, CycL).

posm_build(CycWord,Eng,POS,Form,meaningOfWord(CycWord)):-
   not('denotation'(CycWord, _, _, CycL)),
   not('denotationRelatedTo'(CycWord, _, _, CycL)),
   not('preferredGenUnit'(CycL, _, CycWord)).
   
%'relationIndicators'('catalyst', 'Catalyst-TheWord', 'Verb').


pos(CycWord,Phrase,POS,Form):-'lex'(Form, CycWord, Phrase),'lexMap'(PosForms, CycWord, POS).
pos(CycWord,Phrase,POS,_) :- 'partOfSpeech'(CycWord,POS, Phrase).
%'abbreviationForLexicalWord'('Kilogram-TheWord', 'singular', [kg])
pos(CycWord,Phrase,POS,_) :- 'abbreviationForLexicalWord'(CycWord,POS, Phrase).


% 'prepCollocation'('Beset-TheWord', 'Adjective', 'By-TheWord').
posTT(CycWord,Phrase,POS,Form:PosForms):- fail.
     % 'TT-lex'(Form, CycWord, Phrase),not('lex'(_, _, Phrase)),
     %  'TT-lexMap'(PosForms, CycWord, POS).

%'termStrings-GuessedFromName'('GenlsFormat', 'Genls Format').
%   'nounPrep'('Offspring-TheWord', 'Of-TheWord', ['children', ':NOUN', ':OBLIQUE-OBJECT']),



%:-at_initialization(convertCycKb).
:-dynamic(posm_cached).
:-dynamic(posm_cached/5).
:-dynamic(real_posm_cached/5).
:-dynamic(real_posm_cachedTT/5).


  
:-catch(consult('posm_cached_data.pl'),_,true).

%:-posMeans(CycWord,Phrase,POS,Form,CycL).


clean_posm_cache:-
      retractall(posm_cached(CycWord,Phrase,POS,Form,[null])),
      retractall(posm_cached(CycWord,[],POS,Form,CycL)),
      retractall(real_posm_cached(CycWord,_,POS,Form,CycL)),
      retractall(real_posm_cachedTT(CycWord,_,POS,Form,CycL)),
      posm_cached(CycWord,Phrase,POS,Form,CycL),
      once(partition_cache(CycWord,Phrase,POS,Form,CycL)),
      fail.

clean_posm_cache:-tell(foo2),
   listing(real_posm_cached),
   listing(real_posm_cachedTT),
   told.

save_posm_cache
   :-tell('posm_cached_data.pl'),
   listing(posm_cached),
   told.




partition_cache(CycWord,Phrase,POS,Form:'posForms',CycL):-!,partition_cache(CycWord,Phrase,POS,Form,CycL).

partition_cache(CycWord,Phrase,POS,Form,CycL):-
   atom(CycWord),
      atom_concat('TT',_,CycWord),!,
      partition_cacheTT(CycWord,Phrase,POS,Form,CycL).


% ======================================================
% Partitinion CycNL
% ======================================================

posm_cached('Skill-TheWord', [skilled], 'MassNoun', 'regularDegree':'posForms', meaningOfWord('Skill-TheWord')).

partition_cache(CycWord,Phrase,POS,Form,meaningOfWord(CycWord)):-!,
   posm_cached(CycWord,Phrase,_,_,CycL),not(CycL=meaningOfWord(_)),
   assert_if_new(real_posm_cached(CycWord,Phrase,POS,Form,CycL)).
      
   %real_posm_cached('Type-TheWord', [of, geographical, entity, classified, by, hierarchy], 'SimpleNoun', form, 'GeographicalEntityByHierarchy').
partition_cache(CycWord,Phrase,POS,form,CycL):-!,
      posm_cached(CycWord,BPhraseing,POS,Not_form,OMeaning),not(Not_form=form),
      append(BPhraseing,Phrase,OPhrasing),
      partition_cache(CycWord,OPhrasing,POS,Not_form,CycL).
      

partition_cache(CycWord,Phrase,POS,Form,CycL):-!,
   assert_if_new(real_posm_cached(CycWord,Phrase,POS,Form,CycL)).

% ======================================================
% Partitinion TT CycNL
%posm_cached('TTWord-RATP', ['RATP'], 'Noun', 'TTPred-inflNounFemininePluralUnchecked', 'TT-company-RATP')
% ======================================================

% Delete copies of cycNL from TT
partition_cacheTT(_CycWord,Phrase,POS,Form,CycL):-
   posm_cached(OCycWord,Phrase,_,_,_),
   atom(OCycWord),
   not(atom_concat('TT',_,OCycWord)),!.

partition_cacheTT(CycWord,Phrase,POS,Form,meaningOfWord(CycWord)):-
   posm_cached(CycWord,Phrase,_,_,CycL),not(CycL=meaningOfWord(_)),!,
   assert_if_new(real_posm_cachedTT(CycWord,Phrase,POS,Form,CycL)).

partition_cacheTT(CycWord,Phrase,POS,Form,CycL):-!,
   assert_if_new(real_posm_cachedTT(CycWord,Phrase,POS,Form,CycL)).




%:-clean_posm_cache.

/*
   
cycPred('subcatFrameKeywords').
cycPred('nounPrep').
cycPred('verbPrep-Transitive').
cycPred('prepReln-Action').
cycPred('prepReln-Obj').
cycPred('preferredGenUnit').
cycPred('prepSemTrans').
cycPred('subcatFrame').
cycPred('verbSemTrans').
cycPred('genTemplate').
cycPred('multiWordSemTrans').
cycPred('denotationPlaceholder').
cycPred('denotesArgInReln').
cycPred('nounSemTrans').
cycPred('multiWordStringDenotesArgInReln').
cycPred('headMedialString').
cycPred('verbPrep-TransitiveTemplate').
cycPred('agentiveNounSemTrans').
cycPred('nonCompositionalVerbSemTrans').
cycPred('abbreviationForMultiWordString').
cycPred('abbreviationForLexicalWord').
cycPred('formalityOfWS').
cycPred('relationIndicators-Strong').
cycPred('verbSemTransPartial').
cycPred('implies').
cycPred('verbPrep-Passive').
cycPred('compoundStringDenotesArgInReln').
cycPred('genNatTerm-multiWordString').
cycPred('hyphenString').
cycPred('adjSemTrans').
cycPred('properNounSemTrans').
cycPred('morphologicallyDerivedFrom').
cycPred('politenessOfWS').
cycPred('genNatTerm-compoundString').
cycPred('genPhrase').
cycPred('abbreviationForCompoundString').
cycPred('posForms').
cycPred('plural').
cycPred('synonymousExternalConcept').
cycPred('lightVerb-TransitiveSemTrans').
cycPred('genTemplate-Constrained').
cycPred('expansion').
cycPred('compoundSemTrans').
cycPred('genStringAssertion-Old').
cycPred('compoundVerbSemTrans').
cycPred('adjSemTrans-Restricted').
cycPred('massNounSemTrans').
cycPred('morphologicalComposition').
cycPred('determinerAgreement').
cycPred('TT-lexMap').
cycPred('TT-lex').
cycPred('TTPred-denotation').
cycPred('TTPred-thetaRoleFeat-Frequent').
cycPred('TTPred-thetaRoleFeat-Trademark').
cycPred('TTPred-thetaRoleFeat-Informal').
cycPred('TTPred-thetaRoleFeat-LiteraryTechnical').
cycPred('TTPred-thetaRoleFeat-DefiniteArticle').
cycPred('TTPred-thetaRole').
cycPred('TTPred-thetaRoleFeat-ZeroArticle').
cycPred('TTPred-thetaRoleFeat-Slang').
cycPred('TTPred-thetaRoleFeat-Dated').
cycPred('TTPred-thetaRoleFeat-MassNoun').
cycPred('TTPred-thetaRoleFeat-Clause2Only').
cycPred('TTPred-thetaRoleFeat-Coordinator').
cycPred('TTPred-thetaRoleFeat-Abstract').
cycPred('TTPred-thetaRoleFeat-Plural').
cycPred('TTPred-thetaRoleFeat-CommonInflection').
cycPred('TTPred-thetaRoleFeat-AmericanEnglish').
cycPred('TTPred-thetaRoleFeat-BritishEnglish').
cycPred('TTPred-thetaRoleFeat-Infrequent').
cycPred('TTPred-thetaRoleSubcat-Indicative').
cycPred('TTPred-thetaRoleFeat-OtherRegionalDialect').
cycPred('TTPred-thetaRoleFeat-Feminine').
cycPred('TTPred-thetaRoleSubcat-Infinitive').
cycPred('TTPred-thetaRoleSubcat-PresentParticiple').
cycPred('TTPred-thetaRoleFeat-Humorous').
cycPred('TTPred-thetaRoleFeat-Masculine').
cycPred('TTPred-thetaRoleFeat-DoNotReorder').
cycPred('TTPred-thetaRoleFeat-ProgressiveNontaker').
cycPred('TTPred-thetaRoleFeat-BEEBlock').
cycPred('TTPred-thetaRoleFeat-ZBZBlock').
cycPred('TTPred-thetaRoleFeat-BWWBlock').
cycPred('TTPred-thetaRoleFeat-WBWBlock').
cycPred('TTPred-thetaRoleFeat-BZZBlock').
cycPred('TTPred-thetaRoleFeat-TranslationOnly').
cycPred('TTPred-thetaRoleFeat-PreposedAdjective').
cycPred('TTPred-thetaRoleFeat-Singular').
cycPred('TTPred-thetaRoleFeat-Derogatory').
cycPred('TTPred-thetaRoleFeat-Attributive').
cycPred('TTPred-thetaRoleSubcat-Subjunctive').
cycPred('TTPred-thetaRoleFeat-Predicative').
cycPred('TTPred-thetaRoleFeat-Canadian').
cycPred('TTPred-thetaRoleFeat-Tutoiement').
cycPred('TTPred-thetaRoleFeat-Etre').
cycPred('speechPartPreds').
cycPred('genlPreds').
cycPred('afterRemoving').
cycPred('isa').
cycPred('quotedArgument').
cycPred('notAssertible').
cycPred('genKeyword').
cycPred('basicSpeechPartPred').
cycPred('genFormat').
cycPred('arg2Format').
cycPred('argFormat').
cycPred('comparativeDegree').
cycPred('regularSuffix').
cycPred('paraphraseCoercionAllowedFrom').
cycPred('arg1Format').
cycPred('posPredForTemplateCategory').
cycPred('superlativeDegree').
cycPred('relationAllInstance').
cycPred('backchainForbidden').
cycPred('typedGenlPreds').
cycPred('transitiveViaArg').
cycPred('functionalInArgs').
cycPred('ncRuleLabel').
cycPred('keRequirementPreds').
cycPred('ncRuleTemplate').
cycPred('ncRuleConstraint').
cycPred('defaultCorrespondingRoles').
cycPred('headsPhraseOfType').
cycPred('posOfPhraseType').
cycPred('argGenl').
cycPred('barLevelOfPhraseType').
cycPred('transitiveViaArgInverse').
cycPred('nlPhraseTypeForTemplateCategory').
cycPred('denotatumArg').
cycPred('placeName-ShortForm').
cycPred('abnormal').
cycPred('scientificName').
cycPred('keWeakSuggestionPreds').
cycPred('sharedNotes').
cycPred('nameString').
cycPred('termStrings').
cycPred('interArgReln1-3').
cycPred('termStrings-GuessedFromName').
cycPred('acronymString').
cycPred('genFormat-Precise').
cycPred('completeExtentKnown').
cycPred('preferredTermStrings').
cycPred('initialismString').
cycPred('abbreviationString-PN').
cycPred('formerName').
cycPred('preferredNameString').
cycPred('countryName-LongForm').
cycPred('countryName-ShortForm').
cycPred('keStrongSuggestionPreds').
cycPred('arg5Isa').
cycPred('semTransArg').
cycPred('assertTemplate-Reln').
cycPred('interArgIsa3-4').
cycPred('interArgIsa4-5').
cycPred('arg4Format').
cycPred('termPOS-Strings').
cycPred('adjSemTransTemplate').
cycPred('salientAssertions').
cycPred('verbSemTransTemplate').
cycPred('semTransPredForPOS').
cycPred('arg5Format').
cycPred('not').
cycPred('phraseTemplateArg').
cycPred('ist').
cycPred('requiredArg1Pred').
cycPred('genPreferredKeyword').
cycPred('genQuestion').
cycPred('genExpansion').
cycPred('genStringAssertion').
cycPred('genFormat-ArgFixed').
cycPred('genStringAssertion-Precise').
cycPred('genFormat-NP').
cycPred('genNatTerm-ArgLast').
cycPred('genCodeSupport').
cycPred('relationAll').
cycPred('unitOfMeasurePrefixString').
cycPred('generateQuantOverArg').
cycPred('interArgIsa1-3').
cycPred('generateArgWithOutsideScope').
cycPred('interArgIsa1-2').
cycPred('formalityOfWS-New').
cycPred('languageOfLexicon').
cycPred('arg6Isa').
cycPred('abbreviationForString').
cycPred('instancesDontNeedLexification').
cycPred('lexicalWordTypeForLanguage').
cycPred('psRuleTemplateBindings').
cycPred('genlFuncs').
cycPred('resultIsaArgIsa').
cycPred('reformulatorEquiv').
cycPred('reformulatorEquals').
cycPred('reformulationPrecondition').
cycPred('subcatFrameDependentConstraint').
cycPred('subcatFrameArity').
cycPred('subcatFrameDependentKeyword').
cycPred('subcatFrameExample').
cycPred('prefixString').
cycPred('derivedUsingPrefix').
cycPred('relationAllExists').
cycPred('baseForm').
cycPred('derivedUsingSuffix').
cycPred('negationInverse').
cycPred('relationExistsAll').
cycPred('posBaseForms').
cycPred('suffixString').
cycPred('phoneticVariantOfPrefix').
cycPred('phoneticVariantOfSuffix').
cycPred('relationAllExistsMin').
cycPred('derivationalAffixBasePOS').
cycPred('affixRuleArity').
cycPred('affixRuleCategorialConstraint').
cycPred('derivationalAffixResultPOS').
cycPred('affixRuleTypeMorphemePosition').
cycPred('etymologicalVariantOfSuffix').
cycPred('variantOfSuffix').
cycPred('relationInstanceAll').
cycPred('genls').
cycPred('disjointWith').
cycPred('coExtensional').
cycPred('partitionedInto').
cycPred('notAssertibleCollection').
cycPred('typeGenls').
cycPred('resultIsaArg').
cycPred('resultGenlArg').
cycPred('arityMin').
cycPred('arityMax').
cycPred('argAndRestIsa').
cycPred('functionCorrespondingPredicate-Canonical').
cycPred('argsIsa').
cycPred('evaluationDefn').
cycPred('psRuleArity').
cycPred('psRuleSyntacticHeadDtr').
cycPred('psRuleTemplateDtr').
cycPred('psRuleConstraint').
cycPred('psRuleCategory').
cycPred('psRuleExample').
cycPred('psRuleSemanticsHandler').
cycPred('keConsiderationPreds').
cycPred('genlAttributes').
cycPred('negationAttribute').
cycPred('TTPred-thoughtTreasureToCyc').
cycPred('rewriteOf').
cycPred('TTPred-cycToThoughtTreasure').
cycPred('psRuleSemanticsFromDtr').
cycPred('posForTemplateCategory').
cycPred('backchainRequired').
cycPred('defnIff').
cycPred('completeCollectionExtent').
cycPred('quotedCollection').
cycPred('keClarifyingCollection').
cycPred('relationAllExistsCount').
cycPred('rolesForEventType').
cycPred('defaultReformulationDirectionInModeForPred').
cycPred('keStrongConsiderationPreds').
cycPred('keStrongSuggestion').
cycPred('requiredActorSlots').
cycPred('collectionUnion').
cycPred('keCommonQueryPredForInstances').
cycPred('subjectRoles').
cycPred('trueRule').
cycPred('TTPred-processor-of').
cycPred('TTPred-create').
cycPred('TTPred-eng-aux-verb-of').
cycPred('TTPred-cpart-of').
cycPred('TTPred-capital-of').
cycPred('TTPred-polity-of').
cycPred('TTPred-first-surname-of').
cycPred('TTPred-performed-in').
cycPred('TTPred-part-of').
cycPred('TTPred-processors-of').
cycPred('TTPred-owner-of').
cycPred('TTPred-cost-of').
cycPred('TTPred-first-name-of').
cycPred('TTPred-population-of').
cycPred('TTPred-antonym-of').
cycPred('TTPred-time-range-of').
cycPred('TTPred-event02-of').
cycPred('TTPred-fr-infl-tense-of').
cycPred('TTPred-role07-of').
cycPred('TTPred-stereo').
cycPred('TTPred-NTSC').
cycPred('TTPred-phone-prefix-of').
cycPred('TTPred-variant-of').
cycPred('TTPred-r1').
cycPred('TTPred-time-off').
cycPred('TTPred-diminutive-of').
cycPred('TTPred-value-of').
cycPred('TTPred-result-of').
cycPred('TTPred-eng-aux-tense-of').
cycPred('TTPred-fr-subjunctive-of').
cycPred('TTPred-street-of').
cycPred('TTPred-feed-of').
cycPred('TTPred-role03-script-of').
cycPred('TTPred-atomic-weight-of').
cycPred('TTPred-political-affiliation-of').
cycPred('TTPred-dark-brown').
cycPred('TTPred-violet').
cycPred('TTPred-SECAM').
cycPred('TTPred-fr').
cycPred('TTPred-product-of').
cycPred('TTPred-computer-bus-of').
cycPred('TTPred-event03-of').
cycPred('TTPred-attend-twelfth-grade').
cycPred('TTPred-inside').
cycPred('TTPred-street-number-of').
cycPred('TTPred-period-of').
cycPred('TTPred-role02-of').
cycPred('TTPred-MIPS-of').
cycPred('TTPred-attr-occultist').
cycPred('TTPred-canonical-factor-of').
cycPred('TTPred-diploma-of').
cycPred('TTPred-event04-of').
cycPred('TTPred-duration-of').
cycPred('TTPred-specialty-of').
cycPred('TTPred-attr-rel-value').
cycPred('TTPred-eng-infl-mood-of').
cycPred('TTPred-fr-size-of').
cycPred('TTPred-us').
cycPred('TTPred-producer-of').
cycPred('TTPred-second-name-of').
cycPred('TTPred-emotion-of').
cycPred('TTPred-event01-of').
cycPred('TTPred-silk').
cycPred('TTPred-max-value-of').
cycPred('TTPred-preppy').
cycPred('TTPred-frequency-of').
cycPred('TTPred-attend-kindergarten').
cycPred('TTPred-politically-ultraconservative').
cycPred('TTPred-other-language-of').
cycPred('TTPred-SPECintRate92-of').
cycPred('TTPred-role02-script-of').
cycPred('TTPred-event05-of').
cycPred('TTPred-slots-of').
cycPred('TTPred-attr-South-Korean').
cycPred('TTPred-coordinates-of').
cycPred('TTPred-goal-of').
cycPred('TTPred-incorporated-in').
cycPred('TTPred-green').
cycPred('TTPred-male').
cycPred('TTPred-clothing-middle').
cycPred('TTPred-female').
cycPred('TTPred-href').
cycPred('TTPred-red').
cycPred('TTPred-r8').
cycPred('TTPred-many-to-one').
cycPred('TTPred-clothing-bottom').
cycPred('TTPred-role05-of').
cycPred('TTPred-dark-gray').
cycPred('TTPred-nationality-of').
cycPred('TTPred-activated-emotion-of').
cycPred('TTPred-brown').
cycPred('TTPred-headquarters-of').
cycPred('TTPred-attr-Liverpudlian').
cycPred('TTPred-attr-polytheist').
cycPred('TTPred-event09-of').
cycPred('TTPred-actor-of').
cycPred('TTPred-antiparticle-of').
cycPred('TTPred-baryon-number-of').
cycPred('TTPred-role01-of').
cycPred('TTPred-thin-stripes').
cycPred('TTPred-fr-infl-mood-of').
cycPred('TTPred-gray').
cycPred('TTPred-clothing-top').
cycPred('TTPred-max-value1-of').
cycPred('TTPred-first-OS-of').
cycPred('TTPred-row-distance-of').
cycPred('TTPred-event11-of').
cycPred('TTPred-religion-of').
cycPred('TTPred-SPECint92-of').
cycPred('TTPred-Dacron').
cycPred('TTPred-r3').
cycPred('TTPred-postal-code-of').
cycPred('TTPred-event22-of').
cycPred('TTPred-r2').
cycPred('TTPred-role04-of').
cycPred('TTPred-symmetric').
cycPred('TTPred-official-language-of').
cycPred('TTPred-leadto1').
cycPred('TTPred-height-of').
cycPred('TTPred-sphere').
cycPred('TTPred-white').
cycPred('TTPred-luminance-of').
cycPred('TTPred-acrylic').
cycPred('TTPred-clothing-ankle').
cycPred('TTPred-fr-aux-tense-of').
cycPred('TTPred-event06-of').
cycPred('TTPred-text-ref').
cycPred('TTPred-us-size-of').
cycPred('TTPred-min-value-of').
cycPred('TTPred-length-of').
cycPred('TTPred-atomic-number-of').
cycPred('TTPred-affiliation-of').
cycPred('TTPred-humanmade').
cycPred('TTPred-made-in').
cycPred('TTPred-attr-ENA').
cycPred('TTPred-virtual-memory-of').
cycPred('TTPred-nonencrypted').
cycPred('TTPred-MFLOPS-of').
cycPred('TTPred-contralto').
cycPred('TTPred-SPECmark89-of').
cycPred('TTPred-has-ceiling').
cycPred('TTPred-travel-max-speed-of').
cycPred('TTPred-event12-of').
cycPred('TTPred-fr-tense-0-of').
cycPred('TTPred-triangles').
cycPred('TTPred-travel-cargo-capacity-of').
cycPred('TTPred-rhs-assertion-of').
cycPred('TTPred-eng-main-tense-of').
cycPred('TTPred-role10-of').
cycPred('TTPred-phone-number-of').
cycPred('TTPred-attend-day-care').
cycPred('TTPred-event08-of').
cycPred('TTPred-event17-of').
cycPred('TTPred-attr-rel-range').
cycPred('TTPred-topic-of').
cycPred('TTPred-inverse-of').
cycPred('TTPred-occupation-of').
cycPred('TTPred-isospin-of').
cycPred('TTPred-fr-main-tense-of').
cycPred('TTPred-canonical-of').
cycPred('TTPred-original-run').
cycPred('TTPred-event15-of').
cycPred('TTPred-first-author-of').
cycPred('TTPred-weight-of').
cycPred('TTPred-clock-frequency-of').
cycPred('TTPred-residence-of').
cycPred('TTPred-intelligent').
cycPred('TTPred-post-title-of').
cycPred('TTPred-shirtlayer').
cycPred('TTPred-RAM-of').
cycPred('TTPred-blue').
cycPred('TTPred-topological-genus-of').
cycPred('TTPred-saturation-of').
cycPred('TTPred-arch-of').
cycPred('TTPred-electric-charge-of').
cycPred('TTPred-r4').
cycPred('TTPred-vitamin-B2').
cycPred('TTPred-cotton').
cycPred('TTPred-blue-green').
cycPred('TTPred-wool').
cycPred('TTPred-attr-Anglican').
cycPred('TTPred-diameter-of').
cycPred('TTPred-eng-infl-adverb-of').
cycPred('TTPred-studio-of').
cycPred('TTPred-strangeness-of').
cycPred('TTPred-spin-of').
cycPred('TTPred-lhs-class-of').
cycPred('TTPred-role03-of').
cycPred('TTPred-famous').
cycPred('TTPred-underlying-of').
cycPred('TTPred-case-of').
cycPred('TTPred-counter-tenor').
cycPred('TTPred-gen-max-of').
cycPred('TTPred-unwalkable').
cycPred('TTPred-entry-condition-of').
cycPred('TTPred-politically-radical-socialiste').
cycPred('TTPred-event07-of').
cycPred('TTPred-FPU-of').
cycPred('TTPred-magenta').
cycPred('TTPred-avoid').
cycPred('TTPred-orientation-of').
cycPred('TTPred-third-name-of').
cycPred('TTPred-baritone').
cycPred('TTPred-fine-weave').
cycPred('TTPred-light-gray').
cycPred('TTPred-rhs-feat-of').
cycPred('TTPred-clothing-foot').
cycPred('TTPred-affiliate-of').
cycPred('TTPred-schizophrenia').
cycPred('TTPred-col-distance-of').
cycPred('TTPred-attr-Baptist').
cycPred('TTPred-frying-of').
cycPred('TTPred-comment').
cycPred('TTPred-stripes').
cycPred('TTPred-mother-of').
cycPred('TTPred-role13-of').
cycPred('TTPred-host-of').
cycPred('TTPred-adult').
cycPred('TTPred-min-value2-of').
cycPred('TTPred-tt-ticker-of').
cycPred('TTPred-vertical-polarization').
cycPred('TTPred-skeptical').
cycPred('TTPred-ca').
cycPred('TTPred-seat-of').
cycPred('TTPred-preposition-of').
cycPred('TTPred-eng-infl-tense-of').
cycPred('TTPred-fr-tense-neg-4-of').
cycPred('TTPred-lhs-pos-of').
cycPred('TTPred-fr-literary-subjunctive-of').
cycPred('TTPred-litigious').
cycPred('TTPred-role06-of').
cycPred('TTPred-cardioid').
cycPred('TTPred-omnidirectional').
cycPred('TTPred-fr-tense-pos-2-of').
cycPred('TTPred-black').
cycPred('TTPred-clothing-hand').
cycPred('TTPred-optimistic').
cycPred('TTPred-ap').
cycPred('TTPred-event19-of').
cycPred('TTPred-walkable').
cycPred('TTPred-SPECfpRate92-of').
cycPred('TTPred-level-of').
cycPred('TTPred-eng-tense-0-of').
cycPred('TTPred-rhs-pos-of').
cycPred('TTPred-figure-8').
cycPred('TTPred-serve-meal').
cycPred('TTPred-bigoted').
cycPred('TTPred-middle-aged-adult').
cycPred('TTPred-role18-of').
cycPred('TTPred-anchor-of').
cycPred('TTPred-pdg-of').
cycPred('TTPred-lighter-than-air').
cycPred('TTPred-attr-Canadian').
cycPred('TTPred-fr-aux-verb-of').
cycPred('TTPred-manufacturer-of').
cycPred('TTPred-eng-tense-neg-5-of').
cycPred('TTPred-believe').
cycPred('TTPred-sister-of').
cycPred('TTPred-superior-of').
cycPred('TTPred-attr-Irish').
cycPred('TTPred-exchange-ticker-of').
cycPred('TTPred-amber').
cycPred('TTPred-role08-of').
cycPred('TTPred-ideal-sleep-of').
cycPred('TTPred-Caucasian').
cycPred('TTPred-politically-subversive').
cycPred('TTPred-attr-Chinese').
cycPred('TTPred-eat').
cycPred('TTPred-tweed').
cycPred('TTPred-one-to-one').
cycPred('TTPred-pink').
cycPred('TTPred-waveform-of').
cycPred('TTPred-light-blue').
cycPred('TTPred-fanciful').
cycPred('TTPred-success-emotion-of').
cycPred('TTPred-aunt-of').
cycPred('TTPred-unique-author-of').
cycPred('TTPred-event27-of').
cycPred('TTPred-barrier-isa').
cycPred('TTPred-politically-Leninist').
cycPred('TTPred-event14-of').
cycPred('TTPred-free-object').
cycPred('TTPred-gen-min-of').
cycPred('TTPred-travel-max-distance-of').
cycPred('TTPred-currency-of').
cycPred('TTPred-tie-dye').
cycPred('TTPred-attr-Kashmiri').
cycPred('TTPred-isbn-of').
cycPred('TTPred-dots').
cycPred('TTPred-unit-of').
cycPred('TTPred-overlayer').
cycPred('TTPred-event10-of').
cycPred('TTPred-taped').
cycPred('TTPred-event16-of').
cycPred('TTPred-cycle-time-of').
cycPred('TTPred-stored-in').
cycPred('TTPred-r9').
cycPred('TTPred-sold-at').
cycPred('TTPred-event29-of').
cycPred('TTPred-event13-of').
cycPred('TTPred-attr-Swiss').
cycPred('TTPred-live').
cycPred('TTPred-surface-area-of').
cycPred('TTPred-eng-tense-pos-3-of').
cycPred('TTPred-fr-translation-of').
cycPred('TTPred-attr-African').
cycPred('TTPred-similar').
cycPred('TTPred-attr-Menton').
cycPred('TTPred-related-concept-of').
cycPred('TTPred-language-of').
cycPred('TTPred-closed-captioned').
cycPred('TTPred-eng-tense-neg-4-of').
cycPred('TTPred-used-for').
cycPred('TTPred-infant').
cycPred('TTPred-psychosis').
cycPred('TTPred-film-converted-to-NTSC').
cycPred('TTPred-attend-eighth-grade').
cycPred('TTPred-light-brown').
cycPred('TTPred-stitch-strings').
cycPred('TTPred-politically-Owenite').
cycPred('TTPred-model-number-of').
cycPred('TTPred-professional-product').
cycPred('TTPred-used-at').
cycPred('TTPred-attr-Neuilly').
cycPred('TTPred-polyester').
cycPred('TTPred-event18-of').
cycPred('TTPred-role11-of').
cycPred('TTPred-role09-of').
cycPred('TTPred-politically-Republican').
cycPred('TTPred-response-of').
cycPred('TTPred-leadto12').
cycPred('TTPred-r7').
cycPred('TTPred-min-value1-of').
cycPred('TTPred-power-of').
cycPred('TTPred-attr-North-Korean').
cycPred('TTPred-rhs-class-of').
cycPred('TTPred-yellow').
cycPred('TTPred-fr-tense-literary-neg-4-of').
cycPred('TTPred-event25-of').
cycPred('TTPred-fr-tense-pos-1-of').
cycPred('TTPred-SPECfp92-of').
cycPred('TTPred-politically-Social-Democratic').
cycPred('TTPred-mezzo-soprano').
cycPred('TTPred-computer-chassis-of').
cycPred('TTPred-eng-translation-of').
cycPred('TTPred-politically-phalansterian').
cycPred('TTPred-attend-graduate-school').
cycPred('TTPred-time-on').
cycPred('TTPred-attend-seventh-grade').
cycPred('TTPred-uk').
cycPred('TTPred-print').
cycPred('TTPred-role06-script-of').
cycPred('TTPred-underlayer').
cycPred('TTPred-skill-of-play').
cycPred('TTPred-attr-Scottish').
cycPred('TTPred-eng-progressive-of').
cycPred('TTPred-eng-tense-pos-2-of').
cycPred('TTPred-politically-nazi').
cycPred('TTPred-UK-eng-subjunctive-of').
cycPred('TTPred-attr-libertine').
cycPred('TTPred-travel-crew-of').
cycPred('TTPred-fr-tense-neg-3-of').
cycPred('TTPred-attr-Czech').
cycPred('TTPred-diagonal-length-of').
cycPred('TTPred-crosses').
cycPred('TTPred-cyan').
cycPred('TTPred-Ceefax').
cycPred('TTPred-unit2-of').
cycPred('TTPred-fruit-of').
cycPred('TTPred-last-OS-of').
cycPred('TTPred-attend-ninth-grade').
cycPred('TTPred-very-old-adult').
cycPred('TTPred-annoying').
cycPred('TTPred-champagne').
cycPred('TTPred-gestation-period-of').
cycPred('TTPred-groggy').
cycPred('TTPred-steel').
cycPred('TTPred-thin-checker').
cycPred('TTPred-cylinder').
cycPred('TTPred-create-paint').
cycPred('TTPred-vitamin-A').
cycPred('TTPred-clothing-calf').
cycPred('TTPred-minitel-number-of').
cycPred('TTPred-vestlayer').
cycPred('TTPred-charm-of').
cycPred('TTPred-inexperienced').
cycPred('TTPred-illegal').
cycPred('TTPred-leadto2').
cycPred('TTPred-coatlayer').
cycPred('TTPred-attr-Episcopalian').
cycPred('TTPred-child').
cycPred('TTPred-creator-of').
cycPred('TTPred-gold').
cycPred('TTPred-attr-Zen-Buddhist').
cycPred('TTPred-r6').
cycPred('TTPred-max-value2-of').
cycPred('TTPred-attr-British').
cycPred('TTPred-event32-of').
cycPred('TTPred-role16-of').
cycPred('TTPred-attr-Greek-Orthodox').
cycPred('TTPred-fr-tense-neg-7-of').
cycPred('TTPred-unit1-of').
cycPred('TTPred-snobby').
cycPred('TTPred-attr-Burgundian').
cycPred('TTPred-attend-school').
cycPred('TTPred-width-of').
cycPred('TTPred-video-channel-of').
cycPred('TTPred-soprano').
cycPred('TTPred-attr-Protestant').
cycPred('TTPred-next-state-of').
cycPred('TTPred-cowardly').
cycPred('TTPred-OS-of').
cycPred('TTPred-attr-doer').
cycPred('TTPred-apolitical').
cycPred('TTPred-event20-of').
cycPred('TTPred-wife-of').
cycPred('TTPred-composer-of').
cycPred('TTPred-fixed-object').
cycPred('TTPred-white-wine').
cycPred('TTPred-neurosis').
cycPred('TTPred-small-squares').
cycPred('TTPred-radio-station-of').
cycPred('TTPred-Shetland-wool').
cycPred('TTPred-politically-Maoist').
cycPred('TTPred-prejudiced').
cycPred('TTPred-attr-Sorbonne').
cycPred('TTPred-issuer-of').
cycPred('TTPred-r5').
cycPred('TTPred-computer-monitor-of').
cycPred('TTPred-role04-script-of').
cycPred('TTPred-politically-progressive').
cycPred('TTPred-calfskin').
cycPred('TTPred-bass-baritone').
cycPred('TTPred-clothing-thigh').
cycPred('TTPred-example').
cycPred('TTPred-attr-Lutheran').
cycPred('TTPred-calfskin-velvet').
cycPred('TTPred-politically-fascist').
cycPred('TTPred-worsted').
cycPred('TTPred-crosshatch').
cycPred('TTPred-translation-of').
cycPred('TTPred-attr-East-Ender').
cycPred('TTPred-event23-of').
cycPred('TTPred-publish').
cycPred('TTPred-clothing-wrist').
cycPred('TTPred-event21-of').
cycPred('TTPred-supercardioid').
cycPred('TTPred-lazy').
cycPred('TTPred-attr-Vedaic').
cycPred('TTPred-glove-leather').
cycPred('TTPred-fine-stitch').
cycPred('TTPred-vitamin-B5').
cycPred('TTPred-politically-anticapitalist').
cycPred('TTPred-attr-shamanist').
cycPred('TTPred-corduroy').
cycPred('TTPred-olive-green').
cycPred('TTPred-eng-tense-pos-1-of').
cycPred('TTPred-first-editor-of').
cycPred('TTPred-humorous').
cycPred('TTPred-mono').
cycPred('TTPred-attend-eleventh-grade').
cycPred('TTPred-RIC-of').
cycPred('TTPred-khaki-color').
cycPred('TTPred-politically-liberal').
cycPred('TTPred-role12-of').
cycPred('TTPred-roommate-of').
cycPred('TTPred-fr-tense-neg-6-of').
cycPred('TTPred-old-adult').
cycPred('TTPred-role17-of').
cycPred('TTPred-attr-Hongkong').
cycPred('TTPred-attr-anti-religious').
cycPred('TTPred-pin-stripe').
cycPred('TTPred-rayon').
cycPred('TTPred-leather').
cycPred('TTPred-encrypted').
cycPred('TTPred-wide-angle-cardioid').
cycPred('TTPred-politically-collectivist').
cycPred('TTPred-official-residence-of').
cycPred('TTPred-politically-right-wing').
cycPred('TTPred-attend-technical-school').
cycPred('TTPred-attr-Jaina').
cycPred('TTPred-drivable').
cycPred('TTPred-politically-conservative').
cycPred('TTPred-attend-junior-college').
cycPred('TTPred-attr-Shintoist').
cycPred('TTPred-trade-arbitrage').
cycPred('TTPred-politically-Tory').
cycPred('TTPred-acetate').
cycPred('TTPred-second-author-of').
cycPred('TTPred-thirty-something').
cycPred('TTPred-architect-of').
cycPred('TTPred-attr-rel-proportional').
cycPred('TTPred-nonborn').
cycPred('TTPred-delayed').
cycPred('TTPred-fabric-linen').
cycPred('TTPred-politically-Marxist-Leninist').
cycPred('TTPred-entertaining').
cycPred('TTPred-unsuccessful').
cycPred('TTPred-attr-Danish').
cycPred('TTPred-cone').
cycPred('TTPred-abstinent').
cycPred('TTPred-young-adult').
cycPred('TTPred-nonreligious').
cycPred('TTPred-dark-blue').
cycPred('TTPred-eng-tense-literary-neg-4-of').
cycPred('TTPred-create-write-music').
cycPred('TTPred-attr-Taoist').
cycPred('TTPred-attr-Mormon').
cycPred('TTPred-politically-radical').
cycPred('TTPred-cusip-of').
cycPred('TTPred-politically-royalist').
cycPred('TTPred-overlayerpost').
cycPred('TTPred-attr-Finnish').
cycPred('TTPred-create-draw').
cycPred('TTPred-eng-tense-neg-7-of').
cycPred('TTPred-vitamin-B9').
cycPred('TTPred-attr-New-York').
cycPred('TTPred-hypercardioid').
cycPred('TTPred-liquid').
cycPred('TTPred-employer-of').
cycPred('TTPred-attr-agnostic').
cycPred('TTPred-super-100-CycL').
cycPred('TTPred-fr-tense-neg-5-of').
cycPred('TTPred-attend-first-grade').
cycPred('TTPred-attr-Orthodox-Eastern-Church').
cycPred('TTPred-travel-passengers-of').
cycPred('TTPred-secretary-of').
cycPred('TTPred-key-of').
cycPred('TTPred-eng-tense-neg-3-of').
cycPred('TTPred-sick').
cycPred('TTPred-rare').
cycPred('TTPred-Hispanic').
cycPred('TTPred-bottomness-of').
cycPred('TTPred-suburban').
cycPred('TTPred-US-eng-subjunctive-of').
cycPred('TTPred-coupon-of').
cycPred('TTPred-floral').
cycPred('TTPred-attr-kharidjite').
cycPred('TTPred-attr-Christian').
cycPred('TTPred-bits-of').
cycPred('TTPred-fr-tense-neg-2-of').
cycPred('TTPred-nylon').
cycPred('TTPred-attr-monotheist').
cycPred('TTPred-chine-cotton').
cycPred('TTPred-attend-junior-high-school').
cycPred('TTPred-tenor').
cycPred('TTPred-market-of').
cycPred('TTPred-attr-Korean').
cycPred('TTPred-underlayerpre').
cycPred('TTPred-politically-yippie').
cycPred('TTPred-researcher-of').
cycPred('TTPred-politically-individualistic').
cycPred('TTPred-natural-parent-of').
cycPred('TTPred-vitamin-B12').
cycPred('TTPred-politically-anarcho-syndicalist').
cycPred('TTPred-blue-gray').
cycPred('TTPred-checker').
cycPred('TTPred-attr-Episcopal').
cycPred('TTPred-attr-Asian').
cycPred('TTPred-event31-of').
cycPred('TTPred-ivory-colored').
cycPred('TTPred-circumference-of').
cycPred('TTPred-politically-left-wing-radical').
cycPred('TTPred-attr-X').
cycPred('TTPred-second-editor-of').
cycPred('TTPred-eng-tense-neg-6-of').
cycPred('TTPred-market-manipulation').
cycPred('TTPred-insider-trading').
cycPred('TTPred-role01-script-of').
cycPred('TTPred-spinoff-of').
cycPred('TTPred-embryonic').
cycPred('TTPred-attr-Calvinist').
cycPred('TTPred-copper').
cycPred('TTPred-politically-Stalinist').
cycPred('TTPred-attr-Catholic').
cycPred('TTPred-Panda1').
cycPred('TTPred-circadian-rhythm-of').
cycPred('TTPred-iodine').
cycPred('TTPred-nerdy').
cycPred('TTPred-PAL').
cycPred('TTPred-small-dots').
cycPred('TTPred-attend-third-grade').
cycPred('TTPred-attr-Jewish').
cycPred('TTPred-poor').
cycPred('TTPred-bars').
cycPred('TTPred-event28-of').
cycPred('TTPred-role14-of').
cycPred('TTPred-distillation-of').
cycPred('TTPred-attr-Dutch').
cycPred('TTPred-attr-Swedish').
cycPred('TTPred-politically-Democratic').
cycPred('TTPred-attr-Afghan').
cycPred('TTPred-attr-Mennonite').
cycPred('TTPred-create-write-literature').
cycPred('TTPred-viscose').
cycPred('TTPred-addiction-of').
cycPred('TTPred-squares').
cycPred('TTPred-rubber').
cycPred('TTPred-attr-Muslem').
cycPred('TTPred-attr-wrongdoing').
cycPred('TTPred-politically-Saint-Simonian').
cycPred('TTPred-bullying').
cycPred('TTPred-attend-secondary-school').
cycPred('TTPred-attr-Indian').
cycPred('TTPred-carrier-of').
cycPred('TTPred-attr-Russian-Orthodox').
cycPred('TTPred-dead').
cycPred('TTPred-talkative').
cycPred('TTPred-writer-of').
cycPred('TTPred-attr-Confucianist').
cycPred('TTPred-create-arrange').
cycPred('TTPred-noble').
cycPred('TTPred-attend-medical-school').
cycPred('TTPred-unkind').
cycPred('TTPred-Sanforized').
cycPred('TTPred-fly').
cycPred('TTPred-second-surname-of').
cycPred('TTPred-eng-tense-literary-0-of').
cycPred('TTPred-skin-material-resin').
cycPred('TTPred-can-lift').
cycPred('TTPred-attend-doctoral-program').
cycPred('TTPred-ellipsoid').
cycPred('TTPred-introverted').
cycPred('TTPred-attr-Tokyoite').
cycPred('TTPred-ceo-of').
cycPred('TTPred-father-of').
cycPred('TTPred-polka-dot').
cycPred('TTPred-politically-reactionary').
cycPred('TTPred-event24-of').
cycPred('TTPred-herringbone').
cycPred('TTPred-orange').
cycPred('TTPred-attr-German').
cycPred('TTPred-vitamin-E').
cycPred('TTPred-clothing-forearm').
cycPred('TTPred-attr-Buddhist').
cycPred('TTPred-male-chauvinist').
cycPred('TTPred-politically-capitalist').
cycPred('TTPred-politically-socialist').
cycPred('TTPred-attr-Polish').
cycPred('TTPred-eng-tense-neg-2-of').
cycPred('TTPred-politically-moderate').
cycPred('TTPred-dg-adjoint-of').
cycPred('TTPred-attr-chafiite').
cycPred('TTPred-smooth').
cycPred('TTPred-splotches').
cycPred('TTPred-silent').
cycPred('TTPred-VRAM-of').
cycPred('TTPred-director-of').
cycPred('TTPred-attr-hanafite').
cycPred('TTPred-combed-cotton').
cycPred('TTPred-clothing-knee').
cycPred('TTPred-BW').
cycPred('TTPred-fr-tense-neg-1-of').
cycPred('TTPred-black-leather').
cycPred('TTPred-attr-Slovakian').
cycPred('TTPred-attr-pantheist').
cycPred('TTPred-nubuck').
cycPred('TTPred-attr-Spanish').
cycPred('TTPred-bass').
cycPred('TTPred-political').
cycPred('TTPred-attr-Londoner').
cycPred('TTPred-amplitude-modulation-of').
cycPred('TTPred-construction-membrane').
cycPred('TTPred-attr-ENS').
cycPred('TTPred-orange-red').
cycPred('TTPred-heavier-than-air').
cycPred('TTPred-attr-American').
cycPred('TTPred-narrator-of').
cycPred('TTPred-small-chevrons').
cycPred('TTPred-extroverted').
cycPred('TTPred-vitamin-B3').
cycPred('TTPred-politically-Fabian').
cycPred('TTPred-attr-Biarritz').
cycPred('TTPred-Black').
cycPred('TTPred-failure-emotion-of').
cycPred('TTPred-brother-of').
cycPred('TTPred-attr-Hindu').
cycPred('TTPred-event26-of').
cycPred('TTPred-politically-state-socialist').
cycPred('TTPred-jacketlayer').
cycPred('TTPred-Nagravision').
cycPred('TTPred-politically-Bolshevist').
cycPred('TTPred-attend-sixth-grade').
cycPred('TTPred-attr-Italian').
cycPred('TTPred-attr-Shiite').
cycPred('TTPred-attr-materialist').
cycPred('TTPred-rural').
cycPred('TTPred-atom-nickel').
cycPred('TTPred-politically-Marxist').
cycPred('TTPred-politically-progressiste').
cycPred('TTPred-fr-tense-pos-3-of').
cycPred('TTPred-light-violet').
cycPred('TTPred-iron').
cycPred('TTPred-blackcurrant-liqueur').
cycPred('TTPred-attend-nursery-school').
cycPred('TTPred-politically-worker').
cycPred('TTPred-trade-speculate').
cycPred('TTPred-attr-Taiwanese').
cycPred('TTPred-levels-of').
cycPred('TTPred-attr-Methodist').
cycPred('TTPred-event30-of').
cycPred('TTPred-gas').
cycPred('TTPred-attend-elementary-school').
cycPred('TTPred-bronze').
cycPred('TTPred-attr-New-Jersey').
cycPred('TTPred-fluent-language-of').
cycPred('TTPred-politically-left-wing').
cycPred('TTPred-fr-tense-literary-neg-7-of').
cycPred('TTPred-department-head-of').
cycPred('TTPred-attr-Northern-Irish').
cycPred('TTPred-role15-of').
cycPred('TTPred-urban').
cycPred('TTPred-attr-Roman-Catholic').
cycPred('TTPred-ptrans-swim').
cycPred('TTPred-politically-nationalist').
cycPred('TTPred-underlayerpost').
cycPred('TTPred-politically-syndicalist').
cycPred('TTPred-circles').
cycPred('TTPred-teenager').
cycPred('TTPred-politically-nihilist').
cycPred('TTPred-attend-law-school').
cycPred('TTPred-unintelligent').
cycPred('TTPred-attr-Czechoslovakian').
cycPred('TTPred-gullible').
cycPred('TTPred-ovals').
cycPred('TTPred-J17').
cycPred('TTPred-bouncy').
cycPred('TTPred-tail-length-of').
cycPred('TTPred-vitamin-B1').
cycPred('TTPred-burgundy').
cycPred('TTPred-denim').
cycPred('TTPred-attr-Anglo-Catholic').
cycPred('TTPred-attr-European').
cycPred('TTPred-attr-Welsh').
cycPred('TTPred-good').
cycPred('TTPred-eng-tense-neg-1-of').
cycPred('TTPred-radius-of').
cycPred('TTPred-weave').
cycPred('TTPred-attr-Zoroastrian').
cycPred('TTPred-product-release').
cycPred('TTPred-attend-fifth-grade').
cycPred('TTPred-lucky').
cycPred('TTPred-teach').
cycPred('TTPred-pacifist').
cycPred('TTPred-clumsy').
cycPred('TTPred-attr-Californian').
cycPred('TTPred-edition-of').
cycPred('TTPred-attend-second-grade').
cycPred('TTPred-attr-atheist').
cycPred('TTPred-do-postdoctoral-work').
cycPred('TTPred-fr-progressive-of').
cycPred('TTPred-rich').
cycPred('TTPred-attr-good-Samaritan').
cycPred('TTPred-politically-Titoist').
cycPred('TTPred-politically-neonazi').
cycPred('TTPred-attr-Japanese').
cycPred('TTPred-technical').
cycPred('TTPred-politically-Trotskyite').
cycPred('TTPred-attend-college').
cycPred('TTPred-attr-Martinique').
cycPred('TTPred-executive-producer-of').
cycPred('TTPred-fetal').
cycPred('TTPred-racist').
cycPred('TTPred-attr-English').
cycPred('TTPred-sky-blue').
cycPred('TTPred-husband-of').
cycPred('TTPred-vitamin-B6').
cycPred('TTPred-rollable').
cycPred('TTPred-attr-witch').
cycPred('TTPred-attend-preschool').
cycPred('TTPred-attr-animist').
cycPred('TTPred-not').
cycPred('TTPred-attend-MBA-program').
cycPred('TTPred-attend-fourth-grade').
cycPred('TTPred-attr-Ile-de-France').
cycPred('TTPred-politically-revolutionary').
cycPred('TTPred-in-color').
cycPred('TTPred-politically-communist').
cycPred('TTPred-attr-Seventh-Day-Adventist').
cycPred('TTPred-yellow-green').
cycPred('TTPred-attend-tenth-grade').
cycPred('TTPred-attr-malekite').
cycPred('TTPred-politically-Castroite').
cycPred('TTPred-large-stripes').
cycPred('TTPred-politically-Marxist-revisionist').
cycPred('TTPred-politically-extremist').
cycPred('TTPred-attr-Douarnenez').
cycPred('TTPred-attr-Sunnite').
cycPred('TTPred-tielayer').
cycPred('TTPred-attr-Adventist').
cycPred('TTPred-content-of').
cycPred('TTPred-Asian').
cycPred('TTPred-can-hold').
cycPred('TTPred-politically-anarchist').
cycPred('TTPred-fan-of').
cycPred('TTPred-unique-translator-of').
cycPred('TTPred-large-crosshatch').
cycPred('TTPred-attr-Thai').
cycPred('TTPred-attr-French').
cycPred('TTPred-unique-editor-of').
cycPred('TTPred-vitamin-C').
cycPred('TTPred-sing').
cycPred('TTPred-sentient').
cycPred('TTPred-role09-script-of').
cycPred('TTPred-alto').
cycPred('TTPred-attr-Parisian').
cycPred('TTPred-event33-of').
cycPred('TTPred-spaced-out').
cycPred('TTPred-attr-Quaker').
cycPred('TTPred-attend-high-school').
cycPred('TTPred-attr-Christian-Scientist').
cycPred('TTPred-attr-Sikh').
cycPred('genlMt').

*/

testE2C:-make,halt.
codesToForms(Codes,[],Codes):-!.
codesToForms(Codes,[List|More],Out):-!,
      codesToForms(Codes,List,M),!,
      codesToForms(M,More,Out),!.

codesToForms(Codes,lowercase,Out):-!,toLowercase(Codes,Out).
codesToForms(Codes,uppercase,Out):-!,toUppercase(Codes,Out).
codesToForms(Codes,cyclist,Out):-!,getSurfaceFromChars(Codes,M,_).
codesToForms(Codes,cyclistvars,Out:V):-!,getSurfaceFromChars(Codes,Out,V).
codesToForms(Codes,cycl,Out):-!,getSurfaceFromChars(Codes,O,V),balanceBinding(O,Out).
codesToForms(Codes,cyclvars,Out:V):-!,getSurfaceFromChars(Codes,O,V),balanceBinding(O,Out).
codesToForms(Codes,words,Out):-!,to_word_list(Codes,Out).
codesToForms(Codes,idioms(D),Out):-!,idioms(D,Codes,Out).
codesToForms(Codes,Pred,Out):-atom(Pred),!,Call=..[Pred,Codes,Out],!,Call.

dirrect_order([start,tomcat]).
%dirrect_order([start,tomcat]):-shell('/opt/tomcat/bin/startup.sh'),fmt([ok,done]).


:- op(500,xfy,&). 
:- op(510,xfy,=>). 
:- op(100,fx,('`')).

	 
:-export((collection/3, fdelete/3)).

% ===============================================================================================
	             	 	
fdelete([],T,[]):-!.

fdelete([Replace|Rest],[H|T],Out):-
	functor(Replace,F,_),memberchk(F,[H|T]),!,
       fdelete(Rest,[H|T],Out),!.

fdelete([Replace|Rest],[H|T],[Replace|Out]):-!,
       fdelete(Rest,[H|T],Out),!.

fdelete([Replace|Rest],F,Out):-
	functor(Replace,F,_),!,%F=FF,
       fdelete(Rest,F,Out),!.

fdelete([Replace|Rest],F,[Replace|Out]):-
       fdelete(Rest,F,Out),!.

%:-ensure_loaded(opencyc_chatterbot_data).


% end_of_file.
% =================================================================
% english2Kif
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% =================================================================
/*

clientEvent(Channel,Agent,english(phrase([learn|Input],Codes),_)):-!,
	    AS = exec_lf(and(equals('?TargetAgent','Self'),equals('?Speaker',Agent),['or'|Ors])),
	    findall(Kif,english2Kif(Input,Kif),Ors),fmt(AS).

clientEvent(Channel,Agent,english(phrase(Input,Codes),_)):-
	    AS = exec_lf(and(equals('?TargetAgent','Self'),equals('?Speaker',Agent),['or'|Ors])),
	    findall(Kif,english2Kif(Input,Kif),Ors),
	    sendEvent(Channel,Agent,(AS)).
*/

english2Kif(Sentence):-english2Kif(Sentence,Kif),fmt(Kif).
english2Obj(Sentence):-english2Obj(Sentence,Kif),fmt(Kif).

english2Kif(Sentence,Kif):-
      convertToWordage(Sentence,Words),
      wordageToKif(Words,Kif).

english2Obj(Sentence,noun_phrase(A,C)):-
      convertToWordage(Sentence,Words),
      phrase(noun_phrase(A,_,C),Words).

english2Kif:-english2Kif('i am happy').

convertToWordage([],['True']):-!.
convertToWordage(Atom,C):-to_word_list(Atom,List),!,
      idioms(chat,List,ListExpanded),!,
      convertToWordage(ListExpanded,C),!.
      %isDebug(fmt('~q<br>',[C])),!.

convertToWordage(Words,C):-is_list(Words),
      removeRepeats(Words,NoRepeats),!,
      convertToWordage(noRepeats(NoRepeats),C),!.

convertToWordage(noRepeats(NoRepeats),Next):-!,
      fdelete(NoRepeats,['Hm','hm','ah','uh','Uh','Um','um'],WordsNoIT),!,
      subst(WordsNoIT,i,'I',Next),!.
%e2c(English,CycLOut)   

removeRepeats(WordsNoIT,WordsNoITO):-
      removeRepeats1(WordsNoIT,M),
      removeRepeats2(M,WordsNoITO),!.
     

% =================================================================
% wordageToKif
% =================================================================
      
wordageToKif(Words,ResultO):- reverse(Words,[Symbol|Rev]),reverse(Rev,Quest),!,
	 wordageToKif(Symbol,Words,Quest,ResultO). %,simplifyLF(Result,ResultO).


wordageToKif(('?'),Words,Quest,query(Kif)) :- phrase(questionmark_sent(Kif),Quest).
wordageToKif(('.'),Words,Quest,assert(Kif)) :- phrase(period_sent(Kif),Quest).
wordageToKif(Symbol,Words,Quest,Kif) :- not(memberchk(Symbol,[('.'),('?')])),phrase(sentence(Kif),Words).
wordageToKif(Symbol,Words,Quest,words(Words)).


% =======================================================
% sentence(CycL, [every,man,that,paints,likes,monet],[]) 
% =======================================================
%sentence(S) --> conjunct(_),!,syntact(S).
%sentence(S) --> interjections(_),!,syntact(S).
questionmark_sent(true(CycL)) --> assertion_nl(CycL).
questionmark_sent(can(CycL)) --> interogative(CycL).
questionmark_sent(yn(CycL)) --> imparative(CycL).

simplifyLF(true(X),X).
simplifyLF(yn(X),X).

period_sent(CycL) --> assertion_nl(CycL).
period_sent(interogative(CycL)) --> interogative(CycL).
period_sent(command(Act)) --> imparative(Act).

simplifyLF(interogative(X),X).
simplifyLF(command(X),X).

sentence(command(Act)) --> imparative(Act).
sentence(assert(CycL)) --> assertion_nl(CycL).
%sentence(query(CycL)) --> interogative(CycL).
     
simplifyLF(interogative(X),X).
simplifyLF(assert(X),assert(X)).

literal([E|F], C, B):-!,append([E|F],B, C).
literal(E, [E|C], C).
literal([], C, C).

      
% =================================================================
% interjections  TODO
% =================================================================
interjections(interject(W)) --> interjection(W).
interjections(interjects(H,T)) --> interjection(H),interjections(T).

interjection(C) --> isPOS('Interjection-SpeechPart',C).

% =================================================================
% imparative  TODO
% =================================================================

% tell me
imparative(CycL) --> verb_phrase(TargetAgent,ImparitiveEvent,CycL),
	 {varnameIdea('?TargetAgent',TargetAgent),varnameIdea('ImparitiveEvent',ImparitiveEvent)}.

% =================================================================
% interogative  TODO
% =================================================================
% How are you
% What have you
% What do you have?
% What do you think?
% How will you
% Could the dog
% could he think of it? 
% are you happy
% * ?
interogative(CycL) --> verb_phrase(TargetAgent,ImparitiveEvent,CycL),
	 {varnameIdea('?TargetAgent',TargetAgent),varnameIdea('QuestionEvent',ImparitiveEvent)}.

% =================================================================
% assertion_nl
% =================================================================
% Now lets say that the input values for the memory NN uses the pattern from the other nodes output
% our naming specialist, Linda Bergstedt
% it is good
% the fubar licks the bowl
% It should be a mix.

% the dog licks the bowl
assertion_nl(CycL) --> noun_phrase(Subj,CycL1,CycL),verb_phrase_after_nouns(Subj,Event,CycL1).

% gen assertion 1
assertion_nl(gen_assert(Call,Result)) --> [S],
	    { 'genFormat'(Predicate,[S|Template],ArgsI,_,_),atom(Predicate),
	    (compound(ArgsI) -> trasfromArgs(ArgsI,Args) ; Args=[1,2]),
	    length(Args,Size),functor(Call,Predicate,Size),
	    placeVars(Blanks,Args,Call)},
	    do_dcg(Template,Blanks,Result).

assertion_nl(gen_assert(Predicate)) --> [S],
	    { 'genFormat'(Predicate,S,_,_,_) }.

% =================================================================
% WHORDS
% =================================================================

% which do
what_do(W,V) --> query_starter(W),isPOS('DoAux',V).

% where 
%query_starter(W)  --> isPOS('WHAdverb',W).
% could / which
query_starter(W)  --> isPOS('Modal',W);isPOS('WHWord',W).

% =======================================================
% Rel Clauses
% =======================================================

% Linda Bergstedt
human_name(([First,Last])) --> capitolWord(First),capitolWord(Last).
% Linda
human_name(Name) --> capitolWord(Name). 

capitolWord(A) --> [A],{atom(A),atom_codes(A,[C|_]),char_type(C,upper)}.

% =======================================================
% Nouns Phrases
% =======================================================
% =======================================================
% TODO
%'properNounSemTrans'('Egyptian-TheWord', 0, 'RegularNounFrame', 'citizens'('Egypt', ':NOUN'), 'GeneralEnglishMt', v(v('Egypt', 'Egyptian-TheWord', 'RegularNounFrame', 'citizens', ':NOUN'), A)).
% =======================================================


% TODO
% =======================================================
%'nlPhraseTypeForTemplateCategory'('PhraseFn-Bar1'('Verb'), 'PerfectiveVBarTemplate', 'AuxVerbTemplateMt', v(v('PerfectiveVBarTemplate', 'PhraseFn-Bar1', 'Verb'), A)).


% TODO
% =======================================================
%'nounSemTrans'('Aspect-TheWord', 0, 'PPCompFrameFn'('TransitivePPCompFrame', 'Of-TheWord'), 'hasAttributes'(':OBLIQUE-OBJECT', A), 'GeneralEnglishMt', v(v('Aspect-TheWord', 'Of-TheWord', 'PPCompFrameFn', 'TransitivePPCompFrame', 'hasAttributes', ':OBLIQUE-OBJECT'), ['?ATTR'=A|B])).

% TODO
% a man hapilly maried
% a man who knows
% a man of his word that walks
% a man of his word
% the cost of what the product is

% a man that walks
%noun_phrase(Subj,In,also(A,LL)) --> [A|LL],{cont([A|LL])}.
noun_phrase(List, In, Out, [M,N,O|More], F) :-
      (nth1(Loc,[M,N,O],',');nth1(Loc,[M,N,O],'and')),
      noun_phrase_list(Loc,List, In, Out, [M,N,O|More], F).

noun_phrase_list(Loc,[H],In,Out) --> subject(H,In,Out).
noun_phrase_list(Loc,[H|T],In,Out) --> ([and];[(',')];[]),
      subject(H,In,Mid),([and];[(',')];[]),
      noun_phrase_list(Loc,T,Mid,Out),{!}. 

noun_phrase(S,A,B)-->subject(S,A,B).

%subject(_,_,_) --> isPOS('Verb',_),{!,fail},_.

% a man that walks
subject(List, In, Out, [M,N,O|More], F) :-
      nonvar(More),
      (nth1(Loc,[M,N,O],'who');nth1(Loc,[M,N,O],'that')),
      noun_phrase_rel_clause(Loc,List, In, Out, [M,N,O|More], F).

%rel_clause(Subj,HowDoes) -->isPOS('Complementizer',Modal,String),verb_phrase(Subj,Event,HowDoes),{varnameIdea(String,Event)}.
noun_phrase_rel_clause(Loc,Subj,In,rel_clause(In,Out)) --> 
	 subject(Subj,HowDoes,Out), 
	 (isPOS('Complementizer',Modal,String);[]),
	 verb_phrase(Subj,Event,HowDoes).


% =======================================================
subject_isa(SubjectIsa,Subj,Template,TemplateO) --> subject(Subj,Template,TemplateO).


% =======================================================
% 'Determiner-Indefinite' , 'Determiner-Definite'
%'determinerAgreement'('A-Half-Dozen-MWW', 'plural-Generic',

% all dog
subject(Subj,In,'forAll'(Subj,AttribIsa)) --> 
    ([every];[all];[forall];[each];[for,all]),
    det_object(Subj,In,AttribIsa).


% the happy dog
%subject(X,In,referant(X,isa(X,Subj),AttribIsa)) --> [the],      det_object(Subj,In,AttribIsa),{varnameIdea('Thing',X),!}.
subject(Subj,In,AttribIsa) --> [the],det_object(Subj,In,AttribIsa).

% a dog
subject(Subj,In,'thereExists'(Subj,AttribIsa)) --> 
    ([a];[an];[some];[there,is];[there,are];[there,exists]),
    det_object(Subj,In,AttribIsa).

% your rainbow
subject(X,A,and(ownedBy(X,Agent),isa(X,Thing),B)) --> possessive(Agent),noun_phrase(Thing,A,B),{varnameIdea('Thing',X),!}.

% he
subject(PN,CycL,CycL) --> pronoun(PN),{!}.

% Joe blow
subject(named(Name),CycL,CycL) --> human_name(Name),{!}.

% a man

% dog
subject(Subj,In,AttribIsa) --> det_object(Subj,In,AttribIsa).

% =======================================================
%'multiWordSemTrans'([equilateral], 'Shaped-TheWord', 'Adjective', 'RegularAdjFrame', 'shapeOfObject'(':NOUN', 'EquilateralShaped'), 'EnglishMt', v(v('Adjective', 'EquilateralShaped', 'RegularAdjFrame', 'Shaped-TheWord', 'shapeOfObject', ':NOUN', equilateral), A)).
det_object(Subj,In,and(Extras,Out)) --> [S],
  {'multiWordSemTrans'([S|String],CycWord, 'Adjective', NextFrame,Template,_,_)},
     String,isCycWord(CycWord), frame_template(NextFrame,Subj,Result,Extras),
    {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

det_object(Subj,In,Out) --> 
	 isPOS('Adjective',CycAdj),
	 det_object_adj(CycAdj,Subj,In,Out).

% =======================================================
%'adjSemTrans-Restricted'('Wooden-TheWord', 0, 'RegularAdjFrame', 'Artifact', 'isa'(':NOUN', 'Wood'), 'GeneralEnglishMt', v(v('Artifact', 'RegularAdjFrame', 'Wood', 'Wooden-TheWord', 'isa', ':NOUN'), A)).
det_object_adj(CycAdj,Subj,In,and(Extras,Out)) --> 
	 {'adjSemTrans-Restricted'(CycAdj, _, FrameType, NounIsa, Template,_,_)},
	subject_isa(NounIsa,Subj,Template,TemplateO),
      {apply_frame(TemplateO,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'adjSemTrans'('Tame-TheWord', 0, 'RegularAdjFrame', 'isa'(':NOUN', 'TameAnimal'), 'GeneralEnglishMt', v(v('RegularAdjFrame', 'Tame-TheWord', 'TameAnimal', 'isa', ':NOUN'), A)).
%'adjSemTransTemplate'('ColorTingeAttribute', 'RegularAdjFrame', 'objectHasColorTinge'(':NOUN', ':DENOT'), 'GeneralEnglishMt', v(v('ColorTingeAttribute', 'RegularAdjFrame', 'objectHasColorTinge', ':DENOT', ':NOUN'), A)).
det_object_adj(CycAdj,Subj,In,and(Extras,Out)) --> 
	 {'adjSemTrans'(CycAdj, _, FrameType, Template,_,_);
	 ('adjSemTransTemplate'(AdjIsa, FrameType, Template,_,_),cycQueryIsa(CycAdj,AdjIsa))},
	frame_template(NextFrame,Subj,Result,Extras),
      {apply_frame(Template,Subj,Event,Obj,Result,Out)}.


det_object_adj(CycAdj,Subj,In,and(Isa,hasTrait(Subj,CycL))) -->
       det_object(Subj,In,Isa),{cvtWordPosCycL(CycAdj,'Adjective',CycL),!}.

% =======================================================
det_object(Subj,In,Isa) --> object(Subj,In,Isa).

% =======================================================
det_object(PN,CycL,CycL) --> proper_object(PN).

%'multiWordSemTrans'([intended, recipient, of], 'Communicate-TheWord', 'SimpleNoun', 'RegularNounFrame', 'communicationTarget'(A, ':NOUN'), 'EnglishMt', v(v('Communicate-TheWord', 'RegularNounFrame', 'SimpleNoun', 'communicationTarget', ':NOUN', intended, of, recipient), ['?X'=A|B])).
object(Subj,In,'and'(In,Out)) --> [S],
   {'multiWordSemTrans'([S|String],CycWord,POS, NextFrame,Template,_,_),POS \= 'Adjective'},
     String,isCycWord(CycWord), frame_template(NextFrame,Subj,Result,Extras),
    {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'nounPrep'('Address-TheWord', 'Of-TheWord', ['pointOfContactInfo', ':OBLIQUE-OBJECT', 'ContactLocation', 'addressText', ':NOUN']).
%'nounPrep'('Retail-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
%'nounPrep'('Market-TheWord', 'Of-TheWord', ['sellsProductType', ':NOUN', ':OBLIQUE-OBJECT']).
%'nounPrep'('Start-TheWord', 'Of-TheWord', ['startingPoint', ':OBLIQUE-OBJECT', ':NOUN']).
%'nounPrep'('City-TheWord', 'Of-TheWord', 'equals'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('City-TheWord', 'Of-TheWord', 'equals', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'nounPrep'('Victim-TheWord', 'Of-TheWord', 'victim'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('Of-TheWord', 'Victim-TheWord', 'victim', ':NOUN', ':OBLIQUE-OBJECT'), A)).
object(Subject,In,and(CycL,Out)) --> isPOS('Noun',CycWord), 
      {'nounPrep'(CycWord,CycWordPrep, Template,_,_)},
      isCycWord(CycWordPrep),subject(Result,In,CycL),
    {apply_frame(Template,Subject,Event,Object,Result,Out)}.

% the happy dog
object(Subj,CycL,and(CycL,Isa)) --> colection(Subj,Isa).

%% of what the product is
%'team-mate'
% happy dog
% kickoff call
colection(Subj,isaMember(Subj,W)) --> [W],{atom(W),atom_concat('',_,W),!}.
colection(Subj,isaMember(Subj,CycL)) --> isPOS('Noun',CycWord,String),
	    {cvtWordPosCycL(CycWord,'Noun',CycL),varnameIdea(String,Subj),!}.

wordPosCycL(CycWord,POS,CycL):-
      'denotation'(CycWord, POS, _, CycL,_,_);'denotationRelatedTo'(CycWord, POS, _, CycL,_,_).
wordPosCycL(CycWord,POS,CycL):- 'genls'(Child,POS,_,_),wordPosCycL(CycWord,Child,CycL).
wordPosCycL(CycWord,_,CycL):-
      'denotation'(CycWord, POS, _, CycL,_,_);'denotationRelatedTo'(CycWord, POS, _, CycL,_,_).
      

cvtWordPosCycL(CycWord,POS,CycL):-wordPosCycL(CycWord,POS,CycL),!.
cvtWordPosCycL(CycWord,POS,CycL):-CycL=..[POS,CycWord],!.
% ==========================================================
% String to CYC - POS
% ==========================================================
proper_object(CycL) --> [S,S2],{poStr(CycL,[S,S2|String])},String.
proper_object(CycL) --> [String],{poStr(CycL,String)}.

poStr(CycL,String):-
      'initialismString'(CycL,String,_,_);
      'abbreviationString-PN'(CycL, String,_,_);
      'preferredNameString'(CycL, String,_,_);
      'countryName-LongForm'(CycL, String,_,_);
      'countryName-ShortForm'(CycL, String,_,_);
      'acronymString'(CycL,String, _,_);
      'scientificName'(CycL,String, _,_);
      'termStrings'(CycL,String, _,_);
      'termStrings-GuessedFromName'(CycL,String, _,_);
      'prettyName'(CycL,String, _,_);
      'nameString'(CycL,String, _,_);
      'nicknames'(CycL,String, _,_);
      'preferredTermStrings'(CycL,String, _,_).

proper_object(CycL) --> {'genPhrase'(CycL, POS,Form, String,_,_)},String.

proper_object(CycL) --> {'termPOS-Strings'(CycL,POS,String,_,_)},literal(String).

proper_object(CycL) -->
	 isPOS('NLWordForm',CycWord),
	 {'compoundString'(CycWord,String, POS,CycL,_,_)},String.

proper_object(CycL) --> [S],{'multiWordString'([S|String], CycWord,POS,CycL,_,_)},
	    String,isCycWord(CycWord).

proper_object(CycL) --> [S],{'headMedialString'([S|String], CycWord,POS,Right,CycL,_,_)},
	    String,isCycWord(CycWord),Right.


proper_object(multFn(Multiply,Collection)) --> [String],
	 {'unitOfMeasurePrefixString'(Multiply, Prefix,_,_),
	 atom_concat(Prefix,Rest,String),!,phrase(collection(Collection),[Rest])}.

proper_object(CycL) -->  [String],
      {concat_atom([Left,Right],'-',String),
      'hyphenString'([Left], RightWord, POS,CycL, _,_),
	 phrase(isCycWord(RightWord),[Rest])}.

%possessive(Agent)-->pronoun(Agent),isCycWord('Have-Contracted'),{!}.
possessive(Agent)-->pronoun(Agent),isCycWord('Be-Contracted').
possessive(Agent)-->isPOS('PossessivePronoun-Pre',Agent).
possessive(Agent)-->isPOS('PossessivePronoun-Post',Agent).
possessive(Inters)-->human_name(Inters),['\'',s].
%possessive(Agent)-->pronoun(Agent).

pronoun('?Speaker') --> isCycWord('I-TheWord');isCycWord('Me-TheWord').
pronoun('?TargetAgent') --> isCycWord('You-TheWord').
pronoun('?Where') --> isCycWord('Where-TheWord').
pronoun('?How') --> isCycWord('How-TheWord').
pronoun('?IT') --> isCycWord('It-TheWord').
pronoun('?He') --> isCycWord('He-TheWord').
pronoun('?She') --> isCycWord('She-TheWord').

pronoun(X) --> wh_pronoun(X).
pronoun(ref(CycWord)) --> isPOS('Pronoun', CycWord).

wh_pronoun('?Agent') --> [who].
wh_pronoun('?What') --> [what].

% ==========================================================
% POS DCG
% ==========================================================
isForm(POS,CycWord,Form) --> [String],{hotrace(meetsForm(Form,String,POS,CycWord)),!}.
isForm(POS,CycWord,Form) --> [S,W|String],{hotrace(meetsForm(Form,[S,W|String],POS,CycWord))},String.

meetsForm(Form,String,POS,CycWord):- (var(String);var(Form)),throw(meetsForm(String,POS,CycWord)).
meetsForm(Form,String,POS,CycWord):-stringToWordForm(String,CycWord,Form),cycWordPosForm(POS,CycWord,Form).

meetsForm(Form,String,POS,CycWord):-'genlPreds'(Child,Form,_,_),meetsForm(Child,String,POS,CycWord).


isPOS(POS,CycWord) --> isPOS(POS,CycWord,String).

isPOS(POS,CycWord,String) --> [String],{hotrace(meetsPos(String,POS,CycWord)),!}.
isPOS(POS,CycWord,String) --> [S,W],{hotrace(meetsPos([S,W|String],POS,CycWord))},String.

cycWordPosForm(POS,CycWord,Form):-
	 'preferredGenUnit'(CycL,POS, CycWord,_,_);
	 'posBaseForms'(CycWord,POS,_,_);
	 'posForms'(CycWord,POS,_,_);
	 'denotation'(CycWord,POS, Arg, CycL, _,_);
	 'speechPartPreds'(POS, Form, _,_).

nonground(V):-notrace(not(ground(V))).

meetsPos(String,POS,CycWord):- (nonground(String);nonground(POS)),throw(meetsPos(String,POS,CycWord)).
meetsPos([String],POS,CycWord):-!,meetsPos(String,POS,CycWord).
meetsPos(String,POS,CycWord):-'partOfSpeech'(CycWord,POS, String,_,_).
meetsPos(String,POS,CycWord):-stringToWordForm(String,CycWord,Form),cycWordPosForm(POS,CycWord,Form).
meetsPos(String,POS,CycWord):-moomt:'genls'(Child,POS,_,_),meetsPos(String,Child,CycWord).
meetsPos(String,'Verb',CycWord):-atom(String),meetsPosVerb(String,CycWord),!.

%meetsPos(String,'Noun',CycWord):-atom(String),meetsPosNoun(String,CycWord).
meetsPosVerb(String,CycWord):-atom_concat(S,'ed',String),meetsPos(S,'Verb',CycWord).
meetsPosVerb(String,CycWord):-atom_concat(S,'s',String),meetsPos(S,'Verb',CycWord).

meetsPos(String,POS,CycWord):-
	 memberchk(POS,['Noun','Adjective','Verb','Adverb']),
	 'wnS'(CycWord,_, String,POS, _,_,_,_),!.
meetsPos(String,'Adjective',CycWord):-'wnS'(CycWord,_, String, 'AdjectiveSatellite', _,_,_,_).

meetsPos(String,POS,CycWord):- atom(String),
	    'prefixString'(CycWord, Prefix, _,_),
            atom(Prefix),
	    atom_concat(Prefix,_,String),
	    'derivationalAffixBasePOS'(CycWord,POS,_,_).


% Wordnet
wordToWNPOS(CycWord,WNWord,POS):-'denotationPlaceholder'(CycWord,POS, _, WNWord, _,_).
%'synonymousExternalConcept'('AbandoningSomething', 'WordNet-1995Version', 'V01269572', 'WordNetMappingMt', v(v('AbandoningSomething', 'WordNet-1995Version', 'V01269572'), A)).


% ==========================================================
% String / Word
% ==========================================================


% cycWordForISA
cycWordForISA(CycWord,EventIsa):-fail.  


% peace atal beh - 695-1297
%isCycWord(CycWord) --> {var(CycWord),!,trace}.
isCycWord(CycWord) --> {hotrace(cycWordFromString(CycWord,String))},literal(String).

cycWordFromString(CycWord,String):-'baseForm'(CycWord,String,_,_).
cycWordFromString(CycWord,String):-'partOfSpeech'(CycWord,POS, String,_,_).
cycWordFromString(CycWord,String):- stringToWordForm(String,CycWord,Form).


% ==========================================================
% stringToWordForm(String,CycWord,Form)
% ==========================================================
%stringToWordForm([W],CycWord,Form):-atom(W),!,stringToWordForm(W,CycWord,Form).
stringToWordForm(String,CycWord,Form):-'abbreviationForLexicalWord'(CycWord,Form, String,_,_).

% Nouns
stringToWordForm(String,CycWord,'singular'):-'singular'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'plural'):-'plural'(CycWord, String, _,_).
%stringToWordForm(String,CycWord,'nonPlural-Generic'):-'nonPlural-Generic'(CycWord, String, _,_).

stringToWordForm(String,CycWord,'agentive-Mass'):-'agentive-Mass'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'agentive-Pl'):-'agentive-Pl'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'agentive-Sg'):-'agentive-Sg'(CycWord, String, _,_).
%stringToWordForm(String,CycWord,'singular-Feminine'):-'singular-Feminine'(CycWord, String, _,_).
%stringToWordForm(String,CycWord,'singular-Masculine'):-'singular-Masculine'(CycWord, String, _,_).
%stringToWordForm(String,CycWord,'singular-Neuter'):-'singular-Neuter'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'massNumber'):-'massNumber'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'pnSingular'):-'pnSingular'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'pnMassNumber'):-'pnMassNumber'(CycWord, String, _,_).

% Adjectives
stringToWordForm(String,CycWord,'regularDegree'):-'regularDegree'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'comparativeDegree'):-'comparativeDegree'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'superlativeDegree'):-'superlativeDegree'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'nonGradableAdjectiveForm'):-'nonGradableAdjectiveForm'(CycWord, String, _,_).

% Adverbs
stringToWordForm(String,CycWord,'regularAdverb'):-'regularAdverb'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'superlativeAdverb'):-'superlativeAdverb'(CycWord, String, _,_).

% Verbs
stringToWordForm(String,CycWord,'infinitive'):-'infinitive'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'perfect'):-'perfect'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'presentParticiple'):-'presentParticiple'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'pastTense-Universal'):-'pastTense-Universal'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'presentTense-Universal'):-'presentTense-Universal'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'firstPersonSg-Present'):-'firstPersonSg-Present'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'secondPersonSg-Present'):-'secondPersonSg-Present'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'nonThirdSg-Present'):-'nonThirdSg-Present'(CycWord, String, _,_).
stringToWordForm(String,CycWord,'thirdPersonSg-Present'):-'thirdPersonSg-Present'(CycWord, String, _,_).
stringToWordForm(String,CycWord,Form):-atom(String),!,stringAtomToWordForm(String,CycWord,Form).
%stringToWordForm(String,CycWord,Form):-nonvar(String),stringListToWordForm(String,CycWord,Form).
stringAtomToWordForm(String,CycWord,Form):-
	    'regularSuffix'(Before, Form, Suffix, _,_),
	    atom_concat(NewString,Suffix,String),
	    stringToWordForm(NewString,CycWord,Before).
%'suffixString'('Y_AdjectiveProducing-TheSuffix', y, 'GeneralEnglishMt', v(v('Y_AdjectiveProducing-TheSuffix', y), A)).

%'variantOfSuffix'('Able-TheSuffix', ible, 'GeneralEnglishMt', v(v('Able-TheSuffix', ible), A)).
%'variantOfSuffix'('Al_AdjectiveProducing-TheSuffix', ual, 'GeneralEnglishMt', v(v('Al_AdjectiveProducing-TheSuffix', ual), A)).

%'compoundStringDenotesArgInReln'('Actor-TheWord', [remaining, afterwards], 'CountNoun', 'postActors', 2, 'GeneralEnglishMt', v(v('Actor-TheWord', 'CountNoun', 'postActors', afterwards, remaining), A)).
%'multiWordStringDenotesArgInReln'([unchanged], 'Actor-TheWord', 'SimpleNoun', 'unchangedActors', 2, 'GeneralEnglishMt', v(v('Actor-TheWord', 'SimpleNoun', 'unchangedActors', unchanged), A)).
	 

% ==========================================================
% String to WordsList - Form / POS
% ==========================================================
stringToWordsForm(String,[CycWord|Words],Form):- 
         'abbreviationForCompoundString'(CycWord,WordList,Form,String,_,_),
	 stringToWords(WordList,Words).

stringToWordsPOS(String,CycWords,POS):-
      'abbreviationForMultiWordString'(List,Word,POS,String,_,_),
      stringToWordsForm(List,Words,_),
      append(Words,[Word],CycWords).


varnameIdea(X,Y):-varnameIdea2(X,Y),!.
varnameIdea2([String|_],Subj):-!,varnameIdea2(String,Subj).
varnameIdea2('?TargetAgent','?TargetAgent').
varnameIdea2('TargetAgent','?TargetAgent').
varnameIdea2('?Speaker','?Speaker').
varnameIdea2(String,Subj):-atom(String),var(Subj),atom_concat('?',String,Sym),gensym(Sym,Subj).
varnameIdea2(String,Subj).

% =======================================================
% Conjunctions
% =======================================================
% [that];[who]

conjunct --> conjunct(X).
conjunct(C) --> isPOS('CoordinatingConjunction',C).
conjunct(C)--> [and];[also].

disj_word --> [or];[not];[but].


modal_phrase(CycAuxVerb,Subj,Event,Out)-->aux_phrase(CycAuxVerb,Subj,Event,Out).
% =======================================================
% Aux Phrases
% =======================================================

% is good
aux_phrase('Be-TheWord',Subj,Event,and(CycL,hasTrait(Subj,AdjCycL))) -->
	 isPOS('Adjective',CycAdj),
      aux_phrase('Be-TheWord',Subj,Event,CycL),
      {cvtWordPosCycL(CycAdj,'Adjective',AdjCycL)}.


% can
aux_phrase('Can-TheModal',Subj,Event,'can'(Subj,CycL)) --> 
	  verb_phrase(Subj,Event,CycL).

% do/is/be/does
aux_phrase(CycAuxVerb,Subj,Event,aux_isa_for(Subj,Event,Action)) --> [],
	 {cvtWordPosCycL(CycAuxVerb,'Verb',Action)}.

% do <X>
aux_phrase('Do-TheWord',Subj,Event,(CycL)) --> 
	 verb_phrase(Subj,Event,CycL) .

% =======================================================
%'verbPrep-Passive'('Make-TheWord', 'Of-TheWord', 'mainConstituent'(':OBJECT', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Make-TheWord', 'Of-TheWord', 'mainConstituent', ':OBJECT', ':OBLIQUE-OBJECT'), A)).
aux_phrase(CycWord,Subj,Event,CycLO) --> 
      {'verbPrep-Passive'(CycWord, CycWord2, Template,_,_)},
       isCycWord(CycWord2),subject(Result,Out,CycLO),
      {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'prepSemTrans'('Above-TheWord', 0, 'Post-NounPhraseModifyingFrame', 'above-Generally'(':NOUN', ':OBJECT'), 'GeneralEnglishMt', v(v('Above-TheWord', 'Post-NounPhraseModifyingFrame', 'above-Generally', ':NOUN', ':OBJECT'), A)).
aux_phrase(CycAuxWord,Subj,Event,Out) -->
      {'prepSemTrans'(CycWordPrep, _, NextFrame, Template,_,_)},
      isCycWord(CycWordPrep),subject(Obj,Out,CycLO),
    {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
% preopistional_phrase
% =======================================================
preopistional_phrase(Oblique,CycWord,CycL) -->
	 isPOS('Preposition',CycWord), 
	 noun_phrase(Oblique,Prep,CycL),{varnameIdea('Prep',Prep)}.
      
% =======================================================
% verb_phrase
% =======================================================

% no verb
verb_phrase(_,_,_) --> isPOS('Determiner',CycWord),{!,fail}.

verb_phrase_after_nouns(Subj,Event,exists(Subj)) --> [].

verb_phrase_after_nouns(Subj,Event,CycL) --> 
	 verb_phrase(Subj,Event,CycL).

% One verb
%verb_phrase(Subj,Event,do(Subj,Verb)) --> [Verb].

% known phrase
verb_phrase(Subj,Event,known_phrase(CycL)) --> 
	    isPOS('Verb',CycVerb),
	    verb_phrase_known(CycVerb,Subj,Event,CycL).

% gen phrase 2
verb_phrase(Subj,Event,gen_phrase2(Call,Result)) --> [S,N],
	    { 'genFormat'(Predicate,['~a',S,N|Template],ArgsI,_,_),atom(Predicate),
	    (compound(ArgsI) -> trasfromArgs(ArgsI,Args) ; Args=[1,2]),
	    length(Args,Size),functor(Call,Predicate,Size),
	    placeVars([Subj|Blanks],Args,Call)},
	    do_dcg(Template,Blanks,Result).
	    
% modal phrase
verb_phrase(Subj,Event,modal(CycL)) --> 
	    isPOS('Modal',CycWord,String),
	    modal_phrase(CycWord,Subj,Event,CycL),{varnameIdea(String,Event)}.

% aux phrase
verb_phrase(Subj,Event,(CycL)) --> 
	    isPOS('AuxVerb',CycWord,String),
	    aux_phrase(CycWord,Subj,Event,CycL),{varnameIdea(String,Event)}.

% adverbal phrase
verb_phrase(Subj,Event,'and_adverbal'(Event,AdvCycL,CycL))  --> 
	    isPOS('Adverb',CycWord),
	    verb_phrase(Subj,Event,CycL),
	    {cvtWordPosCycL(CycWord,'Adverb',AdvCycL)}.

% unknown phrase has arity CycL + object %TODO rename subject/3 to noun_phrase/3
verb_phrase(Subj,Event,and_concat(CycL)) --> [Verb],
	 {atom(Verb),((atom_concat('',Verb,Predicate),holds_t('arity',Predicate,N));(holds_t('arity',Verb,N),Predicate=Verb)),!},
	 verb_phrase_arity(N,Predicate,Subj,Event,CycL).

% :-index(verb_phrase_arity(0,0,0,0,0,0,0)).
%TODO rename subject/3 to noun_phrase/3
verb_phrase_arity(2,Predicate,Subj,Event,CycL) --> 
	       best_subject(Obj,ACT,Mid),
	       colect_noun_list(List,Mid,CycL),
	       {apply_act(Predicate,Subj,[Obj|List],ACT)}.
%and
verb_phrase_arity(3,Predicate,Subj,Event,CycL) --> 
	 best_subject(Obj,Event,Mid),
	 best_subject_constituant(RES,Event,Mid,CycL).
	{ACT=..[Predicate,Subj,Obj,RES]}.

colect_noun_list([],In,In) --> [].
colect_noun_list([H|T],In,Out) --> ([(',')];[and];[]),
      best_subject(H,In,Mid),
      colect_noun_list(T,Mid,Out).

verb_phrase(Subj,Event,(CycL)) --> 
	 isPOS('Verb',CycVerb,String),
%	 best_subject(Obj,true,CycL),
 %        best_subject_constituant(Target,Event,CycL,CycLO),
	 {cvtWordPosCycL(CycVerb,'Verb',Verb),
	 (atom(Verb),(atom_concat('',Verb,Predicate),holds_t('arity',Predicate,N));(holds_t('arity',Verb,N),Predicate=Verb)),!},
	  verb_phrase_arity(N,Predicate,Subj,Event,CycL),{varnameIdea(String,Event)}.
	 
% gen phrase 1
verb_phrase(Subj,Event,gen_phrase1(Call,Result)) --> [S],
	    {S\=is, 'genFormat'(Predicate,['~a',S|Template],ArgsI,_,_),
	    (compound(ArgsI) -> trasfromArgs(ArgsI,Args) ; Args=[1,2]),
	    length(Args,Size),functor(Call,Predicate,Size),atom(Predicate),
	    placeVars([Subj|Blanks],Args,Call)},
	    do_dcg(Template,Blanks,Result).



% unkown phrase	+ object %TODO rename subject/3 to noun_phrase/3
verb_phrase(Subj,Event,and(isaAction(Event,Action),'doneBy'(Event,Subj),'constituentInSituation'(Event,Obj),CycLO)) --> 
	 isPOS('Verb',CycVerb,String),
	 best_subject(Obj,true,CycL),
	 best_subject_constituant(Target,Event,CycL,CycLO),
	 {cvtWordPosCycL(CycVerb,'Verb',Action),varnameIdea(String,Event)}.
														  
% unkown phrase + text
best_subject(Obj,Event,CycL) --> isPOS('Preposition',_),{!},best_subject(Obj,Event,CycL).
best_subject(Obj,Event,CycL) --> noun_phrase(Obj,Event,CycL),{!}.
best_subject(Obj,CycL,CycL) --> rest_of(Obj).

best_subject_constituant(RES,Event,CycL,CycL) --> [].
best_subject_constituant(Target,Event,CycL,and(CycL,CycLO,'eventOccursAt'(Event,Target))) --> 
	 best_subject(Target,Event,CycLO).
   
%rest_of(txt([A|C])) --> [A|C].
rest_of(thingFor(Rest), [A|Rest], []):-hotrace(meetsPos(A,'Determiner',CycWord)),!.
rest_of(thingFor(Rest), Rest, []):-Rest=[_|_].


apply_act(Predicate,Subj,Obj,ACT) :- \+ ';'(is_list(Subj),is_list(Obj)),!,ACT=..[Predicate,Subj,Obj].

apply_act(Predicate,Subj,[Obj],ACT):-!,ACT=..[Predicate,Subj,Obj].
apply_act(Predicate,Subj,[Obj|List],each(ACT,MORE)):-
      ACT=..[Predicate,Subj,Obj],apply_act(Predicate,Subj,List,MORE),!.

apply_act(Predicate,[Obj],Subj,ACT):-!,ACT=..[Predicate,Obj,Subj].
apply_act(Predicate,[Obj|List],Subj,each(ACT,MORE)):-
      ACT=..[Predicate,Obj,Subj],apply_act(Predicate,List,Subj,MORE),!.

% =======================================================
% GENFORMAT Verbs TODO
% =======================================================

do_dcg([],_,nil_true) --> {!},[].
do_dcg(['~a'|Template],[Subj|Blanks],(Result)) -->{!},
	    noun_phrase(Subj,More,Result),
      do_dcg(Template,Blanks,More).
do_dcg(Template,Blanks,end_true) --> Template,{!}.
do_dcg([Word|Template],[Subj|Blanks],(Result)) --> [Word],
      {append(Find,['~a'|More],Template),!},
      Find,noun_phrase(Subj,CycL,Result),
      do_dcg(More,Blanks,CycL).

/*
genFormatVerb2(Term,String,More,Subj,CycLO,noun_phrase(Object,[Predicate,Subj,Object],CycLO)):-
      append(String,['~a'],More),!.
genFormatVerb2([2,1],Predicate,String,More,Subj,CycLO,noun_phrase(Object,[Predicate,Object,Subj],CycLO)):-
      append(String,['~a'],More),!.

genFormatVerb2(Call,[Subj|Blanks],String,More,CycLO,ToDO):-!.
*/

%trasfromArgs(Args,List).
trasfromArgs('NIL',[1,2,3,4,5,6]):-!.
trasfromArgs([H],[HH]):-trasfromArg(H,HH),!.
trasfromArgs([H|T],[HH|TT]):-trasfromArg(H,HH),trasfromArgs(T,TT),!.

trasfromArg([[]|_],_).
trasfromArg([H|_],H).
trasfromArg(H,H).

placeVars([Subj],[N],Call):-integer(N),arg(N,Call,Subj),!.
placeVars([Subj|Blanks],[N|More],Call):-integer(N),arg(N,Call,Subj),placeVars(Blanks,More,Call),!.
      

      	 

%'genTemplate'('many-GenQuantRelnToType', 'TermParaphraseFn'([':ARG1', 'BestDetNbarFn'('TermParaphraseFn'('Many-NLAttr'), 'TermParaphraseFn-Constrained'('plural-Generic', ':ARG2')), 'ConditionalPhraseFn'('equals'(':ARG3', 'Thing'), 'BestNLPhraseOfStringFn'(something), 'BestDetNbarFn'('TermParaphraseFn'('BareForm-NLAttr'), 'TermParaphraseFn-Constrained'('nonSingular-Generic', ':ARG3')))]), 'EnglishParaphraseMt', v(v('BareForm-NLAttr', 'BestDetNbarFn', 'BestNLPhraseOfStringFn', 'ConditionalPhraseFn', 'Many-NLAttr', 'TermParaphraseFn', 'TermParaphraseFn-Constrained', 'Thing', 'equals', 'many-GenQuantRelnToType', 'nonSingular-Generic', 'plural-Generic', ':ARG1', ':ARG2', ':ARG3', something), A)).
%'genTemplate'('many-GenQuant', 'TermParaphraseFn'('elementOf'('BestDetNbarFn'('TermParaphraseFn'('Many-NLAttr'), 'TermParaphraseFn-Constrained'('nonSingular-Generic', ':ARG1')), ':ARG2')), 'EnglishParaphraseMt', v(v('BestDetNbarFn', 'Many-NLAttr', 'TermParaphraseFn', 'TermParaphraseFn-Constrained', 'elementOf', 'many-GenQuant', 'nonSingular-Generic', ':ARG1', ':ARG2'), A)).
%'genTemplate'('markCreated', 'ConcatenatePhrasesFn'('TermParaphraseFn-NP'(':ARG2'), 'BestHeadVerbForInitialSubjectFn'('Be-TheWord'), 'BestNLPhraseOfStringFn'([the, mark, created, by]), 'TermParaphraseFn-NP'(':ARG2')), _,_)

% =======================================================
% Intrans phrase                                                                    
verb_phrase_known(CycWord,Subj,Event,CycLO) --> 
	 [],{cvtWordPosCycL(CycWord,'Verb',CycL),
	 (('arg2Isa'(CycL,Type,_,_),Rel=..[CycL,Subj,Obj],
	 CycLO = and_iv(isa(Obj,Type),Rel) );
	 CycLO=and_iv('bodilyDoer'(Subj,Event),event_isa(Event,CycL))),varnameIdea('Intrans',Event),varnameIdea('Thing',Obj)}.

% TODO
%'agentiveNounSemTrans'('Assist-TheWord', 0, 'RegularNounFrame', 'assistingAgent'(A, ':NOUN'), 'GeneralEnglishMt', v(v('Assist-TheWord', 'RegularNounFrame', 'assistingAgent', ':NOUN'), ['?X'=A|B])).
%'agentiveNounSemTrans'('Emit-TheWord', 0, 'RegularNounFrame', ['emitter', '?X', ':NOUN']).	    
% =======================================================
%'lightVerb-TransitiveSemTrans'('Take-TheWord', 'DrugProduct', 'and'('isa'(':ACTION', 'Ingesting'), 'performedBy'(':ACTION', ':SUBJECT'), 'primaryObjectMoving'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('DrugProduct', 'Ingesting', 'Take-TheWord', 'and', 'isa', 'performedBy', 'primaryObjectMoving', ':ACTION', ':OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'lightVerb-TransitiveSemTrans'(Out)) -->
	{'lightVerb-TransitiveSemTrans'(CycWord,ObjectIsa, Template,_,_)},
	subject_isa(ObjectIsa,Object,Template,TemplateO),
     {apply_frame(TemplateO,Subj,Event,Object,Result,Out)}.

% =======================================================
%'prepReln-Action'('LosingUserRights', 'Agent', 'From-TheWord', 'fromPossessor'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Agent', 'From-TheWord', 'LosingUserRights', 'fromPossessor', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('MovementEvent', 'PartiallyTangible', 'From-TheWord', 'fromLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('From-TheWord', 'MovementEvent', 'PartiallyTangible', 'fromLocation', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('Movement-TranslationEvent', 'SomethingExisting', 'On-TheWord', 'toLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Movement-TranslationEvent', 'On-TheWord', 'SomethingExisting', 'toLocation', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('Stealing-Generic', 'Agent', 'From-TheWord', 'victim'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Agent', 'From-TheWord', 'Stealing-Generic', 'victim', ':ACTION', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Action'('TransportationEvent', 'Conveyance', 'By-TheWord', 'transporter'(':ACTION', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('By-TheWord', 'Conveyance', 'TransportationEvent', 'transporter', ':ACTION', ':OBLIQUE-OBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'prepReln-Action'(CycLO,Out)) -->
      {'prepReln-Action'(EventIsa, SubjIsa, CycWordPrep, Template,_,_),cycQueryIsa(Subj,SubjIsa)},
	verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,EventMid),
      isCycWord(CycWordPrep),subject(Result,EventMid,CycLO),
    {apply_frame(Template,Subject,Event,Object,Result,Out)}.

% =======================================================
%'prepReln-Object'('Action', 'PartiallyTangible', 'Of-TheWord', 'objectActedOn'(':NOUN', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Action', 'Of-TheWord', 'PartiallyTangible', 'objectActedOn', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Object'('AnimalBodyPartType', 'Animal', 'Of-TheWord', 'anatomicalParts'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('Animal', 'AnimalBodyPartType', 'Of-TheWord', 'anatomicalParts', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Object'('Area', 'PartiallyTangible', 'Of-TheWord', 'areaOfObject'(':OBLIQUE-OBJECT', ':NOUN'), 'EnglishMt', v(v('Area', 'Of-TheWord', 'PartiallyTangible', 'areaOfObject', ':NOUN', ':OBLIQUE-OBJECT'), A)).
%'prepReln-Object'('CapitalCityOfRegion', 'IndependentCountry', 'Of-TheWord', 'capitalCity'(':OBLIQUE-OBJECT', ':SUBJECT'), 'EnglishMt', v(v('CapitalCityOfRegion', 'IndependentCountry', 'Of-TheWord', 'capitalCity', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'prepReln-Object'('Communicating', 'Agent', 'By-TheWord', 'senderOfInfo'(':NOUN', ':OBLIQUE-OBJECT'), 'EnglishMt', v(v('Agent', 'By-TheWord', 'Communicating', 'senderOfInfo', ':NOUN', ':OBLIQUE-OBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'prepReln-Object'(Out)) -->
      {'prepReln-Object'(SubjIsa, ObjectIsa, CycWordPrep, Template,_,_),cycQueryIsa(Subj,SubjIsa)},
	subject_isa(ObjectIsa,Object,Template,TemplateO),
      isCycWord(CycWordPrep),subject(Result,TemplateO,CycLO),
    {apply_frame(CycLO,Subject,Event,Object,Result,Out)}.

% =======================================================
%'verbSemTrans'('Depart-TheWord', 0, 'PPCompFrameFn'('TransitivePPCompFrame', 'From-TheWord'), 'and'('isa'(':ACTION', 'LeavingAPlace'), 'fromLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('Depart-TheWord', 'From-TheWord', 'LeavingAPlace', 'PPCompFrameFn', 'TransitivePPCompFrame', 'and', 'doneBy', 'fromLocation', 'isa', ':ACTION', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,verbSemTrans(Out,Extras)) -->
	 {'verbSemTrans'(CycWord, _, NextFrame, Template,_,_)},
	    frame_template(NextFrame,Object,Result,Extras),
	 {apply_frame(Template,Subj,Event,Object,Result,Out)}.

% =======================================================
%'verbPrep-Transitive'('Ablate-TheWord', 'From-TheWord', 'and'('isa'(':ACTION', 'Ablation'), 'objectOfStateChange'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT'), 'objectRemoved'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('Ablate-TheWord', 'Ablation', 'From-TheWord', 'and', 'doneBy', 'isa', 'objectOfStateChange', 'objectRemoved', ':ACTION', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'verbPrep-Transitive'(Out,Extras)) -->
   {'verbPrep-Transitive'(CycWord, CycWord2, Template,_,_)},
	 isCycWord(CycWord2),{!},subject(Result,Out,CycLO),
   {apply_frame(Template,Subj,Event,Obj,Result,Out)}.
   
% =======================================================
%'compoundVerbSemTrans'('Give-TheWord', [off], 'TransitiveNPCompFrame', 'and'('isa'(':ACTION', 'EmittingAnObject'), 'emitter'(':ACTION', ':SUBJECT'), 'objectEmitted'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('EmittingAnObject', 'Give-TheWord', 'TransitiveNPCompFrame', 'and', 'emitter', 'isa', 'objectEmitted', ':ACTION', ':OBJECT', ':SUBJECT', off), A)).
verb_phrase_known(CycWord,Subj,Event,compoundVerbSemTrans(Out,Extras)) -->
   [S],{'compoundVerbSemTrans'(CycWord, [S|String],NextFrame, Template,_,_)},
   String,frame_template(NextFrame,Object,Result,Extras),
   {apply_frame(Template,Subj,Event,Object,Result,Out)}.

% =======================================================
%'compoundSemTrans'('End-TheWord', [during], 'Verb', 'TransitiveNPCompFrame', 'endsDuring'(':SUBJECT', ':OBJECT'), 'EnglishMt', v(v('End-TheWord', 'TransitiveNPCompFrame', 'Verb', 'endsDuring', ':OBJECT', ':SUBJECT', during), A)).
verb_phrase_known(CycWord,Subj,Event,compoundSemTrans(Out,Extras)) -->
    [S],{'compoundSemTrans'(CycWord, [S|String], 'Verb', NextFrame, Template,_,_)},
   String,frame_template(NextFrame,Obj,Result,Extras),
   {apply_frame(Template,Subj,Event,Obj,Result,Out)}.

% =======================================================
%'nonCompositionalVerbSemTrans'('Separate-TheWord', 'Mixture', 'and'('isa'(':ACTION', 'SeparatingAMixture'), 'doneBy'(':ACTION', ':SUBJECT'), 'objectOfStateChange'(':ACTION', ':OBJECT')), 'EnglishMt', v(v('Mixture', 'Separate-TheWord', 'SeparatingAMixture', 'and', 'doneBy', 'isa', 'objectOfStateChange', ':ACTION', ':OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,nonCompositionalVerbSemTrans(Out)) -->
	{'nonCompositionalVerbSemTrans'(CycWord,ObjectIsa, Template,_,_)},
	subject_isa(ObjectIsa,Object,Template,TemplateO),
     {apply_frame(TemplateO,Subj,Event,Object,Result,Out)}.


% =======================================================
%'verbPrep-TransitiveTemplate'('Constructing', 'Out-Of-MWW', 'and'('isa'(':ACTION', ':DENOT'), 'inputs'(':ACTION', ':OBLIQUE-OBJECT'), 'products'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('Constructing', 'Out-Of-MWW', 'and', 'doneBy', 'inputs', 'isa', 'products', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('DistributionEvent', 'To-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'toLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'objectMoving'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('DistributionEvent', 'To-TheWord', 'and', 'doneBy', 'isa', 'objectMoving', 'toLocation', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('Evaluating', 'For-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'performedBy'(':ACTION', ':SUBJECT'), 'evaluee-Direct'(':ACTION', ':OBJECT'), 'purposeInEvent'(':SUBJECT', ':ACTION', 'knowsAbout'(':SUBJECT', ':OBLIQUE-OBJECT'))), 'EnglishMt', v(v('Evaluating', 'For-TheWord', 'and', 'evaluee-Direct', 'isa', 'knowsAbout', 'performedBy', 'purposeInEvent', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('FusionEvent', 'With-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'inputs'(':ACTION', ':OBJECT'), 'inputs'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('FusionEvent', 'With-TheWord', 'and', 'doneBy', 'inputs', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('HoldingAnObject', 'By-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'objectActedOn'(':ACTION', ':OBLIQUE-OBJECT'), 'physicalParts'(':OBJECT', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('By-TheWord', 'HoldingAnObject', 'and', 'doneBy', 'isa', 'objectActedOn', 'physicalParts', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbPrep-TransitiveTemplate'('InformationRemoving', 'From-TheWord', 'and'('isa'(':ACTION', ':DENOT'), 'informationOrigin'(':ACTION', ':OBLIQUE-OBJECT'), 'infoRemoved'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'EnglishMt', v(v('From-TheWord', 'InformationRemoving', 'and', 'doneBy', 'infoRemoved', 'informationOrigin', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,'verbPrep-TransitiveTemplate'(Out,EventMidO)) -->
	 {'verbPrep-TransitiveTemplate'(EventIsa, CycWordPrep, Template,_,_)},
      verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,EventMid),
      isCycWord(CycWordPrep),subject(Result,EventMid,EventMidO),
   {apply_frame(Template,Subj,Event,Object,Result,OutD),subst(OutD,':DENOT',EventIsa,Out)}.
  
% =======================================================
%'verbSemTransTemplate'('InformationRemoving', 'PPCompFrameFn'('DitransitivePPCompFrame', 'From-TheWord'), 'and'('isa'(':ACTION', ':DENOT'), 'informationOrigin'(':ACTION', ':OBLIQUE-OBJECT'), 'infoRemoved'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('DitransitivePPCompFrame', 'From-TheWord', 'InformationRemoving', 'PPCompFrameFn', 'and', 'doneBy', 'infoRemoved', 'informationOrigin', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('InformationRemoving', 'TransitiveNPCompFrame', 'and'('isa'(':ACTION', ':DENOT'), 'infoRemoved'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('InformationRemoving', 'TransitiveNPCompFrame', 'and', 'doneBy', 'infoRemoved', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('Killing-Biological', 'TransitiveNPCompFrame', 'and'('isa'(':ACTION', ':DENOT'), 'inputsDestroyed'(':ACTION', ':OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('Killing-Biological', 'TransitiveNPCompFrame', 'and', 'doneBy', 'inputsDestroyed', 'isa', ':ACTION', ':DENOT', ':OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('LeavingAPlace', 'PPCompFrameFn'('TransitivePPCompFrame', 'From-TheWord'), 'and'('isa'(':ACTION', ':DENOT'), 'fromLocation'(':ACTION', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT')), 'GeneralEnglishMt', v(v('From-TheWord', 'LeavingAPlace', 'PPCompFrameFn', 'TransitivePPCompFrame', 'and', 'doneBy', 'fromLocation', 'isa', ':ACTION', ':DENOT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
%'verbSemTransTemplate'('Bartering', 'PPCompFrameFn'('DitransitivePPCompFrame', 'For-TheWord'), 'thereExists'(A, 'thereExists'(B, 'and'('isa'(':ACTION', ':DENOT'), 'exchangers'(':ACTION', ':SUBJECT'), 'subEvents'(':ACTION', A), 'subEvents'(':ACTION', B), 'toPossessor'(B, ':SUBJECT'), 'objectOfPossessionTransfer'(A, ':OBLIQUE-OBJECT'), 'objectOfPossessionTransfer'(B, ':OBJECT'), 'fromPossessor'(A, ':SUBJECT'), 'reciprocalTransfers'(A, B)))), 'GeneralEnglishMt', v(v('Bartering', 'DitransitivePPCompFrame', 'For-TheWord', 'PPCompFrameFn', 'and', 'exchangers', 'fromPossessor', 'isa', 'objectOfPossessionTransfer', 'reciprocalTransfers', 'subEvents', 'thereExists', 'toPossessor', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), ['?T1'=A, '?T2'=B|C])).
%'verbSemTransTemplate'('CarryingWhileLocomoting', 'PPCompFrameFn'('DitransitivePPCompFrame', 'By-TheWord'), 'and'('isa'(':ACTION', ':DENOT'), 'transportees'(':ACTION', ':OBJECT'), 'physicalParts'(':OBJECT', ':OBLIQUE-OBJECT'), 'doneBy'(':ACTION', ':SUBJECT'), 'objectsInContact'(':ACTION', ':OBLIQUE-OBJECT', ':SUBJECT')), 'GeneralEnglishMt', v(v('By-TheWord', 'CarryingWhileLocomoting', 'DitransitivePPCompFrame', 'PPCompFrameFn', 'and', 'doneBy', 'isa', 'objectsInContact', 'physicalParts', 'transportees', ':ACTION', ':DENOT', ':OBJECT', ':OBLIQUE-OBJECT', ':SUBJECT'), A)).
verb_phrase_known(CycWord,Subj,Event,verbSemTransTemplate(Out,EventMidO)) -->
      {'verbSemTransTemplate'(EventIsa,NextFrame, Template,_,_)},
	 verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,EventMid),
      frame_template(NextFrame,Obj,Result,Extras),
   {apply_frame(Template,Subj,Event,Object,Result,OutD),subst(OutD,':DENOT',EventIsa,Out)}.

verb_phrase_known(CycWord,Subj,Event,auxV(Out)) --> isPOS('AuxVerb',_),
      verb_phrase_known(CycWord,Subj,Event,Out).


% =======================================================
% verb_phrase_event_isa
% =======================================================
verb_phrase_event_isa(CycWord,EventIsa,Subj,Object,Event,Out) -->
      {cycWordForISA(CycWord,EventIsa)},verb_phrase_known(CycWord,Subj,Event,Out).

% =======================================================
%'semTransPredForPOS'('Verb', 'verbSemTrans', 'EnglishMt', v(v('Verb', 'verbSemTrans'), A)).
% =================================================================
apply_frame(Formula,Subj,Event,Obj,Target,CycL):-
      varnameIdea('ACTION',Event),
      varnameIdea('SUBJECT',Subj),
      varnameIdea('OBJECT',Obj),
      varnameIdea('OBLIQUE',Target),
      subst(Formula,':SUBJECT',Subj,Formula1),
      subst(Formula1,':NOUN',Subj,Formula2),
      subst(Formula2,':ACTION',Event,Formula3),
      subst(Formula3,':OBJECT',Obj,Formula4),
      subst(Formula4,':EVENT',Event,Formula5),
      subst(Formula5,':OBLIQUE-OBJECT',Target,Formula6),
      subst(Formula6,':ARG1',Subj,Formula7),
      subst(Formula7,':VERB',Event,Formula8),
      subst(Formula8,':ARG2',Obj,Formula9),
      subst(Formula9,':EVENT',Event,Formula10),
      subst(Formula10,':ARG3',Target,CycL).

contains_obliqe(Formula):-flatten(Formula,Flat),member(':OBLIQUE-OBJECT',Flat).


% =======================================================
% frame_template
% =======================================================
frame_template('TransitiveNPCompFrame',Obj,Result,Extras) --> noun_phrase(Obj,true,Extras). 
%$DitransitivePPCompFrame','TransitivePPCompFrame'
frame_template('PPCompFrameFn'(_,CycPrep),Obj,Result,Extras) --> isCycWord(CycPrep),{!},best_subject(Result,true,Extras).
frame_template('PPCompFrameFn'(_,CycPrep),Obj,Result,Extras) --> noun_phrase(Obj,true,Mid),isCycWord(CycPrep),{!},best_subject(Result,Mid,Extras).
frame_template('RegularAdjFrame',Subj,Result,Extras) -->noun_phrase(Subj,true,Extras).


% ==========================================================
% String to String
% ==========================================================



removeRepeats1([],[]):-!.
removeRepeats1([H|T],[HH|TT]):- stringToString(H,HH),!,removeRepeats1(T,TT).
removeRepeats1([H,H1|Rest],Out):-toLowercase(H,H1),!,removeRepeats1([H|Rest],Out).
removeRepeats1([H|Rest],[H|Out]):-removeRepeats1(Rest,Out).
removeRepeats1(X,X).

removeRepeats2(X,X):-!.

removeRepeats2(X,O):-append(L,R,X),
	    append([S,O|Me],LL,L),
	    append([S,O|Me],RR,R),!,
	    flatten([[S,O|Me],LL,RR],O).

stringToString(Before,After):-'abbreviationForString'(After, Before, _,_).

simplifyLF(Result,Result):-!.

cycQueryIsa(X,Y):-fail,writeq(cycQueryIsa(X,Y)),nl.

/*

% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================
% ==========================================================

% String Based
'genTemplate'('abnormal', 'ConcatenatePhrasesFn'('TermParaphraseFn'(':ARG1'), 'BestNLPhraseOfStringFn'([-, with, the, exception, of]), 'TermParaphraseFn'(':ARG2')), 'EnglishParaphraseMt', v(v('BestNLPhraseOfStringFn', 'ConcatenatePhrasesFn', 'TermParaphraseFn', 'abnormal', -, ':ARG1', ':ARG2', exception, of, the, with), A)).
'genTemplate-Constrained'('arg1Isa', 'isa'(':ARG1', 'BinaryPredicate'), 'TermParaphraseFn'('implies'('thereExists'(A, [':ARG1', B, A]), 'ConcatenatePhrasesFn'('TermParaphraseFn-NP'(B), 'BestNLPhraseOfStringFn'([must, be]), 'BestDetNbarFn-Indefinite'('TermParaphraseFn-Constrained'('nonPlural-Generic', ':ARG2'))))), 'EnglishParaphraseMt', v(v('BestDetNbarFn-Indefinite', 'BestNLPhraseOfStringFn', 'BinaryPredicate', 'ConcatenatePhrasesFn', 'TermParaphraseFn', 'TermParaphraseFn-Constrained', 'TermParaphraseFn-NP', 'arg1Isa', 'implies', 'isa', 'nonPlural-Generic', 'thereExists', ':ARG1', ':ARG2', be, must), ['?Y'=A, '?X'=B|C])).
'genQuestion'('genls', 1, [what, kinds, of, '~a', are, there], [[2, ':NON-SINGULAR-GENERIC']], 'EnglishParaphraseMt', v(v('genls', ':NON-SINGULAR-GENERIC', are, kinds, of, there, what, '~a'), A)).
'genQuestion'('hasAttributes', 2, ['What', attributes, does, '~a', 'have?'], [1], 'EnglishParaphraseMt', v(v('hasAttributes', 'What', attributes, does, 'have?', '~a'), A)).
%'assertTemplate-Reln'(TemplateType,Predicate, Match, Template, _,_).
	    %InfinitivalVPTemplate'
	    % 'aspectPerfect'
	    %[have, not, 'PerfectiveVPTemplate'(':VP-CONT')]
	    %'NLNegFn'('NotVP-NLAttr', 'aspectPerfect'(':VP-CONT'))


% meetsParam(transverb,String,CycWord):-genFormatAccess(String,CycWord).

%genFormat(Verb,X,'NIL',_,_):-'genFormat'(Verb,X,_,_,_).


'genFormat'('abbreviationString-PN', abbreviation, 'NIL', 'EnglishParaphraseMt', v(v('abbreviationString-PN', 'NIL', abbreviation), A)).
'genFormat'('abbreviationString-PN', ['~a', is, the, abbreviated, form, of, the, name, for, '~a'], [[2, ':QUOTE'], 1], 'EnglishParaphraseMt', v(v('abbreviationString-PN', ':QUOTE', abbreviated, for, form, is, name, of, the, '~a'), A)).
'genFormat'('adjSemTrans', ['~a', is, the, semantic, translation, of, word, sense, number, '~a', of, '~a', with, the, subcategorization, frame, '~a'], [[4, ':QUOTE'], 2, [1, ':QUOTE'], [3, ':QUOTE']], 'EnglishParaphraseMt', v(v('adjSemTrans', ':QUOTE', frame, is, number, of, semantic, sense, subcategorization, the, translation, with, word, '~a'), A)).
'genFormat'('adjSemTrans-Restricted', [the, semantic, translation, of, word, sense, number, '~a', of, '~a', (','), when, modifying, '~a', with, the, subcategorization, frame, '~a', (','), is, '~a'], [2, [1, ':QUOTE'], [4, ':A'], [3, ':QUOTE'], 5], 'EnglishParaphraseMt', v(v('adjSemTrans-Restricted', (','), ':A', ':QUOTE', frame, is, modifying, number, of, semantic, sense, subcategorization, the, translation, when, with, word, '~a'), A)).
'genFormat'('Area1023', ['Troi', '\'', s, 'Quarters'], 'NIL', 'EnglishParaphraseMt', v(v('Area1023', '\'', 'NIL', 'Quarters', 'Troi', s), A)).
'genFormat-ArgFixed'('SubcollectionOfWithRelationFromTypeFn', 2, 'surfaceParts', ['~A', on, '~A'], [[1, ':MASS-NUMBER', ':PLURAL', ':GERUND'], [3, ':PLURAL', ':MASS-NUMBER']], 'EnglishParaphraseMt', v(v('SubcollectionOfWithRelationFromTypeFn', 'surfaceParts', ':GERUND', ':MASS-NUMBER', ':PLURAL', on, '~A'), A)).
'genFormat-ArgFixed'('SubcollectionOfWithRelationToFn', 2, 'containsInformationAbout', ['~A', about, '~A'], [[1, ':MASS-NUMBER', ':PLURAL', ':GERUND'], 3], 'EnglishParaphraseMt', v(v('SubcollectionOfWithRelationToFn', 'containsInformationAbout', ':GERUND', ':MASS-NUMBER', ':PLURAL', about, '~A'), A)).
'genFormat-ArgFixed'('SubcollectionOfWithRelationToFn', 2, 'eventOccursAt', ['~A', in, '~A'], [[1, ':MASS-NUMBER', ':PLURAL', ':GERUND'], 3], 'EnglishParaphraseMt', v(v('SubcollectionOfWithRelationToFn', 'eventOccursAt', ':GERUND', ':MASS-NUMBER', ':PLURAL', in, '~A'), A)).
'genFormat-Precise'('synonymousExternalConcept', [the, 'Cyc', concept, '~s', is, synonymous, with, the, concept, named, by, '~s', in, the, external, data, source, '~a'], [1, 3, 2], 'EnglishParaphraseMt', v(v('synonymousExternalConcept', 'Cyc', by, concept, data, external, in, is, named, source, synonymous, the, with, '~a', '~s'), A)).
'genFormat-Precise'('tastes', [the, agent, '~a', can, taste, '~a'], 'NIL', 'EnglishParaphraseMt', v(v('tastes', 'NIL', agent, can, taste, the, '~a'), A)).
'genFormat-Precise'('temporalBoundsIntersect', [the, temporal, interval, of, '~a', intersects, the, temporal, interval, of, '~a'], 'NIL', 'EnglishParaphraseMt', v(v('temporalBoundsIntersect', 'NIL', intersects, interval, of, temporal, the, '~a'), A)).



'formalityOfWS'('Aussie-TheWord', 'ProperCountNoun', 0, 'InformalSpeech', 'GeneralEnglishMt', v(v('Aussie-TheWord', 'InformalSpeech', 'ProperCountNoun'), A)).
'formalityOfWS'('Babe-TheWord', 'SimpleNoun', 1, 'InformalSpeech', 'GeneralEnglishMt', v(v('Babe-TheWord', 'InformalSpeech', 'SimpleNoun'), A)).
'politenessOfWS'('Cock-TheWord', 'SimpleNoun', 1, 'VulgarSpeech', 'GeneralEnglishMt', v(v('Cock-TheWord', 'SimpleNoun', 'VulgarSpeech'), A)).

% ==========================================================
% WordsList Heuristics
% ==========================================================
'determinerAgreement'('A-Dozen-MWW', 'plural-Generic', 'EnglishMt', v(v('A-Dozen-MWW', 'plural-Generic'), A)).

'denotesArgInReln'('Acquaint-TheWord', 'CountNoun', 'acquaintedWith', 2, 'GeneralEnglishMt', v(v('Acquaint-TheWord', 'CountNoun', 'acquaintedWith'), A)).
'generateArgWithOutsideScope'('several-GenQuantRelnToType', 2, 'ParaphraseMt', v(v('several-GenQuantRelnToType'), A)).
'generateQuantOverArg'('few-GenQuantRelnFrom', 'Few-NLAttr', 3, 'ParaphraseMt', v(v('Few-NLAttr', 'few-GenQuantRelnFrom'), A)).
'genNatTerm-ArgLast'('PureFn', [pure], 'Noun', 'EnglishMt', v(v('Noun', 'PureFn', pure), A)).
'genNatTerm-compoundString'('AttemptingFn', 'Try-TheWord', [to], 'Verb', 'infinitive', 'EnglishMt', v(v('AttemptingFn', 'Try-TheWord', 'Verb', 'infinitive', to), A)).
'genNatTerm-multiWordString'('TreatmentFn', 'NIL', 'Treatment-TheWord', 'MassNoun', 'nonPlural-Generic', 'EnglishMt', v(v('MassNoun', 'Treatment-TheWord', 'TreatmentFn', 'nonPlural-Generic', 'NIL'), A)).
%'headsPhraseOfType'('Pronoun', 'Noun', 'GeneralLexiconMt', v(v('Noun', 'Pronoun'), A)).
'ncRuleConstraint'('AttackingDogs-NCR', 'NCGenlsConstraintFn'('TheNCModifier', 'Event'), 'GeneralLexiconMt', v(v('AttackingDogs-NCR', 'Event', 'NCGenlsConstraintFn', 'TheNCModifier'), A)).
'ncRuleLabel'('WaterSolution-NCR', [water, solution], 'GeneralLexiconMt', v(v('WaterSolution-NCR', solution, water), A)).
'ncRuleTemplate'('AnimalPopulations-NCR', 'SubcollectionOfWithRelationToTypeFn'('TheNCHead', 'groupMembers', 'TheNCModifier'), 'GeneralLexiconMt', v(v('AnimalPopulations-NCR', 'SubcollectionOfWithRelationToTypeFn', 'TheNCHead', 'TheNCModifier', 'groupMembers'), A)).
'posForTemplateCategory'('Verb', 'ProgressiveVPTemplate', 'EnglishTemplateMt', v(v('ProgressiveVPTemplate', 'Verb'), A)).
'posOfPhraseType'('NounPhrase', 'Noun', 'GeneralLexiconMt', v(v('Noun', 'NounPhrase'), A)).
'posOfPhraseType'('PhraseFn'(A), A, 'GeneralLexiconMt', v(v('PhraseFn', '$VAR'), ['VAR1'=A|B])).
'posPredForTemplateCategory'('presentParticiple', 'ProgressiveVPTemplate', 'EnglishTemplateMt', v(v('ProgressiveVPTemplate', 'presentParticiple'), A)).
%'prepCollocation'('Beset-TheWord', 'Adjective', 'By-TheWord').      
'prepCollocation'('Wrangle-TheWord', 'Verb', 'With-TheWord', 'EnglishMt', v(v('Verb', 'With-TheWord', 'Wrangle-TheWord'), A)).
'relationIndicators'('ailmentConditionAffects', 'Infect-TheWord', 'SimpleNoun', 'EnglishMt', v(v('Infect-TheWord', 'SimpleNoun', 'ailmentConditionAffects'), A)).
'requiredActorSlots'('MonetaryExchangeOfUserRights', 'buyer', 'HumanActivitiesMt', v(v('MonetaryExchangeOfUserRights', 'buyer'), A)).
%'semTransArg'('adjSemTrans', 4, 'GeneralLexiconMt', v(v('adjSemTrans'), A)).
%'semTransArg'('adjSemTrans-Restricted', 5, 'GeneralLexiconMt', v(v('adjSemTrans-Restricted'), A)).
'subcatFrame'('Argue-TheWord', 'Verb', 0, 'TransitiveNPCompFrame', 'GeneralEnglishMt', v(v('Argue-TheWord', 'TransitiveNPCompFrame', 'Verb'), A)).
'subcatFrameArity'('Post-NounPhraseModifyingFrame', 1, 'GeneralLexiconMt', v(v('Post-NounPhraseModifyingFrame'), A)).
'subcatFrameDependentConstraint'('TransitiveNPCompFrame', 1, 'PhraseFn'('Noun'), 'GeneralLexiconMt', v(v('Noun', 'PhraseFn', 'TransitiveNPCompFrame'), A)).
'subcatFrameDependentKeyword'('Post-NounPhraseModifyingFrame', 1, ':OBJECT', 'GeneralLexiconMt', v(v('Post-NounPhraseModifyingFrame', ':OBJECT'), A)).
'subcatFrameKeywords'('MiddleVoiceFrame', ':ACTION', 'InferencePSC', v(v('MiddleVoiceFrame', ':ACTION'), A)).

'psRuleArity'('PSRule-AdjPFromAdj', 1, 'EnglishLexiconMt', v(v('PSRule-AdjPFromAdj'), A)).
'psRuleArity'('PSRule-AdvP-AdvPAdvP', 2, 'EnglishLexiconMt', v(v('PSRule-AdvP-AdvPAdvP'), A)).
'psRuleCategory'('PSRule-V-VAdvP', 'Verb', 'EnglishLexiconMt', v(v('PSRule-V-VAdvP', 'Verb'), A)).
'psRuleConstraint'('PSRule-AdjPFromAdj', 'ConstituentTypeConstraintFn'(1, 'Adjective'), 'EnglishLexiconMt', v(v('Adjective', 'ConstituentTypeConstraintFn', 'PSRule-AdjPFromAdj'), A)).
'psRuleExample'('PSRule-VbarVComps', [likes, emus], 'EnglishLexiconMt', v(v('PSRule-VbarVComps', emus, likes), A)).
'psRuleSemanticsFromDtr'('PSRule-DbarFromDet', 1, 'EnglishLexiconMt', v(v('PSRule-DbarFromDet'), A)).
'psRuleSemanticsHandler'('PSRule-NP-DetNbar', 'PSP-SEMX-FOR-DET-NBAR', 'EnglishLexiconMt', v(v('PSRule-NP-DetNbar', 'PSP-SEMX-FOR-DET-NBAR'), A)).
'psRuleSyntacticHeadDtr'('PSRule-AdjPFromAdj', 1, 'EnglishLexiconMt', v(v('PSRule-AdjPFromAdj'), A)).
'psRuleTemplateBindings'('PSRule-V-VAdvP', 'PSBindingFn'(1, ':ACTION'), 'EnglishLexiconMt', v(v('PSBindingFn', 'PSRule-V-VAdvP', ':ACTION'), A)).
'psRuleTemplateDtr'('PSRule-AdjPFromAdj', 1, 'EnglishLexiconMt', v(v('PSRule-AdjPFromAdj'), A)).

*/
/*===================================================================
Convert S-Expression originating from user to a Prolog Clause representing the surface level

Recursively creates a Prolog term based on the S-Expression to be done after compiler
                                                 
Examples:

| ?- sterm_to_pterm([a,b],Pterm).
Pterm = a(b)

| ?- sterm_to_pterm([a,[b]],Pterm).    %Note:  This is a special Case
Pterm = a(b)

| ?- sterm_to_pterm([holds,X,Y,Z],Pterm).    %This allows Hilog terms to be Converted
Pterm = _h76(_h90,_h104)                    

| ?- sterm_to_pterm([X,Y,Z],Pterm).   %But still works in normal places
Pterm = _h76(_h90,_h104)                    

| ?- sterm_to_pterm(['AssignmentFn',X,[Y,Z]],Pterm).                                
Pterm = 'AssignmentFn'(_h84,[_h102,_h116])
====================================================================
*/

sterm_to_pterm(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm([VAR],VAR):-isSlot(VAR),!.
sterm_to_pterm([X],Y):-!,nonvar(X),sterm_to_pterm(X,Y).

sterm_to_pterm([S|TERM],PTERM):-isSlot(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-number(S),!,
            sterm_to_pterm_list([S|TERM],PTERM).            
	    
sterm_to_pterm([S|TERM],PTERM):-nonvar(S),atomic(S),!,
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[S|PLIST].

sterm_to_pterm([S|TERM],PTERM):-!,  atomic(S),
            sterm_to_pterm_list(TERM,PLIST),            
            PTERM=..[holds,S|PLIST].

sterm_to_pterm(VAR,VAR):-!.

sterm_to_pterm_list(VAR,VAR):-isSlot(VAR),!.
sterm_to_pterm_list([],[]):-!.
sterm_to_pterm_list([S|STERM],[P|PTERM]):-!,
              sterm_to_pterm(S,P),
              sterm_to_pterm_list(STERM,PTERM).
sterm_to_pterm_list(VAR,[VAR]).


atom_junct(Atom,Words):-!,to_word_list(Atom,Words),!.

atom_junct(Atom,Words):-
   concat_atom(Words1,' ',Atom),
   atom_junct2(Words1,Words),!.

atom_junct2([],[]).
atom_junct2([W|S],[A,Mark|Words]):- member(Mark,['.',',','?']),atom_concat(A,Mark,W),not(A=''),!,atom_junct2(S,Words).
atom_junct2([W|S],[Mark,A|Words]):- member(Mark,['.',',','?']),atom_concat(Mark,A,W),not(A=''),!,atom_junct2(S,Words).
atom_junct2([W|S],[W|Words]):-atom_junct2(S,Words).

% :- include(logicmoo(vworld/moo_footer)).


:-  moodb:end_transform_moo_preds.

