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



chunkParseCycL(Event,Subj,Tagged,CycL9):-
        sentenceChunker(Tagged,Chunks),nl,
        parseCycLChunk(and(isa(Event,'Situation'),situationConstituents(Event,Subj)),Chunks,Event,Subj,CycL9).



parseCycLChunk(_CycLIn,Chunks,_Event,Subj,_):-once(dumpList(Chunks)),nl,true,fail.

% tail closer
parseCycLChunk(CycLIn,[],Event,Subj,CycLIn):-!,suggestVar('SENTENCE',Event),suggestVar('SUBJECT',Subj).

parseCycLChunk(CycLIn,[seg(cc,_,_,_)|Const],Event,Subj,CycL) :-
         parseCycLChunk(CycLIn,Const,Event,Subj,CycL).

parseCycLChunk(CycLIn,[seg(cs,_,_,_)|Const],Event,Subj,CycL) :-
         parseCycLChunk(CycLIn,Const,Event,Subj,CycL).

parseCycLChunk(CycLIn,[seg(sym,_,[X],_)|Const],Event,Subj,CycL):- cmember(X,[('.'),('!'),(',')]),!,
         parseCycLChunk(CycLIn,Const,Event,Subj,CycL).

parseCycLChunk(CycLIn,Const,Event,Subj,'CYC-ASSERT'(CycL)) :- append(Const1,[seg(sym,_,[X],_)],Const),cmember(X,[('.'),('!'),(',')]),!,
         parseCycLChunk(CycLIn,Const1,Event,Subj,CycL).

parseCycLChunk(CycLIn,Const,Event,Subj,'CYC-QUERY'(CycL)) :- append(Const1,[seg(sym,_,[X],_)],Const),cmember(X,[('?')]),!,
         parseCycLChunk(CycLIn,Const1,Event,Subj,CycL).

objTypeCompat(ObjType2,ObjType1):-var(ObjType2),var(ObjType1),!.
objTypeCompat(ObjType2,ObjType1):-(var(ObjType2);var(ObjType1)),!,fail.
objTypeCompat(ObjType1,ObjType1).



isConjoined(seg(in,_,[with],_),with).
isConjoined(seg(cc,_,[Conj],_),Conj).

% theObject and theObject -> theObject
parseCycLChunk(CycLIn,Const,Event,SubjThru,CycLO) :- 
    append(ConstL,[theObject(ObjType1,Subj1,W1),WITH,theObject(ObjType2,Subj2,W2)|ConstR],Const),
    isConjoined(WITH,Conj),objTypeCompat(ObjType2,ObjType1),!,
         append(W1,[Conj|W2],W),
         append(ConstL,[theObject(ObjType1,Subj,W)|ConstR],NConst),
         parseCycLChunk(and(memberOfList(Subj,'TheList'(Subj1,Subj2)),CycLIn),NConst,Event,SubjThru,CycLO),!,
         ignore(SubjThru=Subj),
         suggestVar(W,Subj),!.

% Convert PN -> NPs
parseCycLChunk(CycLIn,Const,Event,SubjThru,CycLO) :- 
   append(ConstL,[seg(pn,T1,W1,D1)|ConstR],Const),
         append(ConstL,[seg(np,T1,W1,D1)|ConstR],NConst),
         parseCycLChunk(CycLIn,NConst,Event,SubjThru,CycLO).

% First  NP -> theObject
%parseCycLChunk(CycLIn,[seg(np,T1,W1,D1)|ConstR],Event,Subj,CycL) :- 
%         phrase(noun_unit(T1,Event,Subj,CycLO,CycL),D1),
%         parseCycLChunk(CycLIn,[theObject(ObjType,Subj,W1)|ConstR],Event,Subj,CycLO).

% NP + OF + NP 
parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[seg(np,T1,W1,D1),seg(in,'IN',[of],[PTAG]),seg(np,T2,W2,D2)|ConstR],Const),
         append(W1,[of|W2],W),append(D1,[PTAG|D2],D),
         append(ConstL,[seg(np,T2,W,D)|ConstR],NConst),%!,
         parseCycLChunk(CycLIn,NConst,EventThru,SubjThru,CycLOut).

% NP + AT + NP 
no_parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[seg(np,T1,W1,D1),seg(in,'IN',[at],[PTAG]),seg(np,T2,W2,D2)|ConstR],Const),
         append(W1,[at|W2],W),append(D1,[PTAG|D2],D),
         append(ConstL,[seg(np,T2,W,D)|ConstR],NConst),%!,
         parseCycLChunk(CycLIn,NConst,EventThru,SubjThru,CycLOut).


% NP -> theObject
parseCycLChunk(CycLIn,Const,Event,SubjThru,CycL) :- 
   append(ConstL,[seg(np,T1,W1,D1)|ConstR],Const),
         asNounUnit(ObjType,T1,D1,W1,Event,Subj,CycLO,CycL),
         append(ConstL,[theObject(ObjType,Subj,W1)|ConstR],NConst),
         parseCycLChunk(CycLIn,NConst,Event,SubjThru,CycLO),suggestVar(W1,Subj).

         
% theObject + VP + theObject + IN + theObject -> theEvent
parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[theObject(ObjType1,Subj,SWords),seg(vp,Type,TXT,VTAG),theObject(ObjType2,Obj,OWords),seg(in,PType,PTXT,PTAG),theObject(ObjType3,Target,TWords)|ConstR],Const),
         append(ConstL,[theObject('Action',Event,TXT),theObject(ObjType1,Subj,SWords)|ConstR],NConst),
         phrase(verb_prep(Event,Subj,Obj,PTAG,Target,CycL),VTAG),
         parseCycLChunk(and(CycL,CycLIn),NConst,EventThru,SubjThru,CycLOut),suggestVar(TXT,Event).

% theObject + VP + theObject + IN + theObject -> theEvent
parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[theObject(ObjType1,Subj,SWords),seg(vp,Type,TXT,VTAG),seg(in,PType,PTXT,PTAG),theObject(ObjType3,Target,TWords)|ConstR],Const),
         append(ConstL,[theObject('Action',Event,TXT),theObject(ObjType1,Subj,SWords)|ConstR],NConst),
         phrase(verb_prep(Event,Subj,Target,PTAG,Target,CycL),VTAG),
         parseCycLChunk(and(CycL,CycLIn),NConst,EventThru,SubjThru,CycLOut),suggestVar(TXT,Event).

% theObject + VP + IN + theObject -> theEvent
parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[theObject(ObjType1,Subj,SWords),seg(vp,Type,TXT,VTAG),seg(in,PType,PTXT,PTAG),theObject(ObjType2,Target,TWords)|ConstR],Const),
         append(ConstL,[theObject('Action',Event,TXT),theObject(ObjType1,Subj,SWords)|ConstR],NConst),
         append(VTAG,PTAG,TAGGED),
         phrase(verb2(Event,Subj,Target,CycL),TAGGED),
         parseCycLChunk(and(CycL,CycLIn),NConst,EventThru,SubjThru,CycLOut),suggestVar(TXT,Event).

% theObject + VP + theObject -> theEvent
parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[theObject(ObjType1,Subj,SWords),seg(vp,Type,TXT,VTAG),theObject(ObjType2,Obj,OWords)|ConstR],Const),
         append(ConstL,[theObject('Action',Event,TXT),theObject(ObjType1,Subj,SWords)|ConstR],NConst),
         phrase(verb2(Event,Subj,Obj,CycL),VTAG),
         parseCycLChunk(and(CycL,CycLIn),NConst,EventThru,SubjThru,CycLOut),suggestVar(TXT,Event).

% theObject + VP  -> theEvent
parseCycLChunk(CycLIn,Const,EventThru,SubjThru,CycLOut):- 
   append(ConstL,[theObject(ObjType1,Subj,SWords),seg(vp,Type,TXT,VTAG)|ConstR],Const),
         append(ConstL,[theObject('Action',Event,TXT),theObject(ObjType1,Subj,SWords)|ConstR],NConst),
         phrase(verb1(Event,Subj,CycL),VTAG),
         parseCycLChunk(and(CycL,CycLIn),NConst,EventThru,SubjThru,CycLOut),suggestVar(TXT,Event).

% Convert IN -> VP
parseCycLChunk(CycLIn,Const,Event,SubjThru,CycLO) :- 
    append(ConstL,[seg(in,T1,W1,D1)|ConstR],Const),
         append(ConstL,[seg(vp,T1,W1,D1)|ConstR],NConst), !,
         parseCycLChunk(CycLIn,NConst,Event,SubjThru,CycLO).

% Skipovers
parseCycLChunk(CycLIn,[theObject(Action,Var,Words)|Rest],EventThru,SubjThru,and('subEvents'(EventThru,Var),CycL)) :-
    Action == 'Action',!,suggestVar(Var,Words),      
         parseCycLChunk(CycLIn,Rest,EventThru,SubjThru,CycL).

parseCycLChunk(CycLIn,[theObject(ObjType,Var,Words)|Rest],EventThru,SubjThru,and('doom:descriptionStrings'(Var,string(Words)),CycL)) :-!, suggestVar(Var,Words),
         ignore(Var=SubjThru),parseCycLChunk(CycLIn,Rest,EventThru,SubjThru,CycL).

parseCycLChunk(CycLIn,[seg(PC,Type,Words,Tagged)|Rest],EventThru,SubjThru,and('doom:descriptionStrings'(Subj,string(Words),PC),CycL)) :- !,
         parseCycLChunk(CycLIn,Rest,EventThru,SubjThru,CycL), suggestVar(Subj,Words).

parseCycLChunk(CycLIn,[Err|Rest],Event,Subj,and('doom:descriptionErrorStrings'(Subj,string(Words)),CycL)) :- true,
         parseCycLChunk(CycLIn,Rest,Event,SubjNext,CycL).

verb1(Event,Subj,and(actors(Event,Subj),CycL))-->verb_unit(Type,Time,Subj,Obj,Target,Event,CycL).
verb2(Event,Subj,Obj,CycL)-->verb_unit(Type,Time,Subj,Obj,Target,Event,CycL).
skip(string(C))-->[X],{getText(X,C)}.




% =======================================================
% sentence constituent breaker
% =======================================================
%sentenceChunker(Ws,[]):-!.

sentenceChunker([],[]).
sentenceChunker([W|Ws],[New|Out]):-
      notrace(isTagChunk(W,Constit)),
      gatherChunk(Constit,Ws,NW,Rest),
      createChunk(Constit,[W|NW],New),
      sentenceChunker(Rest,Out).

createChunk(Constit,WNW,New):-
      notrace(getLoc(WNW,Loc)),
      notrace(getText(WNW,Text)),
      Constit=..List,
      append([seg|List],[Text,WNW],Out),!,
      New=..Out.

chunkHead(W,Type,RA):-cmember([txt|Text],W),cmember(RA:NGE,W),cmember(1.0-Type,W),!.

gatherChunk(Constit,[],[],[]):-!.
gatherChunk(Constit,[W|Ws],[W|SWords],Rest):-
      notrace(isTagChunk(W,New)),
      chunkCompat(Constit,New),!,
      gatherChunk(Constit,Ws,SWords,Rest),!.
gatherChunk(Constit,Rest,[],Rest).

chunkCompat(Start,End):-functor(Start,C,_),functor(End,C,_).

%isTagChunk(W,np('IN')):-getText(W,[of]).
%isTagChunk(W,np('IN')):-getText(W,[for]).
isTagChunk(W,np('PRONOUN')):-getText(W,['I']).
isTagChunk(W,in('IN')):-isTag(W,'Preposition'),!.
isTagChunk(W,vp('Modal')):-isTag(W,'Modal'),!.
isTagChunk(W,vp('AuxVerb')):-isTag(W,'AuxVerb'),!.
isTagChunk(W,vp('AuxVerb')):-isTag(W,'BeAux'),!.
isTagChunk(W,vp('AuxVerb')):-isTag(W,'HaveAux'),!.
isTagChunk(W,wh('WHDeterminer')):-isTag(W,'WHDeterminer'),!.
isTagChunk(W,wh('WHAdverb,')):-isTag(W,'WHAdverb'),!.
isTagChunk(W,cc('CoordinatingConjunction')):-isTag(W,'cc'),!.
isTagChunk(W,cc('CoordinatingConjunction')):-isTag(W,'CoordinatingConjunction'),!.
isTagChunk(W,cc('SubordinatingConjunction')):-isTag(W,'SubordinatingConjunction'),!.
isTagChunk(W,np('POSS')):-isTag(W,'Possessive'),!.
isTagChunk(W,np('POSS')):-isTag(W,'PossessivePronoun'),!.
isTagChunk(W,np('PRONOUN')):-isTag(W,'Pronoun'),!.
isTagChunk(W,np('PROPER')):-isTag(W,'ProperNoun'),!.
isTagChunk(W,np('DET')):-isTag(W,'Determiner'),!.
isTagChunk(W,np('Adjective')):-isTag(W,'Adjective'),!.
isTagChunk(W,np('QUANT')):-isTag(W,'Quantifier'),!.
isTagChunk(W,vp('Verb')):-isTag(W,'Verb'),!.
isTagChunk(W,sym('Interjection-SpeechPart')):-isTag(W,'Interjection-SpeechPart'),!.
isTagChunk(W,sym('SYM')):-isTag(W,'.'),!.
isTagChunk(W,sym('SYM')):-isTag(W,'?'),!.
isTagChunk(W,sym('SYM')):-isTag(W,'!'),!.
isTagChunk(W,sym('SYM')):-isTag(W,'.'),!.
isTagChunk(W,np('COMMON')):-isTag(W,'nn'),!.
isTagChunk(W,vp('Adverb')):-isTag(W,'Adverb'),!.
isTagChunk(W,np('OTHER')):-!.

%sentenceParse(Event,Types,Joined):-joinTypes(Types,Joined).
      
%joinTypes(Types,Joined):-     %most of the features i keep off not to violate copywrites .. since the system was designed arround closed src software i had to emulate.. but i hate writting docs which makes me code arround other peoples manuals
      


/*
TODO These two forms are totally identical?

Mt : EnglishParaphraseMt
(genFormat-Precise hasStoredInside "~a is stored in ~a when not in use" 
       (TheList 2 1))
genTemplate :  (NPIsXP-NLSentenceFn 
   (TermParaphraseFn-NP :ARG2) 
   (ConcatenatePhrasesFn 
       (BestNLPhraseOfStringFn "stored") 
       (BestPPFn In-TheWord 
           (TermParaphraseFn-NP :ARG1))))
 

*/

%>
