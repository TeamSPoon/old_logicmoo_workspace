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
[14:52] <kinoc> http://turing.cs.washington.edu/papers/yates_dissertation.pdf
[14:54] <kinoc> this was the one I was looking for  http://www.cs.washington.edu/homes/soderlan/aaaiSymposium2007.pdf
[15:01] <kinoc> http://www.pat2pdf.org/patents/pat20080243479.pdf  while it lasts
% http://danielmclaren.net/2007/05/11/getting-started-with-opennlp-natural-language-processing/
% http://opennlp.sourceforge.net
% http://www.cs.washington.edu/research/textrunner/index.html
*/
% ===================================================================
/*
:-module(e2c,[
	e2c/1,
	e2c/2,
	testE2C/0]).
(SIMPLIFY-CYCL-SENTENCE-SYNTAX ' (#$and 
    (#$and 
      (#$isa ?THEY-2 
          (#$PronounFn #$ThirdPerson-NLAttr #$Plural-NLAttr #$Ungendered-NLAttr #$ObjectPronoun)))
    (#$and 
      (#$and 
        (#$isa ?THEY-2 Agent-Generic) 
        (#$and 
          (#$preActors ?G20477 ?THEY-2) 
          (#$knows ?THEY-2 ?G57625))) 
      (#$and 
        (#$preActors ?G20477 ?THEY-2) 
        (#$knows ?THEY-2 ?G57625)))))

*/
simplifyCycL(In,Out):- catch((cyc:termCyclify(In,Mid),evalSubL('SIMPLIFY-CYCL-SENTENCE-SYNTAX'(quote(Mid)),Out,_)),E,(writeCycL(E),fail)),not(Out=[unk_comment|_]),!.
simplifyCycL(In,In).


% throwOnFailure/1 is like Java/C's assert/1
throwOnFailure(X):-X,!.
throwOnFailure(X):-trace,not(X).

% imports sentencePos/2,bposToPos/2
%:- style_check(+singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- set_prolog_flag(double_quotes, string).
:- set_prolog_flag(double_quotes,string).
:-use_module(cyc).
:-setCycOption(query(time),2).
:-setCycOption(query(time),2).
:- trace,cyc:startCycAPIServer.

:-multi_transparent(deftest_p2c/2).


% imports sentencePos/2,bposToPos/2
:- style_check(-singleton).
:- style_check(-discontiguous).
:- style_check(-atom).
:- set_prolog_flag(double_quotes, string).
:-set_prolog_flag(double_quotes,string).
:-multifile(user:currentParsingPhrase/1).
:-multifile(user:recentParses/1).
:-dynamic(user:recentParses/1).
:-dynamic(user:currentParsingPhrase/1).
:-(ensure_loaded('freq.pdat.txt')).
:-(ensure_loaded('colloc.pdat.txt')).
:-(ensure_loaded('pt-grammar.prul2.txt')).
:-(ensure_loaded('BRN_WSJ_LEXICON.txt')).
:-(consult(textCached)).
:-(consult(recentParses)).
:-(ensure_loaded(logicmoo_nl_pos)).
   
   
:-multi_transparent(holds/1).
:-multi_transparent(holds/2).
:-multi_transparent(holds/3).
:-multi_transparent(holds/4).
:-multi_transparent(holds/5).
:-multi_transparent(holds/6).
:-multi_transparent(holds/7).
:-multi_transparent(holds/8).

:-ensure_loaded(owl_parser).

todo(X):-notrace(noteFmt('~q',todo(X))).


printAndThrow(F,A):-sformat(S,F,A),write(user,S),nl(user),flush_output(user),trace,throw(error(representation_error(S),context(printAndThrow/2, S))).

throwOnFailure(A,_B):-call(A),!.
throwOnFailure(A,B):-printAndThrow('~n ;; Must ~q when ~q ~n',[A,B]).

throwOnFailure(A):-catch(A,E,(printAndThrow('~n ;; ~q Must ~q ~n',[E,A]),!,fail)),!.
throwOnFailure(A):-printAndThrow('~n ;; Must ~q ~n',[A]).

% ===================================================================
% Caches
% ===================================================================

:-dynamic(recentParses/1).
:-dynamic(textCached/2).
saveCaches:-tell(textCached),listing(textCached),told,tell(recentParses),listing(recentParses),told.
:-at_halt(saveCaches).

:-use_module(library(make)),redefine_system_predicate(make:list_undefined).
:-abolish(make:list_undefined/0).
make:list_undefined.

:-abolish(make:list_undefined/1).
make:list_undefined(_).

:-abolish(make:list_undefined_/2).
make:list_undefined_(_Informational, _Local).

:-abolish(list_undefined_/2).
list_undefined_(_Informational, _Local).


%:-module_transparent(processRequestHook/1).
%:-multifile(processRequestHook/1).
%:-dynamic(processRequestHook/1).

cmember(H,T):-!,member(H,T).
cmember(H,T):-trace,nonvar(T),T=[H|_].
cmember(H,[_|T]):-nonvar(T),cmember(H,T).

processRequestHook(ARGS):- member(file='english.moo',ARGS),!, 
      ignore(cmember(english=English,ARGS)),
      ignore(English=''),
      cyc:writeHTMLStdHeader('English Parser'),
      fmt('
      <form method="GET">
	<p><textarea rows="2" name="english" cols="80">~w</textarea><br>
             <input type="submit" value="Parse Normally" name="submit">&nbsp;<input type="submit" value="Parse with idiomatics" name="submit">
                <input type="checkbox" value="CHECKED" name="findall">&nbspShow All<br>
        </p>
       </form>
      <pre>Please wait..
<textarea rows="10" name="debug" cols="80" wrap=off>',[English]),
      flush_output,
      ignore(once(notrace(e2c(English,CycL)))),
      fmt('
      </textarea><br>CycL/KIF<br>
<textarea rows="15" name="cycl" cols="80" wrap=off>',[]),
      flush_output,
      once(writeCycL(CycL)),
%      notrace(once((cmember(findall='CHECKED',ARGS),ignore(catch(dotimes(34,(e2c(W),nl,nl)),E,writeq(E)))) ; ignore(once(catch(dotimes(1,(e2c(W),nl,nl)),E,writeq(E)))))),
      flush_output,
      fmt('
      </textarea> 
      ..Done
      </pre>'),
      listRecentParses,
      flush_output,
      cyc:writeHTMLStdFooter, flush_output,!.


dotimes(N,Goal):- flag('$dotimes',_,N-1),
      Goal,flag('$dotimes',D,D-1),D<1.
         
listRecentParses:-recentParses(Eng),once(listRecentParses(Eng)),fail.
listRecentParses.

listRecentParses(Eng):-
      concat_atom(Eng,' ',EngA),
      fmt('<a href="english.moo?english=~w">~w</a><br>',[EngA,EngA]),nl.

% ===================================================================
% Cyc Database normalizer
% ===================================================================
:-use_module((cyc)).  
%cycQueryV(Vars,CycL):-free_variables(Vars,Free),cyc:cycQueryReal(CycL,'EverythingPSC',Free,Backchains,Number,Time,Depth).
cycQueryA(CycL):-cycQuery(CycL).


/*
assertion(Result):-assertion(_,_PRED,_,_MT,[Pred|ARGS],_TRUE,_FORWARD,_DEFAULT,_NIL1,_NIL2,_ASSERTEDTRUEDEF,_DATE,_NIL3),
         once(assertRefactor(Pred,ARGS,Result)).
*/

assertRefactor(implies,[ARG1,ARG2],Result):-litRefactor([implies,ARG1,ARG2],[implies,LARG1,[Pred|LARG2]]),Result=..[holds,pl_implied,Pred,LARG2,LARG1],!.
assertRefactor(Pred,R1,Out):-litRefactor(R1,R2),Out=..[holds,Pred|R2].

litRefactor(Subj,Subj):- (var(Subj);atom(Subj);number(Subj)),!.
litRefactor([H|T],[HH|TT]):-litRefactor(H,HH),litRefactor(T,TT),!.
litRefactor(var(B,_),B):-!.
litRefactor(B,string(A)):-string(B),string_to_atom(B,S),catch(getWordTokens(S,A),_E,A=B),!.
litRefactor(B,A):-compound(B),B=..BL,litRefactor(BL,AL),A=..AL,!.
litRefactor(B,B).

tas:-tell(a),ignore((assertion(R),fmt('~q.~n',[R]),fail)),told.



:-ensure_loaded(el_holds).
:-retractall(holds(X,Y,zzzzzzzzzzzzzzzzz)).
:-retractall(holds(X,zzzzzzzzzzzzzzzzz,Y)).



writeCycL(CycL):-
      once((toCycApiExpression(CycL,CycL9),
      indentNls(CycL9,CycLOutS),
      fmt('~w~n',[CycLOutS]),flush_output)),!.


atom_junct(X,Y):-concat_atom(Y,' ',X).


indentNls(X,S):-string_to_list(X,M),indentNls(0,M,Y),string_to_list(S,Y).


nSpaces(N,[]):-N<1,!.
nSpaces(N,[32|More]):-NN is N-1,nSpaces(NN,More),!.

indentNls(_,[],[]).
indentNls(N,[40|More],CycLOutS):-NN is N+2,indentNls(NN,More,CycL9),nSpaces(N,Spaces),append([10|Spaces],[40|CycL9],CycLOutS).
indentNls(N,[41|More],[41|CycLOutS]):-NN is N-2,indentNls(NN,More,CycLOutS).
indentNls(N,[X|More],[X|CycLOutS]):-indentNls(N,More,CycLOutS).

noteFmt(F,A):-
        dmsg(F,A),!,
        %% next two lines if writing to a file will write some debug
        current_output(X),!,
        (stream_property(X,alias(user_output)) -> true ; (write(X,' %% DEBUG '), fmt(X,F,A))),!.

      
sentenceUnbreaker([],[]).
sentenceUnbreaker([[W|Ws]|Tagged],[R|ForLGP]):-concat_atom([W|Ws],'_',R),sentenceUnbreaker(Tagged,ForLGP).
sentenceUnbreaker([W|Tagged],[R|ForLGP]):-concat_atom([W],'_',R),sentenceUnbreaker(Tagged,ForLGP).

%getWordTokens("this is English",_Eng).
% string_to_atom(S,'this is English'),getWordTokens(S,E).
%getWordTokens('this is English',Eng).

% ===================================================================
% Semantic Interpretation
%   when using utterance we need to pass 3 arguments, 
%   the first will match CycL in the head of the DGC clause
%   the second is the list containing the words in the utterance
%   the third is the empty list.
% ===================================================================
/*
?- e2c("The cat in the hat sees a bat").

(thereExists ?cat65 
       (and (isa ?cat65 (OneOfFn Cat DomesticCat ) ) 
         (and (and (isa ?sees3 Event ) (thereExists ?bat26 
         (and (isa ?bat26 (OneOfFn BaseballBat BaseballSwing Bat-Mammal ) ) 
         (or (awareOf ?cat65 ?bat26 ) (and (isa ?sees3 VisualPerception ) 
         (performedBy ?sees3 ?cat65 ) (perceivedThings ?sees3 ?bat26 ) ) ) ) ) ) 
         (thereExists ?hat27 (and (isa ?hat27 Hat ) (in-UnderspecifiedContainer ?cat65 ?hat27 ) ) ) ) ) )

*/

e2c([]):-!.
e2c(''):-!.
e2c(English):-
      e2c(English,CycL),
      notrace(writeCycL(CycL)),
      notrace(fmt('~n For sentence: "~w".~n~n', [English])).

e2c(English,CycL9):-throwOnFailure(nonvar(English)),throwOnFailure(var(CycL9)),
     notrace((noteFmt('~nEnglish: ~q.~n~n', [English]))),
     notrace(throwOnFailure(getWordTokens(English,Eng))),
     notrace(asserta_if_new(recentParses(Eng))),
     e2c(Event,Eng,CycL9).

e2c(Event,[],[]):-!.
e2c(Event,English,CycL9):-throwOnFailure(nonvar(English)),throwOnFailure(var(CycL9)),
     %notrace(noteFmt('~nEng: ~q.~n~n', [English])),
     %concat_atom(English,' ',Text),
     fmt('~n?- e2c(~q).~n~n', [English]),flush_output,
      notrace(sentenceTagger(English,IsTagged)),
      linkParse(English,Links,Tree),writeCycL(Links),
      addLinkTags(IsTagged,Links,Tagged),
      notrace(dumpList(Tagged)),!,
      % time(ignore(catch(e2c(Event,English,Tagged,CycL9),E,(writeCycL(E),ignore(CycL9=error(E)))))),
      e2c(Event,English,Tagged,CycL9),
      flush_output,
      ignore(CycL9=not_parsed(Tagged)),
      saveCaches.


p2c(English,DCG):-atom(DCG),!,
   atom_to_pred(DCG,DWIM),p2c(English,DWIM).

p2c(English,DCG):-throwOnFailure(nonvar(English)),throwOnFailure(nonvar(DCG)),
     notrace(noteFmt('~nEng: ~q.~n~n', [English])),
      notrace(fmt('~n?- ~q.~n~n', [p2c(English,DCG)])),flush_output,
      notrace(sentenceTagger(English,IsTagged)),
      linkParse(English,Links,Tree),writeCycL(Links),
      saveCaches,
      addLinkTags(IsTagged,Links,Tagged),
      notrace(dumpList(Tagged)),!,
      phrase(DCG,Tagged,[]),
      notrace(writeCycL(DCG)),
      notrace(fmt('~n For sentence: "~w".~n~n', [English])).
            
atom_to_pred(DCG,DWIM2):-atom_to_pred0(DCG,DWIM),functor(DWIM,F,A),A2 is A-2,functor(DWIM2,F,A2).
atom_to_pred0(DCG,DWIM):-predicate_property(DCG,number_of_clauses(_)),!,DCG=DWIM.
atom_to_pred0(DCG,DCG):-functor(DCG,_,10),!.
atom_to_pred0(DCG,DWIM):-DCG=..[D|CG],append([D|CG],[_],NEW),NEWC=..NEW,!,atom_to_pred0(NEWC,DWIM).



addLinkTags(Pre,['S'|Links],Post):-!,addLinkTags(Pre,Links,Post).
%addLinkTags(Pre,Links,Post):-openLists(Pre,Post),!.
addLinkTags(Pre,Links,Post):-Post=Pre,!.
%%addLinkTags(Pre,['NP'|Links],Post):-getWordSegment(Links,Pre,Segs),addToSegs().

e2c(Event,English,Tagged,CycL):- englishCtx(Event,CycL,Tagged,[]).
%e2c(Event,English,Tagged,CycL):- chunkParseCycL(Event,Subj,Tagged,CycL).
%e2c(Event,English,Tagged,no_e2c(English)):-!.
e2c(Event,English,Tagged,no_e2c(List)):-dcgMapCar(Var,theWord(Var),List,Tagged,[]).
%e2c(Event,English,Tagged,no_e2c(List)):-dcgMapCar(Var,theWord(Var),List,Tagged,[]).
e2c(Event,English,Tagged,no_e2c(['NLPatternList'|List])):-dcgMapCar('NLPattern-Word'(Var,POS),dcgAnd(theCycPOS(POS),theWord(Var)),List,Tagged,[]).

dcgWordList(List)-->dcgMapCar(Var,theWord(Var),List2),{!,List=List2}.
dcgTextList(List)-->dcgMapCar(Var,theGText(Var),List2),{!,List=List2}.

dumpList([]):-nl.
dumpList([B|Tagged]):-dumpList1(B),dumpList(Tagged),!.
dumpList1(Tagged):-writeq(Tagged),nl,flush_output.

%:-      setCycOption(cycServer,'10.1.1.3':13701).

cyclifyTest(String,cyc(Result)):-cyc:evalSubL('cyclify'(string(String)),Result).
%cyclifyTest(String,'cyc-assert'(Result)):-cyc:sublTransaction('10.1.1.3':13701,'parse-a-sentence-completely'(string(String),'#$RKFParsingMt'),Result).
%cyclifyTest(String,'cyc-query'(Result)):-cyc:sublTransaction('10.1.1.3':13701,'parse-a-question-completely'(string(String),'#$RKFParsingMt'),Result).


firstNth1(_Ns,[],[]).
firstNth1(Ns,[H|Src],[H|Res]):-Ns>0,N2 is Ns-1,firstNth1(N2,Src,Res).
firstNth1(Ns,Src,[]).


lowerOnly([],[]):-!.
lowerOnly([[S|H]|T],[[S|H]|TT]):-atom(H),name(H,[A1|_]), is_lower(A1),lowerOnly(T,TT).
lowerOnly([[S|H]|T],TT):-lowerOnly(T,TT).


%sameString(CYCSTRING,String):-var(String),var(CYCSTRING),!,CYCSTRING=String.
%sameString(CYCSTRING,String):-var(CYCSTRING),!,CYCSTRING=String.

reformatStrings(X,X):- (var(X);number(X)),!.
reformatStrings(string(X),S):-string(X),string_to_atom(X,A),reformatStrings(string(A),S).
reformatStrings(string(A),string(S)):-atom(A),!,concat_atom(S,' ',A).
reformatStrings(X,string(S)):-string(X),string_to_atom(X,S),!.
reformatStrings([],[]):-!.
reformatStrings([H|T],[HH|TT]):-!,reformatStrings(H,HH),reformatStrings(T,TT),!.
reformatStrings(X,P):-compound(X),X=..LIST,reformatStrings(LIST,DL),P=..DL,!.
reformatStrings(X,X):-not(atom(X)),!.
reformatStrings(B,A):-atom_concat('#$',A,B),!.
reformatStrings(B,B):-!.

:-setCycOption(query(time),2).



atom_to_worldlist(string(S),U):-!,atom_to_worldlist(S,U),!.
atom_to_worldlist(S,U):-atom(S),atom_concat('"',Right,S),atom_concat(New,'"',Right),!,atom_to_worldlist(New,U),!.
atom_to_worldlist(S,U):-string(S),!,string_to_atom(S,A),!,atom_to_worldlist(A,U),!.
atom_to_worldlist(S,U):-atom(S),concat_atom(U,' ',S).


harvestDenotes(Words):-once((evalSubL('denotation-mapper'(string(Words),quote(['#$middleName','#$alias','#$initialismString',
        '#$middleNameInitial','#$nicknames','#$givenNames','#$firstName','#$abbreviationString-PN']),':greedy'),List,_),
         learnDenotationLists(List))).
learnDenotationLists([]).
learnDenotationLists([string(H)|T]):-learnDenotation([string(H)|T]).
learnDenotationLists([H|T]):-learnDenotation(H),learnDenotationLists(T).
learnDenotation([string(S)|Term]):-string_to_atom(S,Atom),atom_to_worldlist(Atom,[LI|ST]),(ST=[] -> true;
      (saveList(LI,[[multiwordText,[LI|ST],Term]]))).
learnDenotation(H):-trace.




% =======================================================
% Text to word info
% =======================================================

harvestCycConstants(DM,DM):-number(DM),!,fail.

harvestCycConstants(Words,DM):-evalSubL(mapcar('#\'cdr','denotation-mapper'(string(Words),quote(['#$middleName','#$alias','#$initialismString',
        '#$middleNameInitial','#$nicknames','#$givenNames','#$firstName','#$abbreviationString-PN']),':greedy')),List,_),leastOne(List),!,
         reformatStrings(List,DM),!.
harvestCycConstants(Words,DM):-evalSubL(mapcar('#\'cdr','denotation-mapper'(string(Words),quote(['#$middleName','#$alias','#$initialismString','#$middleNameInitial','#$nicknames','#$givenNames','#$firstName','#$abbreviationString-PN']),':diligent')),List,_),!,reformatStrings(List,DM),!.


learnTexts([]).
learnTexts([A|B]):-learnText(A),learnTexts(B).

learnText(Atom):-atom(Atom),concat_atom(List,'_',Atom),!,learnText(List).
learnText(X):-textCached(X,[txt|X]),!.
learnText(W):-
      saveText(W,[txt|W]),
      cyc:fmt(learnText(W)),
      ignore((harvestCycConstants(W,CycL),collectionInfo(CycL,COLINFO),saveList(W,COLINFO))),
      ignore(makePosInfo(W)),
      %denotationInfo(POSINFO,DENOTESINFO),
      saveTemplatesForString(W),
      ignore((mwStringsForString(W,MWStrings),saveList(W,MWStrings))),
      saveCaches.
learnText(W):-!. %true.

saveList(String,[]):-!.
saveList(String,[A|List]):-saveText(String,A),saveList(String,List).

saveText(String,A):-asserta_if_new(textCached(String,A)).

appendLists([List],List):-!.
appendLists([L|List],Res):-appendLists(List,AL),append(L,AL,Res),!.
      
%(#$and (#$speechPartPreds ?Pos ?Pred)(?Pred ?Word ?STRING)
asserta_if_new(X):-retractall(X),asserta(X),!.
asserta_if_new(X):-catch(X,_,fail),!.
asserta_if_new(X):-assertz(X),!.

denotationInfo([],[]).
denotationInfo([COL|INFO],[[COL|COLINFO]|MORE]):-
      denotationPOS(COL,COLINFO),
      denotationInfo(INFO,MORE).

mwStringsForString([N],[]):-number(N),!.
mwStringsForString(N,[]):-number(N),!.
mwStringsForString(String,[]):-!.
mwStringsForString(String,List):-
   findall([mwsem,PreText,Word,Pos,THING],
        cyc:cycQueryReal(thereExists(Pred,
        and(wordForms(Word,Pred,string(String)),speechPartPreds(Pos,Pred),
        multiWordString(PreText,Word,Pos,THING))),'#$EnglishMt',[PreText,Word,Pos,THING],3,'NIL',10,30),List).

wordTerms(Word,List):-
   findall([wformulas,Word,Forms],
        cyc:cycQueryReal(termFormulas(Word,Forms),'#$EverythingPSC',[Word,Forms],3,'NIL',10,30),List).
 

saveTemplatesForString(String):-
      wordForString(String,Word,Pos),
      saveTemplatesForWord(Word),fail.
saveTemplatesForString(String):-!.


saveTemplatesForWord(Word):-var(Word),!,trace,fail.
saveTemplatesForWord(Word):- textCached(Word,completeSemTrans),!.
saveTemplatesForWord(Word):-
      findall([frame,Word,Pos,FRAME,SimpleFrame,Pred],
                      (cycQueryV([Word,Pos,FRAME,CYCL,Pred,PreRequ],
                          wordSemTrans(Word,_Num,FRAME,CYCL,Pos,Pred,PreRequ)),
                          templateConstaint(PreRequ,CYCL,SimpleFrame)),List),saveList(Word,List),fail.

saveTemplatesForWord(Word):-findall([frame,Word,Pos,FRAME,CYCL,Pred],
        cycQueryV([Word,Pos,FRAME,CYCL,Pred],and(isa(Pred,'SemTransPred'),
                semTransPredForPOS(Pos,Pred),arity(Pred,4),[Pred,Word,_Num,FRAME,CYCL])),List),once(saveList(Word,List)),fail.

saveTemplatesForWord(Word):-findall([frame,Word,'Verb',FRAME,CYCL,verbSemTrans],cycQueryV([Word,FRAME,CYCL],verbSemTrans(Word,_Num,FRAME,CYCL)),List),once(saveList(Word,List)),fail.
saveTemplatesForWord(Word):-findall([frame,Word,'Adjective',FRAME,CYCL,adjSemTrans],cycQueryV([Word,FRAME,CYCL],adjSemTrans(Word,_Num,FRAME,CYCL)),List),once(saveList(Word,List)),fail.


saveTemplatesForWord(Word):-
      findall([swframe,PreText,Word,FRAME,CYCL,Pos],cycQuery(multiWordSemTrans(PreText,Word,Pos,FRAME,CYCL)),List),saveList(Word,List),fail.

%(denotation Automobile-TheWord CountNoun 0 Automobile)
saveTemplatesForWord(Word):-
      findall([denotation,Pos,CYCL],cycQuery(denotation(Word,Pos,_Num,CYCL)),List),saveList(Word,List),fail.

saveTemplatesForWord(Word):-
      findall([wsframe,Word,PreText,FRAME,CYCL,Pos],cycQuery(compoundSemTrans(Word,PreText,Pos,FRAME,CYCL)),List),saveList(Word,List),fail.

saveTemplatesForWord(Word):-asserta(textCached(Word,completeSemTrans)).


denotationPOS([lex,COL|Pos],denote(COL)):-!.


/* in POS FILE
wordForString(String,Word,Pos):-atom(String),!,wordForString([String],Word,Pos).
wordForString(String,Word,Pos):-findall(Word,textCached(String,[lex,Word|_]),Words),sort(Words,Words9),!,cmember(Word,Words9).
*/


abbreviationForLexicalWord(S,Pos,Word):-cycQueryV([Pos,Word],abbreviationForLexicalWord(Word,Pos,string(S))).


%getPos(W,Pos,Word):-cycQuery(partOfSpeech(Word,Pos,string(W)),'GeneralEnglishMt').
%getPos(W,Pred,Word):-cycQueryV([Pred,Word],and(speechPartPreds(Pos,Pred),[Pred,Word,string(W)])).
%      sformat(S,'(remove-duplicates (ask-template \'(?Pos  ?Word) \'(#$and (#$speechPartPreds ?Pos ?Pred)(?Pred ?Word "~w")) #$EverythingPSC) #\'TREE-EQUAL)',[W]),
%      evalSubL(S,R:_).

makePosInfo(String):-getPos(String,Pos,Word),saveText(String,[lex,Word,Pos]),fail.
makePosInfo(String):-!.
         
%posInfo([W],RR):-atom_concat(NW,'\'s',W),!,wordAllInfoRestart(NW,R),append(R,[[pos,possr_s]],RR).
posInfo(String,[[lex,'UNKNOWN-Word','UNKNOWN-Pos']]):-!.

posInfo2(String,RESFLAT):-findall([Word,Pos],getPos(String,Pos,Word),RES),
         findall(Word,cmember([Word,Pos],RES),WRODS),sort(WRODS,WORDS),
         posInfo(WORDS,RES,RESFLAT),!.

posInfo([],RES,[]):-!.
posInfo([W|WORDS],RES,[[lex,W|POSESS]|RESFLAT]):-findall(Pos,cmember([_Word,Pos],RES),POSES),=(POSES,POSESS),
      posInfo(WORDS,RES,RESFLAT).


cycAllIsa([Fort],COLS):-!,cycAllIsa(Fort,COLS).
cycAllIsa(nart(Fort),COLS):-!,cycAllIsa(Fort,COLS).
cycAllIsa(Fort,COLS):-number(Fort),!,findall(Type,numberTypes(Fort,Type),COLS),!.
cycAllIsa(Fort,COLS):-copy_term(Fort,FFort),numbervars(FFort,0,_),termCyclify(Fort,CycL),
  findall(MEMBER,cycQuery('#$or'(nearestIsa(CycL,MEMBER),and(isa(CycL,MEMBER),memberOfList(MEMBER,['TheList',
  'StuffType','TemporalObjectType','QuantityType','GameTypeExceptions',
    'SpatialThing-Localized','ClarifyingCollectionType','Collection','Individual','Event',
      'TemporalStuffType','DurativeEventType','SituationPredicate','ObjectPredicate',
    'TruthFunction','BinaryRelation','UnaryRelation','TernaryRelation','ObjectPredicate','Function-Denotational',
    'CollectionType','FacetingCollectionType','Agent-Generic'])))),COLS).

%cycAllIsa(Fort,COLS):-is_list(Fort),termCyclify(Fort,CycL),cycQueryA(isa(CycL,List)),List,_),reformatStrings(List,COLS).
%cycAllIsa(Fort,COLS):-termCyclify(Fort,CycL),evalSubL('ALL-ISA'((CycL),'#$InferencePSC'),List,_),reformatStrings(List,COLS).
numberTypes(N,'Integer'):-integer(N).
numberTypes(N,'Numeral'):-integer(N),N>=0,N<10.
numberTypes(N,'RealNumber'):-not(integer(N)).
numberTypes(N,'NegativeNumber'):-N<0.
numberTypes(N,'PositiveNumber'):-N>0.
numberTypes(N,'Number-General').

collectionInfo([],[]).
collectionInfo([COL|INFO],[[denotation,Pos,COL|COLINFO]|MORE]):-
      cycAllIsa(COL,COLINFO),
      collectionInfo(INFO,MORE).

% ?- A is rationalize(0.999999999999999),rational(A).
% A = 1000799917193442 rdiv 1000799917193443



subcatFrame(Word,Pos,INT,CAT):-cycQueryA(subcatFrame(Word,Pos,INT,CAT)).

vetodenoteMapper('is','#$Israel').

linkParse(String,['S',String],['S',String]):-!,harvestDenotes(String).

linkParse(String,Fourth,P:M):-evalSubL('link-parse'(string(String)),[[P,M,_,string(FourthS)]],_),getSurfaceFromChars(FourthS,Fourth,_),!.
linkParse(A,XX,B):- linkParse0(A,B,string(Y)),concat_atom(['',S,''],'"',Y),getSurfaceFromChars(S,XX,YY).
linkParse0(String,FourthS,Fourth):-evalSubL('link-parse'(string(String)),FourthS,_),
      append(_,[string(FourthAtom)],FourthS),getSurfaceFromChars(FourthAtom,Fourth,_).
      %linkParse(Text,Info,_),!,



theSTemplate(TempType) --> dcgTemplate(TempType,_VarName,_CycL).

theVar(VarName,Subj) --> [_|_],theVar(VarName,Subj),{true}.
theVar(VarName,Subj) --> [].


tellall(X,W):- X, numbervars(X,0,_),(format('~q.~n',[W])),fail.
tellall(_X,_W):-flush_output.


argName(string(N),Name):-!,atom_concat(':ARG',N,Name).
argName(N,Name):-atom_concat(':ARG',N,Name).
makeArgNameLen(0,[]):-!.
makeArgNameLen(N,ArgsS):- N2 is N-1,makeArgNameLen(N2,Args),argName(N,Named),append(Args,[Named],ArgsS).


/*
(genFormat nearestTransitiveNeighbor "~a nearest generalization by transitivity of ~a is ~a" 
       (TheList 
           (TheList 2 :EQUALS :POSSESSIVE) 
           (TheList 1 :EQUALS) 
           (TheList 3 :EQUALS)))
*/
    %tell(pp),rehold,told.
rehold:-between(1,12,A),functor(P,holds,A),
         catch(P,_,fail),once((litRefactor(P,PP),fmt('~q.~n',[PP]))),fail.
rehold.      

%getWordTokens(string([X|Y]),[X|Y]):-atom(X),!.
%getWordTokens([X|Y],[X|Y]):-atom(X),!.
%getWordTokens(X,Y):-getCycLTokens(X,Y),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

getPredType(Pred,'Predicate',2):-holdsWithQuery(isa,Pred,'BinaryPredicate'),!.
getPredType(Pred,'Function',2):-holdsWithQuery(isa,Pred,'BinaryFunction'),!.
getPredType(Pred,'Function',A):-atom(Pred),atom_codes(Pred,[C|_]),is_upper(C),getArity(Pred,A),number(A),!.
getPredType(Pred,'Predicate',A):-atom(Pred),atom_codes(Pred,[C|_]),is_lower(C),getArity(Pred,A),number(A),!.

getArity('NPIsNP-NLSentenceFn',2):-!.
getArity(Pred,A):-holdsWithQuery(arity,Pred,A),(number(A) -> ! ; true).
getArity(Pred,Out):-holdsWithQuery(arityMin,Pred,A),(number(A) -> (! , Out is A + 0 ); Out=A).
getArity(Pred,Out):-(noteFmt('CANT get arity of ~q ~n',[Pred])),!,atom(Pred),Out=1.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
holdsWithQueryTermSearch(QUERY):-findall(QUERY,catch(cycQuery(QUERY,'EverythingPSC'),_,fail),List),leastOne(List),!,
      member(QUERY,List).
holdsWithQueryTermSearch([G|QUERY]):-ground(G),oneGround(QUERY,_),!,fail.
holdsWithQueryTermSearch([Q,U|ERY]):-oneGround([Q,U|ERY],Term),cyc:getAllTermAssertions(Term,Result),once(balanceBinding([Q,U|ERY],Result)),nonvar(U).


oneGround(List,Term):-member(Term,List),ground(Term),!.
oneGround(List,Term):-member(Term,List),nonvar(Term),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55
holdsWithQuery(Reln,DCGPre,CycLPre):-Reln == expansion,catch(cycQuery([Reln,DCGPre,CycLPre],'EverythingPSC'),_,fail).
holdsWithQuery(Reln,DCGPre,CycLPre):-holds(Reln,DCGPre,CycLPre).
holdsWithQuery(Reln,DCGPre,CycLPre):-Reln \= expansion,holdsWithQueryTermSearch([Reln,DCGPre,CycLPre]).

holdsWithQuery(Reln,TempType,DCGPre,CycLPre):-holds(Reln,TempType,DCGPre,CycLPre).
holdsWithQuery(Reln,TempType,DCGPre,CycLPre):-holdsWithQueryTermSearch([Reln,TempType,DCGPre,CycLPre]).

holdsWithQuery(Reln,TempType,Name,DCGPre,CycLPre):-holds(Reln,TempType,Name,DCGPre,CycLPre).
holdsWithQuery(Reln,TempType,Name,DCGPre,CycLPre):-holdsWithQueryTermSearch([Reln,TempType,Name,DCGPre,CycLPre]).
holdsWithQuery(Reln,TempType,Name,DCGPre,CycLPre,Test):-holds(Reln,TempType,Name,DCGPre,CycLPre,Test).
holdsWithQuery(Reln,TempType,Name,DCGPre,CycLPre,Test):-holdsWithQueryTermSearch([Reln,TempType,Name,DCGPre,CycLPre,Test]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%55

%templateMarkerRepresentation(TemplateMarker,Char):-assertion(templateMarkerRepresentation,TemplateMarker,[(Char)]).
templateMarkerRepresentation('TemplateHyphenMarker', "-").
templateMarkerRepresentation('TemplateOpenBracketMarker', "[").
templateMarkerRepresentation('TemplateCloseBracketMarker', "]").
templateMarkerRepresentation('TemplateCloseParenMarker', "]").
templateMarkerRepresentation('TemplateDoubleQuoteMarker', "'").
templateMarkerRepresentation('TemplateSingleQuoteMarker', "'").
templateMarkerRepresentation('TemplateExclamationMarkMarker', "!").
templateMarkerRepresentation('TemplateQuestionMarkMarker', "?").
templateMarkerRepresentation('TemplateCommaMarker', ",").
templateMarkerRepresentation('TemplateSemiColonMarker', ";").
templateMarkerRepresentation('TemplatePeriodMarker', ".").
templateMarkerRepresentation('TemplateColonMarker', ":").


toText([],[]):-!.
toText(S,T):-cmember([txt|T],S),!.
toText([S|SS],TTT):-toText(S,T),toText(SS,TT),flatten([T|TT],TTT).


% =======================================================
% DCG Helpers/Operators (and some Terminals)
% =======================================================

dcg_CannotMatchEmpty(Pred):-Pred =.. List, append(List,[[],_],New),
               RePRED=..New,functor(RePRED,F,A),abolish(F/A),dynamic(F/A),asserta((RePRED :- !, fail)).

%memberData(M,[W|Ws]):-textCached([W|Ws],M).
%memberData(M,[W|Ws]):-textCached([_,W|Ws],M),!.
%memberData(M,[W|Ws]):-textCached([W|_],M),!.
%memberData(M,W):-true,concat_atom(WList,'_',W),!,memberData(M,WList).


% Text
:-dcg_CannotMatchEmpty(theText(S)).
theText([S]) --> theGText(S),{!}.
theText([S|Text]) --> {nonvar(Text)}, [Data],{cmember([txt|[S|Text]],Data),!}.
theText([S|Text]) --> {nonvar(Text),!}, [Data],{cmember([txt,S],Data)},theText(Text).
theText(S) --> {atom(S),atom_concat('"',Right,S),atom_concat(New,'"',Right),!},theText(New).
theText(S) --> {atom(S),concat_atom([W1,W2|List],' ',S),!},theText([W1,W2|List]).
theText(S) --> theGText(S).

% Looser text test?
:-dcg_CannotMatchEmpty(theGText(S)).
theGText(S) --> {!}, [Data],{member([txt,S],Data)}.

:-dcg_CannotMatchEmpty(theTerm(S)).
theTerm(Term) --> theText([S|TEXT]),{textCached([S|TEXT],[denotation,_Pos,Term|_])}.

% bposToPos/2 is from [cyc_pos].
%thePOS(Pos) --> [Data],{once((memberchk([tag,_|POSs],Data),cmember(CPos,POSs),(Pos=CPos;bposToPos(CPos,Pos))))}. 

:-dcg_CannotMatchEmpty(thePOS(S)).
thePOS(Pos) --> [Data],{notrace(isTag(Data,Pos))/* ,! */}.
%thePOS(Pos) --> {'NLSentence'==Pos},dcgOneOrMore(dcgAny).

theCycPOS(Cyc) -->  [Data],{notrace(isCycTag(Data,Cyc))/* ,! */}.
theCycPOS(Cyc) -->  [Data],{notrace((isTag(Data,Pos),bposToCPos(Pos,Cyc)))/* ,! */}.

:-dcg_CannotMatchEmpty(thePOS(_,_)).
thePOS(Word,Pos) --> dcgAnd(thePOS(Pos),theWord(Word)).

:-dcg_CannotMatchEmpty(theWord(_)).
theWord(Word) --> theText([S|Text]),{once(textCached([S|Text],[lex,Word|_])),!}.
theWord('WordFn'(string([S|Text]))) --> theText([S|Text]),{!}.
%theWord(Word) --> theText([A]),{once(memberData([lex,Word|_],[A,B]))},theText([B]).

:-dcg_CannotMatchEmpty(theWord(_,_)).
theWord(_,_,[],_):- !,fail.
theWord(Word,Pos) --> {var(Word),!},dcgAnd(thePOS(Pos),theWord(Word)).
theWord(Word,Pos) --> dcgAnd(theWord(Word),thePOS(Pos)).

:-dcg_CannotMatchEmpty(theForm(_)).
theForm(PosForm)--> theText([S|Text]),{(textCached([S|Text],[lex,Word,PosForm|_]))}.

dcgPosVar(Pos,VarName,Subj)-->dcgAnd(thePOS(Pos),theVar(VarName,Subj)).

theName(Var,S,_) :-getText(S,Text),suggestVar(Text,Var),!.

theGData(Data) --> [Data].
theData(Data,Data,[]).

theTense(Time) --> theForm(PosForm),{once(timeLookup(PosForm,Time))}.
thePerson(Pers) --> theForm(PosForm),{once(personLookup(PosForm,Pers))}.
theCount(Num)  --> theForm(PosForm),{once(countLookup(PosForm,Num))}.
theAgreement(Pers,Num) --> theForm(When),{personLookup(When,Pers),countLookup(When,Num)}.

timeLookup(Atom,'Now'):-atom(Atom),atom_concat(_,'Present',Atom).
timeLookup(Atom,'Past'):-atom(Atom),atom_concat(_,'Past',Atom).
timeLookup(Atom,'Future'):-atom(Atom),atom_concat(_,'Future',Atom).
timeLookup(Atom,'Now'):-atom(Atom),atom_concat('present',_,Atom).
timeLookup(Atom,'Past'):-atom(Atom),atom_concat('past',_,Atom).
timeLookup(Atom,'Future'):-atom(Atom),atom_concat('future',_,Atom).

countLookup(Atom,'Plural'):-atom(Atom),atom_concat('plural',_,Atom).
countLookup(Atom,'Plural'):-atom(Atom),atom_concat(_,'Plural',Atom).
countLookup(Atom,'Plural'):-atom(Atom),concat_atom([_,'nonSing',_],Atom).
countLookup(Atom,'Singular'):-atom(Atom),atom_concat('singular',_,Atom).
countLookup(Atom,'Singular'):-atom(Atom),atom_concat(_,'Singular',Atom).
countLookup(Atom,'Singular'):-atom(Atom),concat_atom([_,'Sg',_],Atom).

personLookup(Atom,'First'):-atom(Atom),atom_concat('first',_,Atom).
personLookup(Atom,'Second'):-atom(Atom),atom_concat('second',_,Atom).
personLookup(Atom,'Third'):-atom(Atom),atom_concat('third',_,Atom).


posGenls(Pos1,Pos):-Pos1=Pos,!.

% =======================================================
% The CycL Frames
% =======================================================
% findall(Y,p2c("go to it",verb_phrase(A,B,C,X,Y)),L),length(L,S). => 122

%textCached('Lead-TheWord', [frame, 'Lead-TheWord', 'Verb', 'TransitiveNPFrame', and(isa(':ACTION', 'GuidingAMovingObject'), directingAgent(':ACTION', ':SUBJECT'), primaryObjectMoving(':ACTION', ':OBJECT')), verbSemTrans]).

:-dcg_CannotMatchEmpty(theFrame(Event,FrameType,Pos,Template)).
%theFrame(Event,FrameType,Pos,Template)-->theFrame0(Event,FrameType,Pos,Template).

theFrame(Event,FrameType,Pos,Template,S,E) :- %throwOnFailure(nonvar(FrameType)), 
      (var(Pos)->(theCycPOS(Pos,S,_));(true)),
      findall(E-frame(Event,FrameType,Pos,Template),(posGenls(Pos1,Pos),theFrame0(Event,FrameType,Pos1,Template,S,E)),TemplateList),!,leastOne(TemplateList),
      sort(TemplateList,TemplateListS),
      %length(TemplateList,L0),length(TemplateListS,L1),!,(L0 == L1 ->( trace ,writeq(frame(Template,Event,FrameType,Pos))); true),
      member(E-frame(Event,FrameType,Pos,Template),TemplateListS).

theFrame0(Event,FrameType,Pos,Template) --> theWord(Word),{getFrame(Word,Event,FrameType,Pos,Template),once(suggestVar(SText,Event))}.

%  getFrame(Word,Event,'ParticleCompFrameFn'('IntransitiveParticleFrameType',Prep),'Verb',Template).
getFrame(Word,Event,FrameType,Pos,Template):-ground(FrameType),!,findall(CycL,(textCached(_, [frame,Word,Pos,FrameType,CycL,Pred])),Some),Some=[Lest|One],joinCols(or,[Lest|One],Template).
getFrame(Word,Event,FrameType,Pos,CycL):-textCached(_, [frame,Word,Pos,FrameType,CycL,Pred|_]).
getFrame(Word,Event,FrameType,Pos,Simply):- cycQuery(wordSemTrans(Word,Num,FrameType,CycL,Pos,Pred,Precons)),templateConstaint(Precons,CycL,Simply).
getFrame(Word,Event,'denotation',Pos,isa(':TERM',Term)):- /*throwOnFailure(nonvar(Pos)),*/ textCached(Word, [denotation, Pos,Term ]). %,trace.

theFrame0(Event,FrameType,Pos,Template,START,E):-throwOnFailure(nonvar(Pos)),
   theWord(TheWord,START,ART),
   textCached(TheWord, [wsframe, TheWord, TheList, FrameType, Template,Pos]),
   once(theListTemplate(TheList,TheListTemplate)),
   phrase(dcgSeq(TheListTemplate),ART,E).

theListTemplate([],[]):-!.
theListTemplate([string(S)|List],[theText(SA)|Template]):-
      atom_to_worldlist(S,[SA]),
      theListTemplate(The,TheList),theListTemplate(List,Template).
theListTemplate(TheList,TheListTemplate):-TheList =.. ['TheList'|List],!,theListTemplate(List,TheListTemplate).
theListTemplate(A,A):-trace.

templateConstaint('True',Template,Template):-!.
templateConstaint(Constraints,Template,implies(Constraints,Template)).

framePredForPos(Pos,Pred):-holdsWithQuery(semTransPredForPOS,Pos,Pred).
%framePredForPos('Preposition','prepReln-Action').
%framePredForPos('Preposition','prepReln-Object').
framePredForPos('Adjective','adjSemTrans-Restricted').

frameAdd:-cycQuery('adjSemTrans-Restricted'(WORD,NUM,FRAME,COL,CYCL),'#$EverythingPSC'),cycAssert(wordSemTrans(WORD,'Adjective',NUM,FRAME,implies(isa(':NOUN',COL),CYCL),'adjSemTrans-Restricted',_PreRequ),'#$EnglishMt'),fail.
frameAdd:-cycQuery('nounPrep'(WORD,PREP,CYCL),'#$EverythingPSC'),cycAssert(wordSemTrans(WORD,_Num,'PPCompFrameFn'('TransitivePPFrameType',PREP),CYCL,'Noun','nounPrep',_PreRequ),'#$EnglishMt'),fail.
%frameAdd:-cycQuery('prepReln-Action'(ACTION,OBJECT,WORD,CYCL),'#$EverythingPSC'),cycAssert(wordSemTrans(WORD,_Num,'Post-VerbPhraseModifyingFrame',implies(and(isa(':ACTION',ACTION),isa(':OBLIQUE-OBJECT',OBJECT)),CYCL),'Preposition','prepReln-Action',_PreRequ),'#$EnglishMt'),fail.
%frameAdd:-cycQuery('prepReln-Object'(NOUN,OBJECT,WORD,CYCL),'#$EverythingPSC'),cycAssert(wordSemTrans(WORD,_Num,'Post-NounPhraseModifyingFrame',implies(and(isa(':NOUN',NOUN),isa(':OBLIQUE-OBJECT',OBJECT)),CYCL),'Preposition','prepReln-Object',_PreRequ),'#$EnglishMt'),fail.

frameAdd(WORDS):-cycQuery('compoundSemTrans'(WORDS,WLIST,POS,FRAME,CYCL),'#$EverythingPSC'),
         stringToWords(WLIST,WORDS).

stringToWords([],[]).
stringToWords(TheList,Words):-functor(TheList,'TheList',_),TheList=..[_|List],!,stringToWords(List,Words).
stringToWords(string(S),Words):-!,stringToWords(S,Words).
stringToWords(['TheList'|List],Words):-!,stringToWords(List,Words).
stringToWords([S|Tring],[W|Words]):-stringToWord(S,W),stringToWords(Tring,Words).

stringToWord([S],W):-!,textCached([S],[lex,W|_]).
stringToWord(S,W):-textCached([S],[lex,W|_]).

% =======================================================
% Variable naming
% =======================================================

convertToAtomsList([],[]).
convertToAtomsList(['$'|ORDS],['DOLLARS-'|WORDS]):-convertToAtomsList(ORDS,WORDS).
convertToAtomsList([':'|ORDS],['KW-'|WORDS]):-convertToAtomsList(ORDS,WORDS).
convertToAtomsList(['-'|ORDS],WORDS):-convertToAtomsList(ORDS,WORDS).
convertToAtomsList(['HypothesizedWordSenseFn'|ORDS],WORDS):-convertToAtomsList(ORDS,WORDS).
convertToAtomsList(['nart'|ORDS],WORDS):-convertToAtomsList(ORDS,WORDS).
convertToAtomsList(['OneOfFn'|ORDS],WORDS):-convertToAtomsList(ORDS,WORDS).
convertToAtomsList([W|ORDS],[W,'-'|WORDS]):-not(compound(W)),convertToAtomsList(ORDS,WORDS).
convertToAtomsList([[ST|UFF]|ORDS],WORDS):-append([ST|UFF],ORDS,DO),convertToAtomsList(DO,WORDS).
convertToAtomsList([W|ORDS],WORDS):-W=..STUFF,append(STUFF,ORDS,DO),convertToAtomsList(DO,WORDS).

suggestVar(Subj,Subj2):-nonvar(Subj2),!.
suggestVar(Subj,Subj2):-var(Subj),!.
suggestVar([],Subj):-!.%suggestVar([A],Subj),!.
suggestVar([W|ORDS],Subj):-!,ignore(notrace(once((nonvar(ORDS),convertToAtomsList([W|ORDS],WORDS),
   concat_atom(['?'|WORDS],'',Suj),gensym(Suj,SubjSl),cyc:toUppercase(SubjSl,SubjS),ignore(SubjS=Subj))))),!.
suggestVar(A,Subj):-suggestVar([A],Subj),!.
suggestVar(Subj,Subj):-!.
:-trace(suggestVar,-all).

suggestVarI([W|ORDS],Subj):-!,concat_atom(['?'|[W|ORDS]],'',Suj),cyc:toUppercase(Suj,SubjS),ignore(SubjS=Subj),!.
suggestVarI(A,Subj):-suggestVarI([A],Subj).

% =======================================================
% TERM ISA 
% =======================================================

theIsa(Subj,COLS,ColType) --> theOneOfFn(Subj,COLS,ColType).

theOneOfFn(Subj,COLS,ColType)-->theWord(Word), {%suggestVar(SText,Subj),
    meetsRequirement(Word,ColType,COLSS),joinCols('OneOfFn',COLSS,COLS)}.

meetsRequirement(Word,ColType,COLSS):- 
       notrace(findall(COL,(textCached(Word,[denotation,_Pos,COL|DPROPS]),anyIsa([COL|DPROPS],ColType)),COLSS)),!,leastOne(COLSS).

leastOne([CO|LSS]).

anyIsa([COL|DPROPS],ColType):-member(One,[COL|DPROPS]),cycQuery(isa(One,ColType)),!.

%e2c("the singer sang a song").
        
joinCols(JOINER,[CO|LS1],COLS):-list_to_set([CO|LS1],COLS1S), ([COLS]=COLS1S -> true; COLS=nart([JOINER|COLS1S])),!.

%constraintsOnIsa([_,_,[EQ|ColTypes]|_],EQ,nart(['CollectionIntersectionFn'|ColTypes])).
%constraintsOnIsa([_,_,[EQ]|_],EQ,'Thing').


:-flag(dcg_template_depth,_,0).
:-dynamic(current_dcg_template/2).
% current_dcg_template(?Template,?Depth)


textMathesTemplate(T,TempType):-not(atom(TempType)),!,fail.
textMathesTemplate(T,TempType):- cmember(T,['X','Y','Z']).
textMathesTemplate(Txt,TempType):- atom(Txt),concat_atom([_|_],Txt,TempType).

disable_dcgTemplate(TempType,VarName,cycForTemplate(VarName,TempType,TempType1),[TempTypeData|E],E):-
     getText(TempTypeData,[TempType1]),textMathesTemplate(TempType1,TempType),!.

dcgTemplate(TempType,VarName,CycL,S,E):- TempType == 'AnyTemplate',!, dcgTemplate('NPTemplate',VarName,CycL,S,E).
dcgTemplate(TempType,VarName,CycL,S,E):-
            (current_dcg_template(TempType,Prev)->(fail);true),
            flag(dcg_template_depth,D,D+1),
            asserta(current_dcg_template(TempType,D)),
            (D<40 -> true ; fail),
            %startsTemplateType(S,TempType),
            appliedTemplate(TempType,_Pred,DCG,CycL),
            throwOnFailure(callable(DCG)),
            phrase(DCG,S,E),
            flag(dcg_template_depth,DN,DN-1),
            retractall(current_dcg_template(TempType,D)).



dcgNote(X)--> dcgConsumeData(True),{noteFmt('dcgNote(~q) for ~q )',[X,True]), !}.

dcgTemplateKW(KWs,VarName,Subj)-->{cmember(TempType,KWs)},dcgTemplate(TempType,VarName,Subj).

dcgDressedString(X)-->dcgDressed(X).
dcgDressed(X)-->{throwOnFailure(nonvar(X)->callable(X);true)},X.

startsTemplateType([A|B],VPTemplate,Type):-cmember([lex,_|POSVerb],A),!,cmember(Verb,POSVerb),holdsWithQuery(posForTemplateCategory,Verb,VPTemplate).
startsTemplateType([A|B],VPTemplate,Type).


dcgReinterp(Term) --> theTerm(Term).
theTermStrings(Term) --> theWord(Term).
theTermStrings(Term) --> theTerm(Term).

dcgOptionalOne([])-->[].
dcgOptionalOne([X|_])-->dcgDressed(X),(!).
dcgOptionalOne([_|Xs])-->dcgOptionalOne(Xs).


dcgStartsWith(TheType,[S,M|ORE],[S,M|ORE]) :- phrase(TheType,[S,M],[]),!.
dcgStartsWith(TheType,[S|MORE],[S|MORE]) :- phrase(TheType,[S],[]),!.

dcgAndRest(TheType,TODO,[S|MORE],[]) :- phrase(TheType,[S],[]),phrase(TheType,[S|MORE],[]).

dcgConsumeRest(_,[]).
dcgPreTest(DCG,SE,SE):-phrase(DCG,SE,[]).
dcgContainsPreTest(DCG)-->dcgPreTest((dcgMapOne(DCG),dcgConsumeRest)).


% same dcgNoConsumeStarts(DCG,SE,SE):-dcgNoConsume((DCG,dcgConsumeRest),SE,SE).
dcgNoConsumeStarts(DCG,SE,SE):-phrase(DCG,SE,_).



:-dynamic(posOkForPhraseType(DefiniteNounPPFn,Pos)).

dcgPhraseType(DefiniteNounPPFn,DCG)--> dcgAnd((dcgStartsWith(thePOS(Pos)),{posOkForPhraseType(DefiniteNounPPFn,Pos)}),dcgDressed(X)).

dcgConstraintBindings(DCG,Constr) --> dcgDressed(DCG),{todo(cycQuery(Constr))}.
dcgConstraint(DCG,Constr) --> dcgDressed(DCG),{todo(cycQuery(Constr))}.

dcgCycL(DCG,Constr) --> {todo(getGenerationTemplateFor(dcgCycL(DCG,Constr,Template))),fail},Template.

dcgMaybeQuoted(X)-->dcgDressed(X).

dcgConjWithWord(Num,[Word,ConjList]) --> dcgSeq(Num,ConjList).

dcgRepeatForSubsequentArgsFn(Num,Each)--> dcgDressed(Each).
dcgNoSpaces(X)-->dcgDressed(X).

dcgReverse(Rev) --> {todo(getGenerationTemplateFor(dcgReverse(Rev,Template))),fail},Template.
dcgSeqReinterp(Rev) --> {todo(getGenerationTemplateFor(dcgReverse(Rev,Template))),fail},Template.

dcgOptionalSome(X)-->dcgOptionalCount(X,_N).

dcgOptionalCount([],0)-->[],{!}.
dcgOptionalCount([X|Xs],N)-->dcgDressed(X),{!},dcgOptionalCount(Xs,N1),{N is N1 + 1}.
dcgOptionalCount([_X|Xs],N)-->dcgOptionalCount(Xs,N).

dcgRequireSome(_Num,X)-->dcgOptionalCount(X,N),{!,N>0}.

% Num + List of Items
dcgSeq(Num,List,B,E):-number(Num),
   %is_list(List),
   !,dcgSeq(List,B,E).
% Item1 + Item2
dcgSeq(X,Y,[S0,S1|SS],E):-phrase((X,Y),[S0,S1|SS],E).
dcgSeq(X,Y,Z,[S0,S1|SS],E):-phrase((X,Y,Z),[S0,S1|SS],E).

dcgSeq([],B,B) :- !.
dcgSeq([Dcg|List],B,E) :- catch(phrase(Dcg,B,M),E,(writeq(E+phrase(Dcg,B,M)),nl,fail)),dcgSeq(List,M,E).


dcgBoth(DCG1,DCG2,S,R) :- append(L,R,S),phrase(DCG1,L,[]),once(phrase(DCG2,L,[])).

dcgAnd(DCG1,DCG2,DCG3,DCG4,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E),phrase(DCG3,S,E),phrase(DCG4,S,E).
dcgAnd(DCG1,DCG2,DCG3,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E),phrase(DCG3,S,E).
%dcgAnd(DCG1,DCG2,[S|TART],E) :- phrase(DCG1,[S|TART],E),phrase(DCG2,[S|TART],E).
dcgAnd(DCG1,DCG2,START,E) :- phrase(DCG1,START,E),phrase(DCG2,START,E).
dcgOr(DCG1,DCG2,DCG3,DCG4,DCG5,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E);phrase(DCG4,S,E);phrase(DCG5,S,E).
dcgOr(DCG1,DCG2,DCG3,DCG4,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E);phrase(DCG4,S,E).
dcgOr(DCG1,DCG2,DCG3,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E).
dcgOr(Num,DCG2,S,E) :- number(Num),!,member(DCG,DCG2),phrase(DCG,S,E).
dcgOr(DCG1,DCG2,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E).
dcgOr([D|CG1],S,E) :- member(One,[D|CG1]),phrase(One,S,E).
dcgNot(DCG2,S,E) :- not(phrase(DCG2,S,E)).
dcgIgnore(DCG2,S,E) :- ignore(phrase(DCG2,S,E)).
dcgOnce(DCG2,S,E) :- once(phrase(DCG2,S,E)).

dcgWhile(True,Frag)-->dcgAnd(dcgOneOrMore(True),Frag).

dcgOneOrMore(True) --> True,dcgZeroOrMore(True),{!}.

dcgZeroOrMore(True) --> True,{!},dcgZeroOrMore(True),{!}.
dcgZeroOrMore(True) --> [].

dcgMapCar(Var,DCG,[Var|List])-->{copy_term(Var+DCG,Var2+DCG2)},DCG,{!},dcgMapCar(Var2,DCG2,List).
dcgMapCar(Var,DCG,[])-->[].

dcgMapSome(Var,DCG,[Var|List])-->{copy_term(Var+DCG,Var2+DCG2)},DCG,{!},dcgMapSome(Var2,DCG2,List).
dcgMapSome(Var,DCG,List)--> [_],{!},dcgMapSome(Var,DCG,List).
dcgMapSome(Var,DCG,[])-->[].

dcgMapOne(DCG)--> DCG,{!}.
dcgMapOne(DCG)--> [_],dcgMapOne(DCG).

dcgConsumeData([]) --> [].
dcgConsumeData([Data|More]) --> [Data],{!},dcgConsumeData(More),{!}.

dcgLeftOf(Mid,[Left|T],S,[MidT|RightT]):-append([Left|T],[MidT|RightT],S),phrase(Mid,MidT),phrase([Left|T],LeftT).

dcgMid(Mid,Left,Right) --> dcgLeftOf(Mid,Left),Right.

scanDcgUntil(_,_,[],__):-!,fail.
scanDcgUntil(DCG,[],[Found|Rest],[Found|Rest]):-phrase(DCG,[Found],[]),!.
scanDcgUntil(DCG,[Skipped|Before],[Skipped|More],[Found|Rest]):-scanDcgUntil(DCG,Before,More,[Found|Rest]).

dcgNone --> [].
dcgAny --> [_].

% Matches the rightmost DCG item
dcgLast(DCG,S,Left):-append(Left,[Last],S),phrase(DCG,[Last]).

dcgIfThenElse(DCG,True,False,S,E) :- phrase(DCG,S,M) -> phrase(True,M,E) ; phrase(False,S,E).
dcgIfThenOr(DCG,True,False,S,E) :- (phrase(DCG,S,M) , phrase(True,M,E)) ; phrase(False,S,E).

dcgOptional(_A,[],[]):-!.
dcgOptional(A,S,E):-phrase(A,S,E),!.
dcgOptional(_A,S,S).

%throwOnFailure(X):-once(X;(trace,X)).
debugOnError(X):-catch(X,E,(writeq(E),trace,X)).

dcgNonExpandedFrom(Pos,GenValueParaphraseFn,ConjList) -->  dcgSeq(ConjList).
dcgNonExpandedVar(NDecimalPlaceParaphraseFn, VARG1 , [Num], Var) --> 
   dcgAnd(theVar(VARG1,Var),dcgNote(dcgNonExpandedVar(NDecimalPlaceParaphraseFn, VARG1 , [Num], Var))).


capitalized([W|Text]) --> theText([W|Text]),{atom_codes(W,[C|Odes]),is_upper(C)}.

substAll(B,[],R,B):-!.
substAll(B,[F|L],R,A):-subst(B,F,R,M),substAll(M,L,R,A).
   
substEach(B,[],B):-!.
substEach(B,[F-R|L],A):-subst(B,F,R,M),substEach(M,L,A).
:-trace(substEach,-all).

% =======================================================
% DCG Tester
% =======================================================
testPhrase(Dcg,English):-
         sentenceTagger(English,Tagged),dumpList(Tagged),
         phrase(Dcg,Tagged,Left),
         nl,nl,writeq(left),         
         nl,dumpList(Left).


% =======================================================
% utterance(Event,Subj,CycL, [every,man,that,paints,likes,monet],[]) 
% =======================================================
englishCtx(Event,CycL) --> utterance(Event,Subj,CycL),
      {suggestVar('SENTENCE-EVENT',Event),suggestVar('SENTENCE-SUBJECT',Subj),suggestVar('SENTENCE-CYCL',CycL)}.



utterance(Event,Subj,CycL) -->(theText([X])),{member(X,[('.'),('!'),(',')]),!},utterance(Event,Subj,CycL).
utterance(Event,Subj,'CYC-ASSERT'(CycL9)) -->dcgLast(theText([X])),{member(X,[('.'),('!'),(',')]),!},utterance(Event,Subj,CycL9).
utterance(Event,Subj,'CYC-QUERY'(CycL)) -->dcgLast(theText([X])),{member(X,[('?')]),!},utterance(Event,Subj,CycL).
%test utterance(Event,Subj,CycL) -->['S'|Form],{!,throwOnFailure(phrase(utterance(Event,CycL),Form))}.
%test utterance(Event,Subj,template(TempType,CycL)) --> {member(TempType,['STemplate'])},dcgTemplate(TempType,Event,CycL).
%test utterance(Event,Subj,template(TempType,CycL)) --> dcgTemplate(TempType,Event,CycL).
%utterance(Event,Subj,CycL) --> noun_phrase(Event,Subj,holdsIn('Now',Subj),CycL).
%utterance(Event,Subj,CycL,S,[]) :- once((ground(S), toText(S,T))),cyclifyTest(string(T),CycL).
utterance(Event,Subj,forAll(Subj,forAll(Obj,implies(isa(Subj,SType),isa(Obj,SObj))))) --> 
   universal_word(Form),dcgAnd(theIsa(Subj,SType,'Collection'),theForm(Form)),thePOS('BeAux'),theIsa(Obj,SObj,'Collection').
utterance(Event,Subj,declaritive(CycL)) --> declaritive_sentence(Subj,Event,Obj,CycL).
utterance(Event,Subj,imparitive(CycL)) --> imparitive_sentence(Event,CycL),nonQMarkPunct.
utterance(Event,Subj,'CYC-QUERY'(CycL)) --> inquiry(Event,CycL).	

%utterance(Event,Subj,'NothingSaid') -->[].
%utterance(Event,Subj,'CYC-QUERY'(CycL9))-->thePOS('Modal'),!,utterance(Event,Subj,CycL9).
%old utterance(Event,Subj,verbSubjObj(Event,Subj,Obj,CycL9)) --> noun_phrase(Event,Subj,VerbIn,CycL9),vp(Event,Subj,Obj,VerbIn).
%old utterance(Event,Subj,verbSubjObj(Event,'?TargetAgent',Obj,CycL9)) --> vp(Event,Subj,Obj,CycL9).
utterance(Event,Subj,CycL9) --> noun_phrase(Event,Subj,the(Subj),CycL9).  
utterance(Event,Subj,and(isa(Event,'Event'),rel_clause(CycL9))) --> rel_clause(Event,Subj,CycL9). 
%utterance(Event,Subj,and(CycL1,CycL2)) -->utterance(Event,Subj,CycL1),utterance(Event,Subj,CycL2).
%utterance(Event,Subj,CycL) -->utterance(Event,Subj,CycL1),{substEach(CycL1,[':POSSESSOR'-Pos],CycL),suggestVar('POSSESSOR',Pos)}.

nonQMarkPunct --> dcgAnd(thePOS('Punctuation-SP'),dcgNot(theWord('QuestionMark-TheSymbol'))).
nonQMarkPunct --> [].


imparitive_sentence(Event,CycL) --> thePOS('Interjection-SpeechPart'),{!},imparitive_sentence(Event,CycL).
imparitive_sentence(Event,utters('?SPEAKER',Word,Means)) --> dcgAnd(thePOS(Word,'Interjection-SpeechPart'),dcgIgnore(theTerm(Means))).
imparitive_sentence(Event,CycL) --> theWord('Please-TheWord'),{!},imparitive_sentence(Event,CycL).
imparitive_sentence(Event,CycL) --> verb_phrase(Time,Subj,Obj,Event,CycL),{suggestVarI('TargetAgent',Subj),suggestVar('ImparitiveEvent',Event)}.




declaritive_sentence(Subj,Event,Obj,and(CycL,HowDoes)) --> dcgStartsWith(thePOS(POS)),theFrame(Event,'Pre-NounPhraseModifyingFrame',POS,Template),
    declaritive_sentence(Subj,Event,Obj,CycL), {substEach(Template,[':SUBJECT'-Subj,':NOUN'-Subj,':ACTION'-Event,':OBJECT'-Obj],HowDoes)}.

declaritive_sentence(Subj,Event,Obj,CycL9) -->  theWord('From-TheWord'),{!},noun_phrase(Event,Target,and(CycL1,'from-Underspecified'(Target,Obj)),CycL9),theText([(,)]),
                 declaritive_sentence(Subj,Event,Obj,CycL1).

declaritive_sentence(Subj,Event,Obj,CycL) --> noun_phrase(Event,Subj,CycL1,CycL),verb_phrase(Time,Subj,Obj,Event,CycL1).
%declaritive_sentence(Subj,Event,Obj,CycL) --> noun_phrase(Event,Subj,and(CycL1,CycL2),CycL),
%declaritive_sentence(Subj,Event,Obj,CycL) --> theWord('The-TheWord'),trans2_verb_unit(Subj,Event,Obj,VProp),possible_prep,noun_phrase(Event,Obj,VProp,CycL1),theWord('Be-TheWord'),noun_phrase(Event,Subj,CycL1,CycL).
%declaritive_sentence(Subj,Event,Obj,CycL) --> noun_phrase(Event,Obj,VProp,CycL1),theWord('The-TheWord'),trans2_verb_unit(Subj,Event,Obj,VProp),possible_prep,  noun_phrase(Event,Obj,VProp,CycL1),theWord('Be-TheWord'),noun_phrase(Event,Subj,CycL1,CycL).

%declaritive_sentence(Subj,Event,Obj,CycL) -->{!},utterance(Event,Subj,CycL).

possible_prep-->[of];[].

inquiry(Event,'thereExists'(Actor,CycL)) --> wh_pronoun(Actor), verb_phrase(Time,Actor,Obj,Event,CycL),theText([?]),{suggestVar('Event',Event)}.
inquiry(Event,CycL) --> inv_sentence(Event,CycL),theText([?]).
%inquiry(CycL) --> declaritive_sentence(Subj,Event,Obj,CycL),theText([?]).

inv_sentence(Event,holdsIn(Time,CycL)) --> verbal_aux(Time), utterance(Event,Subj,CycL).

verbal_aux(Time) --> aux_phrase(Time,Subj,Obj,Event,CycL).

%WHAdverb aux_phrase(Time,Subj,Obj,Event,CycL),
wh_pronoun('?Who') --> theWord('Who-TheWord').
wh_pronoun('?What') --> theWord('What-TheWord').


%You could apply chineese room to human physical neurons.. but does that make us unintelligent?


joinNP(N1,no(noun),N1):-!.
joinNP('NounFn'(N1),'NounFn'(N2),'NounFn'(N1,N2)):-!.
joinNP(N1,N2,'JoinFn'(N1,N2)).


/*
vp(Event,Subj,Obj1,and(CycL5,CycL9)) -->vp1(Event,Subj,Obj1,CycL5),conj(CC),vp1(Event,Subj,Obj2,CycL9).
vp(Event,Subj,Obj,CycL9) -->vp1(Event,Subj,Obj,CycL9).

compressed is : 
*/
vp(Event,Subj,Obj,CycLO) --> vp1(Event,Subj,Obj,CycL5),
   dcgIfThenElse((conj(CC),vp1(Event,Subj,Obj2,CycL9)),{CycLO=and(CycL5,CycL9)},{CycLO=CycL5}).


/*
vp1(Event,Subj,Obj,and(hasAttribute(Event,AVP),CycL9)) --> verb_unit(Time,Subj,Obj,Target,Event,CycL5),object_prep_phrase(Event,Obj,CycL5,CycL9),adv(AVP).
vp1(Event,Subj,Obj,CycL9) --> verb_unit(Time,Subj,Obj,Target,Event,CycL5),object_prep_phrase(Event,Obj,CycL5,CycL9).

compressed is : 
*/
vp1(Event,Subj,Obj,holdsIn(Time,CycL9))-->verb_phrase(Time,Subj,Obj,Event,CycL9).
vp1(Event,Subj,Obj,CycLO) --> verb_unit(Time,Subj,Obj,Event,CycL5),object_prep_phrase(Event,Obj,CycL5,CycL9),
   dcgIfThenElse(adv(AVP),{CycLO=and(hasAttribute(Event,AVP),CycL9)},{CycLO=CycL9}).


object_prep_phrase(Event,Obj,CycL0,CycL9) --> noun_phrase(Event,Obj,CycL0,CycL5),  
            thePrep(Event2,P),noun_phrase(Event2,Subj,thereExists(Event2,and(isa(Event2,'Situation'),actorIn(Event2,Subj))),CycL9),{!}.
object_prep_phrase(Event,Obj,CycL0,CycL9) --> noun_phrase(Event,Obj,CycL0,CycL9).
object_prep_phrase(Event,Obj,CycL0,and(CycL0,CycL)) --> utterance(Event,Obj,CycL).
object_prep_phrase(Event,Obj,CycL0,CycL0)-->[].


% :-ignore((predicate_property(aux(_,_,_),PP),writeq(PP),nl,fail)).
auxV(_,[],_):-!,fail.
auxV(AVP) --> thePOS(AVP,'Modal').
auxV(AVP) --> thePOS(AVP,'AuxVerb').


adv(_,[],_):-!,fail.
adv('Adverb2Fn'(AVP,AVP1))-->thePOS(AVP,'Adverb'),conj(CC),thePOS(AVP1,'Adverb'),{!}.
adv('AdverbFn'(AVP))-->thePOS(AVP,'Adverb').


thePrep(_,_,[],_):-!,fail.
thePrep(Event2,P)-->conj(CC),{!},thePrep(Event2,P).
thePrep(Event2,'PrepAFn'(P,V))-->auxV(V),{!},thePrep(Event2,P).
thePrep(Event2,'PrepJoinFn'(P,P2))-->dcgBoth((thePOS(P,'Preposition'),thePOS(P2,'Preposition')),theName(Event2)).
thePrep(Event2,'PrepositionFn'(P))-->dcgBoth((thePOS(P,'Preposition')),theName(Event2)).
thePrep(Event2,Word) --> dcgAnd(thePOS('Preposition'),theWord(Word)).
thePrepWord(Prep)-->dcgAnd(thePOS('Preposition'),theWord(Prep)).


conj(_,[],_):-!,fail.
conj(CC)-->theWord('As-TheWord'),theWord('Well-TheWord'),theWord('As-TheWord').
conj(CC)-->conj1(_),conj1(_),conj1(CC).
conj(CC)-->conj1(_),conj1(CC).
conj(CC)-->conj1(CC).

conj1(CC)-->theText([(',')]).
conj1(CC)-->thePOS(CC,'CoordinatingConjunction').



% =======================================================
% Load required files
% =======================================================
:- ensure_loaded(dcgTermTemplate).
:- ensure_loaded(logicmoo_nl_sem_noun_phrase).
:- ensure_loaded(logicmoo_nl_sem_verb_phrase).

%:- ensure_loaded(logicmoo_nl_sem_chunk_parser).
:- ensure_loaded(logicmoo_nl_sem_dcgRedress).

:-set_prolog_flag(double_quotes,string).
:-include(logicmoo_nl_testing).


/* 
Kino's numspecs
"
(cyc-assert '
(#$implies 
(#$and
(#$genls ?C #$Thing) 
 (#$evaluate ?R (#$EvaluateSubLFn 
     (#$ExpandSubLFn 
       (?C) 
       (LENGTH 
         (REMOVE-DUPLICATES 
           (ALL-SPECS ?C)))))))
(#$ist #$BaseKB     (#$numspecs ?C ?R))) #$BaseKB) "*/

dm1:-e2c("I see two books sitting on a shelf").
dm2:-e2c("AnyTemplate1 affects the NPTemplate2").
dm3:-e2c("AnyTemplate1 at AnyTemplate2").
dm4:-e2c("This is a pos check").

%% ABCL tests: (progn (defconstant CHAR-CODE-LIMIT 256) (load "doit.lsp"))
% ?>
setopt:-set_prolog_flag(toplevel_print_options,[quoted(true), portray(true), max_depth(0), attributes(portray)]).

:-setopt.
