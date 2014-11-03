/** <module> 
% ===================================================================
% File 'dbase_i_cyc.pl'
% Purpose: Emulation of OpenCyc for SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'interface.pl' 1.0.0
% Revision:  $Revision: 1.9 $
% Revised At:   $Date: 2002/06/27 14:13:20 $
% ===================================================================
% File used as storage place for all predicates which make us more like Cyc
%
% Dec 13, 2035
% Douglas Miles
*/

:-user_ensure_loaded(logicmoo(ext/moo_ext_cyc_new)).

:-dynamic(isCycUnavailable_known/1).
:-dynamic(isCycAvailable_known/0).

:-export(isCycAvailable/0).
isCycAvailable:-isCycUnavailable_known(_),!,fail.
isCycAvailable:-isCycAvailable_known,!.
isCycAvailable:-checkCycAvailablity,isCycAvailable.

:-export(isCycUnavailable/0).
isCycUnavailable:-isCycUnavailable_known(_),!.
isCycUnavailable:-isCycAvailable_known,!,fail.
isCycUnavailable:-checkCycAvailablity,isCycUnavailable.

:-export(checkCycAvailablity/0).
checkCycAvailablity:- (isCycAvailable_known;isCycUnavailable_known(_)),!.
checkCycAvailablity:- ccatch((ignore((invokeSubL("(+ 1 1)",R))),(R==2->assert_if_new(isCycAvailable_known);assert_if_new(isCycUnavailable_known(R)))),E,assert_if_new(isCycUnavailable_known(E))),!.


end_of_file.
:- meta_predicate isDebug(0).
/*
stringToWords([],[]).
stringToWords(TheList,Words):-functor(TheList,'TheList',_),TheList=..[_|List],!,stringToWords(List,Words).
stringToWords(string(S),Words):-!,stringToWords(S,Words).
stringToWords(['TheList'|List],Words):-!,stringToWords(List,Words).
stringToWords([S|Tring],[W|Words]):-stringToWord(S,W),stringToWords(Tring,Words).

:-dynamic(textCached/2).

stringToWord([S|L],W):-!,textCached([S|L],[lex,W|_]).
stringToWord(S,W):-textCached([S],[lex,W|_]).
*/
     
:-export(list_to_term/2).
%list_to_term(X,Y):- balanceBinding(X,Y).
list_to_term(X,Y):-nonvar(X),var(Y),!,list_to_terms_lr(X,Y).
list_to_term(X,Y):-list_to_terms_rl(X,Y).
list_to_terms_rl(List,(A,B)):-list_to_terms_rl(A,AL),list_to_terms_rl(B,BL),append(AL,BL,List).
list_to_terms_rl(List,(A;B)):-list_to_terms_rl(A,AL),list_to_terms_rl(B,BL),append(AL,[or|BL],List).
list_to_terms_lr([],true):-!.
list_to_terms_lr([T],T):-!.
list_to_terms_lr([H|T],(H,TT)):-!,list_to_terms_lr(T,TT).

% ===================================================================

%list_to_conj(X,Y):- balanceBinding(X,Y).
list_to_conj(X,Y):-nonvar(X),var(Y),!,list_to_conjs_lr(X,Y).
list_to_conj(X,Y):-list_to_conjs_rl(X,Y).
list_to_conjs_rl(List,(A,B)):-list_to_conjs_rl(A,AL),list_to_conjs_rl(B,BL),append(AL,BL,List).
list_to_conjs_rl(List,(A;B)):-list_to_conjs_rl(A,AL),list_to_conjs_rl(B,BL),append(AL,[or|BL],List).
list_to_conjs_lr([],true):-!.
list_to_conjs_lr([T],T):-!.
list_to_conjs_lr([H|T],(H,TT)):-!,list_to_conjs_lr(T,TT).
   
:-export(balanceBinding/2).
balanceBinding(Binding,Binding):- (var(Binding);number(Binding)),!.
balanceBinding(string(B),string(B)):-!.
balanceBinding(Binding,BindingP):-atom(Binding),atom_concat('#$',BindingP,Binding),!.
balanceBinding(nart(B),nart(BA)):-balanceBinding(B,BA),!.
balanceBinding(nart(B),(BA)):-!,balanceBinding(B,BA),!.
balanceBinding(string(B),List):-atomSplit(List,B),!.
balanceBinding(string(B),B):-!.
balanceBinding(string([]),""):-!.
balanceBinding(quote(B),BO):-!,balanceBinding(B,BO).
balanceBinding(['noparens','#','G',[GU|ID]],guid([GU|ID])):-!.
balanceBinding([A|L],Binding):-balanceBindingCons(A,L,Binding).
balanceBinding(Binding,Binding):-!.
 
balanceBindingCons(A,L,[A|L]):- (var(A);var(L);A=string(_);number(A)),!.
balanceBindingCons('and-also',L,Binding):-balanceBindingS(L,LO), list_to_conj(LO,Binding),!.
balanceBindingCons('eval',L,Binding):-balanceBindingS(L,LO), list_to_conj(LO,Binding),!.
balanceBindingCons('#$and-also',L,Binding):-balanceBindingS(L,LO), list_to_conj(LO,Binding),!.

balanceBindingCons(A,L,Binding):-
	 balanceBinding(A,AO),
         balanceBindingCons(A,AO,L,Binding).
balanceBindingCons(_A,AO,L,Binding):-
         atom(AO),!,
	 balanceBindingS(L,LO),
	 Binding=..[AO|LO],!.
balanceBindingCons(_A,AO,L,Binding):-
	 balanceBindingS(L,LO),
	 Binding=[AO|LO],!.

balanceBindingS(Binding,Binding):- (var(Binding);atom(Binding);number(Binding)),!.
balanceBindingS([],[]).
balanceBindingS([V,[L]|M],[LL|ML]):-V=='\'',balanceBindingS(L,LL),balanceBindingS(M,ML).
balanceBindingS([A|L],[AA|LL]):-balanceBinding(A,AA),balanceBindingS(L,LL).
   

% ===================================================================
% Connecter to Cyc TCP Server
% ===================================================================
:-dynamic(cycConnection/3).
:-dynamic(cycConnectionUsed/3).
:-dynamic(cycMutex/2).
:-dynamic(cycChatMode/1).

getCycConnection(SocketId,OutStream,InStream):-
      retract(cycConnection(SocketId,OutStream,InStream)),
      assertz(cycConnectionUsed(SocketId,OutStream,InStream)),!.

getCycConnection(SocketId,OutStream,InStream):-
      tcp_socket(SocketId),
      tcp_connect(SocketId,'CycServer':3601),
      tcp_open_socket(SocketId, InStream, OutStream),!,
      isDebug((dmsg(user_error,'Connected to Cyc TCP Server {~w,~w}\n',[InStream,OutStream]),flush_output(user_error))),
      assertz(cycConnectionUsed(SocketId,OutStream,InStream)),!.

finishCycConnection(SocketId,OutStream,InStream):-
      ignore(system:retractall(cycConnectionUsed(SocketId,OutStream,InStream))),
      asserta(cycConnection(SocketId,OutStream,InStream)),!.

:-export(cycStats/0).
cycStats:- % will add more 
   listing(cycConnection),
   listing(cycConnectionUsed).

cycInit.

% ===================================================================
% Invoke SubL
% invokeSubLRaw(-Send[,+Receive]).
% 
% ?- invokeSubLRaw('(find-constant "Dog")').
% #$Dog
%
% ===================================================================

invokeSubL(Send):-
      invokeSubLRaw(Send,Receive),
      isDebug(dmsg('~s',[Receive])).

invokeSubL(Send,Receive):-
      invokeSubLRaw(Send,ReceiveCodes),
      atom_codes(Receive,ReceiveCodes).

invokeSubLRaw(Send,Receive):-  
      getCycConnection(SocketId,OutStream,InStream),
      call_cleanup(once((
         printSubL(InStream,OutStream,Send),
         readSubL(InStream,Get))),
       finishCycConnection(SocketId,OutStream,InStream)),
      checkSubLError(Send,Get,Receive),!.

checkSubLError(Send,[53,48,48,_|Info],Info):-!, %Error "500 "
      atom_codes(ErrorMsg,Info),
      throw(cyc_error(ErrorMsg,Send)).
checkSubLError(_,[_,_,_,_|Info],Info):-!.
checkSubLError(_Send,Info,Info).

% ===================================================================
% Lowlevel printng
% ===================================================================

printSubL(InStream,OutStream,Send):-
      popRead(InStream),
      printSubL(OutStream,Send).

printSubL(OutStream,Send):-     
      (var(Send) ->
	 throw(cyc_error('Unbound SubL message',Send));
         is_list(Send) ->
	    formatCyc(OutStream,'~s~n',[Send]);
	       atom(Send) -> formatCyc(OutStream,'~w~n',[Send]);
	       compound(Send) ->
      	       (toCycApiExpression(Send,[],STerm),formatCyc(OutStream,'~w~n',[STerm]));
%	       throw(cyc_error('SubL message type not supported',Send)),
	       	       formatCyc(OutStream,'~w~n',[Send])),!.


formatCyc(OutStream,Format,Args):-
      dmsg(OutStream,Format,Args),
      isDebug(dmsg(user_error,Format,Args)),
      flush_output(OutStream),!.

readSubL(InStream,[G,E,T,Space|Response]):-
      get_code(InStream,G),
      get_code(InStream,E),
      get_code(InStream,T),
      get_code(InStream,Space),
      readCycLTermChars(InStream,Response),!.

% ===================================================================
% Lowlevel readCycLTermChars
% ===================================================================
:-export(readCycLTermChars/2).
readCycLTermChars(InStream,Response):-
   must_det(readCycLTermChars(InStream,Response,_)).
   
:-export(readCycLTermChars/3).
readCycLTermChars(InStream,[Start|Response],Type):-
   peek_code(InStream,Start),
   readCycLTermCharsUntil(Start,InStream,Response,Type),
   isDebug(dmsg('cyc>~s (~w)~n',[Response,Type])).

readCycLTermCharsUntil(34,InStream,Response,string):-!,
   get_code(InStream,_),
   readUntil(34,InStream,Response),
   popRead(InStream).

readCycLTermCharsUntil(35,InStream,[35|Response],term):-!,
   get_code(InStream,_),
   readUntil(10,InStream,Response),
   popRead(InStream).

readCycLTermCharsUntil(84,InStream,"T",true):-!,
   popRead(InStream).

readCycLTermCharsUntil(78,InStream,"N",nill):-!,
   popRead(InStream).

readCycLTermCharsUntil(40,InStream,Trim,cons):-!,
   readCycL(InStream,Trim),
   popRead(InStream).

popRead(InStream) :- once(wait_for_input([InStream], Inputs,0.01)),Inputs=[],!.
popRead(InStream) :-get_code(InStream, _),popRead(InStream).

readUntil(Char,InStream,Response):-
      get_code(InStream,C),
      readUntil(Char,C,InStream,Response).
      
readUntil(Char,Char,_InStream,[]):-!.
readUntil(Char,C,InStream,[C|Out]):-get_code(InStream,Next),
   readUntil(Char,Next,InStream,Out).

      
      
% ===================================================================
%  conversion toCycApiExpression
% ===================================================================

toCycApiExpression(Prolog,CycLStr):-toCycApiExpression(Prolog,[],CycLStr).

toCycApiExpression(Prolog,Vars,Chars):-var(Prolog),!,toCycVar(Prolog,Vars,Chars).
toCycApiExpression(Prolog,_Vars,Prolog):-(atom(Prolog);number(Prolog)),!.
toCycApiExpression(Prolog,_Vars,Chars):-is_string_e2c(Prolog),!,sformat(Chars,'"~s"',[Prolog]).
toCycApiExpression(nv(List),Vars,Chars):-toCycApiExpression_l(List,Vars,Chars),!.
toCycApiExpression([nv|List],Vars,Chars):-toCycApiExpression_l(List,Vars,Chars),!.
   
toCycApiExpression([P|List],Vars,Chars):-
			toCycApiExpression_l([P|List],Vars,Term),
			sformat(Chars,'\'(~w)',[Term]).
toCycApiExpression(quote(List),Vars,Chars):-
			toCycApiExpression(List,Vars,Term),
			sformat(Chars,'\'~w',[Term]).
toCycApiExpression(Prolog,Vars,Chars):-compound(Prolog),!,
			Prolog=..[P|List],
			toCycApiExpression_l(List,Vars,Term),
			(P = holds ->
			   sformat(Chars,'(~w)',[Term]);
			   sformat(Chars,'(~w ~w)',[P,Term])).

toCycApiExpression_l([],_Vars,''):-!.
toCycApiExpression_l([A],Vars,Chars):-toCycApiExpression(A,Vars,Chars),!.
toCycApiExpression_l([A|Rest],Vars,Chars):-
      toCycApiExpression(A,Vars,Chars1),
      toCycApiExpression_l(Rest,Vars,Chars2),
      sformat(Chars,'~w ~w',[Chars1,Chars2]),!.

toCycVar(Var,[VV|_],NameQ):-nonvar(VV),VV=..[_,Name,VarRef],
   Var==VarRef,!,sformat(NameQ,'?~w',[Name]).
toCycVar(Var,[_|Rest],Name):-nonvar(Rest),toCycVar(Var,Rest,Name).
toCycVar(VAR,_,VarName):-
      term_to_atom(VAR,AVAR),
      atom_codes(AVAR,[95|CODES]),!,
      catch(sformat(VarName,'?HYP-~s',[CODES]),_,VarName='?HYP-VAR').

is_string_e2c([A,B|_]):-integer(A),integer(B).


% ===================================================================
%  Debugging Cyc 
% ===================================================================
     
:-dynamic(isDebug).

% Uncomment this next line to see Cyc debug messages

isDebug.

isDebug(Call):- isDebug -> ignore(once(Call)) ; true.


% ===================================================================
%  Cyc Query Cache Control
% ===================================================================


:-dynamic(cachable_query/1).
:-dynamic(cached_query/2).

cachable_query(isa(_,_)).

% ===================================================================
%  Cyc Assert
% ===================================================================
:- dynamic_multifile_exported mtForPred/2.

cycAssert(Mt:CycL):-!,
   cycAssert(CycL,Mt).
cycAssert(CycL):-
   mtForPred(CycL,Mt),
   cycAssert(CycL,Mt).

cycAssert(_CycL,_Mt):- isCycUnavailable,!.
cycAssert(CycL,Mt):-
      system:retractall(cached_query(_,_)),
      cyclifyNew(CycL,CycLGood),
      cyclify(Mt,MtGood),
      defaultAssertOptions(DefaultOptions), 
      toCycApiExpression('CYC-ASSERT'(quote(CycLGood),MtGood,DefaultOptions),API),
      invokeSubL(API),!.

:-dynamic(defaultAssertOptions/1).

defaultAssertOptions([':FORWARD',':MONOTONIC']).

      
% ===================================================================
%  Cyc Unassert/Retract
% ===================================================================
cycRetract(CycL,Mt):-cycRetractAll(CycL,Mt).
cycRetract(CycL):-cycRetractAll(CycL).

cycRetractAll(CycL):-
      mtForPred(CycL,Mt),
      cycUnassert(CycL,Mt).

cycRetractAll(CycL,Mt):-cycUnassert(CycL,Mt).
cycUnassert(CycL,Mt):-
      system:retractall(cached_query(_,_)),
      cyclifyNew(CycL,CycLGood),
      cyclify(Mt,MtGood),
      invokeSubL('CYC-UNASSERT'(quote(CycLGood),MtGood)).


% ===================================================================
%  Cyc Query
% ===================================================================

cycQuery(CycL):-cycQuery(CycL,'EverythingPSC',_Result).
cycQuery(CycL,Mt):-cycQuery(CycL,Mt,_Result).

cycQuery(CycL,Mt,Result):-
      copy_term(CycL,Copy),
      numbervars(Copy,'$VAR',0,_),!,
      cycQuery(Copy,CycL,Mt,Result).

cycQuery(Copy,CycL,_Mt,_Result):-cached_query(Copy,Results),!,
      member(CycL,Results).
cycQuery(Copy,CycL,Mt,Result):-cachable_query(Copy),!,
      findall(CycL,cycQueryReal(CycL,Mt,Result),Save),
      (Save=[] -> true ; asserta(cached_query(CycL,Save))),!,
      member(CycL,Save).
cycQuery(_Copy,CycL,Mt,Result):-
      cycQueryReal(CycL,Mt,Result).

cycQueryReal(CycL,Mt,Result):-
      getCycConnection(SocketId,OutStream,InStream),
      popRead(InStream),
      cyclify(CycL,CycLGood),
      cyclify(Mt,MtGood),
      printSubL(OutStream,'CYC-QUERY'(quote(CycLGood),MtGood)),
      get_code(InStream,_A),
      get_code(InStream,_B),
      get_code(InStream,_C),
      get_code(InStream,_D),
      free_variables(CycLGood,Vars),
      get_code(InStream,_E),!,% Takes the first paren
      repeat,
      (peek_code(InStream,PCode), 
      isDebug(dmsg('PCODE (~q)~n',[PCode])),
      ((member(PCode,[35,73]),finishCycConnection(SocketId,OutStream,InStream),!,fail);true), % 35 is No
      ((PCode=78,finishCycConnection(SocketId,OutStream,InStream),!);(    % 78 is Yes
      readCycL(InStream,Trim),
      peek_code(InStream,Code), 
      isDebug(dmsg('"~s" (~q)~n',[Trim,Code])),
      ((Code\=32,!,finishCycConnection(SocketId,OutStream,InStream));(true)),
      getSurfaceFromChars(Trim,IResult,_),
      IResult=[Result],
      syncCycLVars(Result,Vars)))).

syncCycLVars(_,[]).
syncCycLVars([[_, '.', Binding]|T],[Binding|VV]):-syncCycLVars(T,VV),!.
syncCycLVars([[_|Binding]|T],[Binding|VV]):-syncCycLVars(T,VV),!.

% ===================================================================
%  Cyclification
%
%    cyclify(Before,After)
%     Makes sure that atoms in Statement are prefixed witbh '#$' when comunicationg with Cyc
%
%    cyclifyNew(Before,After)
%     same as cyclify/2 but adds the constant names with (CREATE-CONSTANT "~w")
%
% ===================================================================

cyclify(Same,Same):-var(Same);number(Same).
cyclify([],[]).
cyclify([H|T],Term):-integer(H) -> Term=[H|T]; cyclify_l([H|T],Term).
cyclify(Before,After):-atom(Before),
      sub_atom(Before,0,1,_,F),!,
      cyclify(F,Before,After).
cyclify(Before,After):-
      Before=..[B|BL],
      cyclify(B,A),
      cyclify_l(BL,AL),
      After=..[A|AL].

cyclify('#',Before,Before).
cyclify('?',Before,Before).
cyclify(':',Before,Before).
cyclify('!',Before,After):-atom_concat('!',After,Before).
cyclify('"',Before,Before).
cyclify(_,Before,After):-atom_concat('#$',Before,After).
      
cyclify_l([B],[A]):-cyclify(B,A),!.
cyclify_l([],[]).
cyclify_l([B|BL],[A|AL]):-
      cyclify(B,A),
      cyclify_l(BL,AL).


cyclifyNew(Same,Same):-var(Same);number(Same).
cyclifyNew([],[]).
cyclifyNew([H|T],Term):-integer(H) -> Term=[H|T]; cyclifyNew_l([H|T],Term).
cyclifyNew(Before,After):-atom(Before),
      sub_atom(Before,0,1,_,F),!,
      cyclifyNew(F,Before,After).
cyclifyNew(Before,After):-
      Before=..[B|BL],
      cyclifyNew(B,A),
      cyclifyNew_l(BL,AL),
      After=..[A|AL].

cyclifyNew('#',Before,Before).
cyclifyNew('?',Before,Before).
cyclifyNew(':',Before,Before).
cyclifyNew('!',Before,After):-atom_concat('!',After,Before).
cyclifyNew('"',Before,Before).
cyclifyNew(_,Before,After):-atom_concat('#$',Before,After),makeConstant(Before).
      
cyclifyNew_l([B],[A]):-cyclifyNew(B,A),!.
cyclifyNew_l([],[]).
cyclifyNew_l([B|BL],[A|AL]):-
      cyclifyNew(B,A),
      cyclifyNew_l(BL,AL).


% ============================================
% Make new CycConstant
% ============================================

:-dynamic(cycConstantMade/1).

makeConstant(Const):-
   (cycConstantMade(Const)->true;
   (sformat(String,'(CREATE-CONSTANT "~w")',[Const]),
   catch(invokeSubL(String),_,true),
   asserta(cycConstantMade(Const)))),!.

% ============================================
% Make new Microtheory
% ============================================

ensureMt(Const):-
   cycAssert('isa'(Const,'Microtheory'),'BaseKB').

% ============================================
% dynamic Default Microtheory
% ============================================

:-dynamic(defaultMt/1).

defaultMt('PrologDataMt').


addPrologKB:- defaultMt(Mt),!,ensureMt(Mt),cycAssert(moo:'genlMt'(Mt,'InferencePSC'),'BaseKB'). % Puts the defaultMt/1 into Cyc 

% :- at_start((isCycUnavailable -> true ; addPrologKB)).

% ===================================================================
%  Predicates need and Assertion Mt
% ===================================================================
:-dynamic(mtForPred/2).

mtForPred(CycL,Mt):-
   functor(CycL,Pred,_),
   isRegisteredCycPred(Mt,Pred,_),!.

mtForPred(_CycL,Mt):-defaultMt(Mt).


isRegisteredCycPred(Mt,F,_A):-mpred_prop(F,mt(Mt)).

% ============================================
% Assert Side Effect Prolog to Cyc Predicate Mapping
%
% ?- assert(isa('Fido','Dog')).
% Will assert (#$isa #$Fido #$Dog) into #$BaseKB
%
% ?- assert('DogsMt':isa('Fido','Dog')).
% Will assert (#$isa #$Fido #$Dog) into #$DogsMt
% ============================================
% :-redefine_system_predicate(assert(_)).
% assert(Term):-assertThrough(Term).

assertThrough(Mt:CycL):-
      assertThrough(Mt,CycL).

assertThrough(CycL):-
      assertThrough(_Mt,CycL).

assertThrough(ToMt,CycL):- throw(whyHEre),
      functor(CycL,Pred,Arity),
      isRegisteredCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycAssert(CycL,ToMt),!.

assertThrough(ToMt,CycL):-
      ignore(ToMt=user),
      assertz(ToMt:CycL),!.

% ============================================
% Retract (All) Side Effect Prolog to Cyc Predicate Mapping
%
% ?- retractall(isa('Fido','Dog')).
% Will retract (#$isa #$Fido #$Dog) from #$BaseKB
%
% ?- retractall('DogsMt':isa('Fido','Dog')).
% Will retract (#$isa #$Fido #$Dog) from #$DogsMt
% ============================================
% :-redefine_system_predicate(retractall(_)).
% retractall(Term):-retractAllThrough(Term).

retractAllThrough(Mt:CycL):-
      retractAllThrough(Mt,CycL).

retractAllThrough(CycL):-
      retractAllThrough(_Mt,CycL).

retractAllThrough(ToMt,CycL):-
      functor(CycL,Pred,Arity),
      isRegisteredCycPred(Mt,Pred,Arity),!,
      ignore(ToMt=Mt),
      cycRetract(CycL,ToMt),!.

retractAllThrough(ToMt,CycL):-
      ignore(ToMt=user),
      system:retractall(ToMt:CycL),!.

*/
% ============================================
% Register isa/genls (more for testing :)
% ============================================



% examples
:-decl_mpred('BaseKB',isa,2).
:-decl_mpred('BaseKB',genls,2).
:-decl_mpred('BaseKB',genlMt,2).
:-decl_mpred('BaseKB',arity,2).


% ============================================
% Testing 
% ============================================
      
testOpenCyc:-halt.

% :- scan_arities.



% ===================================================================
% File 'utilities.pl'
% Purpose: Shared utilities for interfacing to OpenCyc from SWI-Prolog
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'utilities.pl' 1.0.0
% Revision:  $Revision: 1.3 $
% Revised At:   $Date: 2002/06/06 15:43:15 $
% ===================================================================
:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
% :- style_check(-string).

% ===================================================================
% ===================================================================
% ===================================================================

:-export(isSlot/1).
isSlot(Var):-var(Var).
isSlot('$VAR'(Var)):-number(Var).


% ===================================================================
% CycL Term Reader
% ===================================================================
:-dynamic reading_in_comment/0.
:-dynamic reading_in_string/0.
:-dynamic read_in_atom/0.

readCycL(CHARS):-readCycL(user_input,CHARS).

readCycL(Stream,[])  :-at_end_of_stream(Stream).     
readCycL(Stream,Trim)  :-
		flag('bracket_depth',_,0),
		retractall(reading_in_comment),
		retractall(reading_in_string),!,
		readCycLChars_p0(Stream,CHARS),!,trim4e2c(CHARS,Trim).

readCycLChars_p0(Stream,[]):-at_end_of_stream(Stream),!.
readCycLChars_p0(Stream,[Char|Chars]):-
        get_code(Stream,C),
	cyclReadStateChange(C),
	readCycLChars_p1(C,Char,Stream,Chars),!.
	
readCycLChars_p1(C,Char,Stream,[]):- at_end_of_stream(Stream),!.
readCycLChars_p1(C,Char,Stream,[]):-isCycLTerminationStateChar(C,Char),!.
readCycLChars_p1(C,Char,Stream,Chars):-cyclAsciiRemap(C,Char),
      flag(prev_char,_,Char),
      readCycLChars_p0(Stream,Chars),!.

isCycLTerminationStateChar(10,32):-reading_in_comment,!.
isCycLTerminationStateChar(13,32):-reading_in_comment,!.
isCycLTerminationStateChar(41,41):-flag('bracket_depth',X,X),(X<1),!.

cyclReadStateChange(_):- reading_in_comment,!.
cyclReadStateChange(34):-flag(prev_char,Char,Char),   % char 92 is "\\" and will escape a quote mark
      (Char=92 -> true;(retract(reading_in_string) ; assert(reading_in_string))),!.
cyclReadStateChange(_):- reading_in_string,!.
cyclReadStateChange(59):- assert(reading_in_comment),!.
cyclReadStateChange(40):-!,flag('bracket_depth',N,N + 1).
cyclReadStateChange(41):-!,flag('bracket_depth',N,N - 1).
cyclReadStateChange(_).

cyclAsciiRemap(X,32):- (not(number(X));X>128;X<32),!.
cyclAsciiRemap(X,X):-!.


% ===================================================================
% CycL Term Parser
% ===================================================================
/*===================================================================
% getSurfaceFromChars/3 does less consistancy checking then conv_to_sterm

Always a S-Expression: 'WFFOut' placing variables in 'VARSOut'

|?-getSurfaceFromChars("(isa a b)",Clause,Vars).
Clause = [isa,a,b]
Vars = _h70

| ?- getSurfaceFromChars("(isa a (b))",Clause,Vars).
Clause = [isa,a,[b]]
Vars = _h70

|?-getSurfaceFromChars("(list a b )",Clause,Vars)
Clause = [list,a,b]
Vars = _h70

| ?- getSurfaceFromChars("(genlMt A ?B)",Clause,Vars).
Clause = [genlMt,'A',_h998]
Vars = [=('B',_h998)|_h1101]

| ?- getSurfaceFromChars("(goals Iran  (not   (exists   (?CITIZEN)   (and    (citizens Iran ?CITIZEN)    (relationExistsInstance maleficiary ViolentAction ?CITIZEN
)))))",Clause,Vars).

Clause = [goals,Iran,[not,[exists,[_h2866],[and,[citizens,Iran,_h2866],[relationExistsInstance,maleficiary,ViolentAction,_h2866]]]]]
Vars = [=(CITIZEN,_h2866)|_h3347]


*/

:-export(getSurfaceFromChars/3).
getSurfaceFromChars([],[EOF],_):- end_of_file == EOF, !.
getSurfaceFromChars([41],[EOF],_):- end_of_file == EOF, !.

getSurfaceFromChars([CH|ARSIn],TERM,VARS):-
         trim4e2c([CH|ARSIn],CHARS),CHARS=[FC|REST],!,
	 (CHARS=[] -> % String came empty
	   TERM=nil; 
            FC=59 ->  % ";" Comment Char found in Line
	       (atom_codes(Atom,REST),TERM=[file_comment,Atom]) ;   
	       FC=40 -> %Use vanila CycL parser
                  getSurfaceFromCharBalanced(CHARS,TERM,VARS);  
		  %All above methods of parsing failed.. Convert to comment
		  (atom_codes(Atom,[FC|REST]),TERM=[file_comment,Atom])),!. 
	    
getSurfaceFromChars(C,TERM,VARS):-string_to_list(C,List),!,getSurfaceFromChars(List,TERM,VARS),!.


getSurfaceFromCharBalanced(Chars,WFFOut,VARSOut):- 
    retractall(var_counter(_)),retractall(numbered_var(_,_,_)),asserta(var_counter(0)), 
               getCycLTokens(Chars,Tokens), 
               clean_sexpression(Tokens,WFFClean),!,
               phrase(cycL(WFF),WFFClean),
               collect_temp_vars(VARS),!,
               ((VARS=[],VARSOut=_,WFFOut=WFF);
                    (unnumbervars_a2t(VARS,LIST),
                     cyclVarNums(LIST,WFF,WFFOut,VARSOut2) ,
                     list_to_set(VARSOut2,VARSOut1),
                     open_list(VARSOut1,VARSOut))),!.

/*===================================================================
% clean_sexpression(Tokens,CleanTokens)

Removes out STANDARD tokens

--*/

clean_sexpression([],[]).
clean_sexpression([''|WFF],WFFClean):-clean_sexpression(WFF,WFFClean).
clean_sexpression(['#'|WFF],WFFClean):-clean_sexpression(WFF,WFFClean).
clean_sexpression(['$'|WFF],WFFClean):-clean_sexpression(WFF,WFFClean).
clean_sexpression([E|WFF],[E|WFFClean]):-clean_sexpression(WFF,WFFClean).

/*===================================================================
% Safe Entry Call Into ISO-Prolog tokenize_chars/2
--*/

getCycLTokens(X,Z):-is_list(X),!,  tokenize_chars(X,Y),convert_the_atoms(Y,Z).
getCycLTokens(X,[X]). 

convert_the_atoms([],[]):-!.
convert_the_atoms([H|T],[HH|TT]):-!,  
                convert_the_atom(H,HH),
                convert_the_atoms(T,TT).

%convert_the_atom(H,HH):-atom_codes(H,[34|Rest]),reverse(Rest,[_|AtomCharsR]),reverse(AtomCharsR,AtomChars),atom_codes(HH,AtomChars).
%convert_the_atom(H,HH):-atom_codes(H,[39|Rest]),reverse(Rest,[_|AtomCharsR]),reverse(AtomCharsR,AtomChars),atom_codes(HH,AtomChars).
convert_the_atom(H,H):-!.



%===================================================================
% Removes Leading and Trailing whitespaces and non ANSI charsets.
%====================================================================
trim4e2c(X,Y):-ltrim(X,R),reverse(R,Rv),ltrim(Rv,RY),reverse(RY,Y).

ltrim([],[]):-!.
ltrim([32,32,32,32,32,32,32|String],Out) :-trim4e2c(String,Out),!.
ltrim([32,32,32,32,32|String],Out) :-trim4e2c(String,Out),!.
ltrim([32,32,32|String],Out) :- trim4e2c(String,Out),!.
ltrim([32,32|String],Out) :- trim4e2c(String,Out),!.
ltrim([P|X],Y):- (not(number(P));P<33;P>128),trim4e2c(X,Y),!.
ltrim(T,T).

/*===================================================================
%  CycL String to DCG Converter
% Converts up to 13 forms
%     13 Terms long
%  
% =169 Parens Pairs at the First 2 levels  
% 
--*/


cycL([A]) --> expr(A).
cycL([and,A|L]) --> expr(A) , cycL(L).

   %%expr(RF) --> reifiableFN(RF),!.
expr([]) -->  ['(',')'],!.
expr([Head]) -->  ['('],opr(Head),[')'],!.
expr([Head|LIST]) -->  ['('],opr(Head),many_slots(LIST),[')'].

many_slots([A]) --> slot(A).
many_slots([A|L]) --> slot(A) , many_slots(L).

opr(Head) --> simple(Head) .
opr(Head) --> expr(Head).

%slot(Name) --> simple(Name),['AssignmentFn'], { nonvar(Name), ! }.
slot(SKFName) --> ['SKF'],simple(Name), { nonvar(Name), ! , skf_name(Name,SKFName) }.
slot(WFF) -->  simple(WFF), { nonvar(WFF), ! }.
%slot(['AssignmentFn',Name,List]) -->  reifiableFN(['AssignmentFn',Name,List]).
slot(WFF) -->  expr(WFF), { nonvar(WFF), ! }.


expr(WFF) -->  variable(WFF), { nonvar(WFF) ,!}.
%expr(WFF) -->  reifiableFN(WFF), { nonvar(WFF),! }.   %slot(WFF) -->  literal_list(WFF), { nonvar(WFF) }.


variables_list([list,A]) --> qual_var(A).
variables_list([list,A]) -->  ['('],qual_var(A),[')'],!.
variables_list([list,A,B]) -->  ['('],qual_var(A),qual_var(B),[')'],! .
variables_list([list,A|QV]) -->  ['('],qual_var(A),many_qual_var(QV),[')'],!.
many_qual_var([A]) -->  qual_var(A).
many_qual_var([A|T]) -->  qual_var(A),many_qual_var(T).

% Var/Quality pairs that Sowa's ACE examples use

qual_var(VN) --> ['('],variable(VN),[')'].
qual_var(VN) --> variable(VN).
qual_var(VN) --> ['('],variable(VN),qual(_Quality),[')'].

qual(Q) --> constant(Q), { nonvar(Q) }. % , 'surface-instance'(_,Q,_) }.

number(Number) -->  [Number] , {  nonvar(Number), number(Number),! } .

quantity(Number) --> number(Number).

simple(WFF) -->  quantity(WFF), { nonvar(WFF), ! }.
simple(WFF) -->  variable(WFF), { nonvar(WFF), ! }.
simple(WFF) -->  constant(WFF), { nonvar(WFF), ! }.
%simple(['AssignmentFn',Name,[]]) --> ['SKF'],constant(Name).
%simple(['AssignmentFn',Name,[]]) --> ['SKF'],simple(Name),{ nonvar(Name) , nonvar(List), ! } .
%simple(['AssignmentFn',Name,[]]) --> ['AssignmentFn'],simple(Name), { nonvar(Name) , nonvar(List), ! } .

%reifiableFN(['AssignmentFn',SKFName,[]]) --> ['(','SKF'],simple(Name),[')'], { nonvar(Name) ,! , skf_name(Name,SKFName),sendNote('(skf)') } .
%reifiableFN(['AssignmentFn',SKFName,List]) --> ['(','SKF'],simple(Name),arbitrary(List),[')'], { nonvar(Name) , nonvar(List), ! , skf_name(Name,SKFName),sendNote('(skf)') } .
%reifiableFN(['AssignmentFn',Name,List]) --> ['(','AssignmentFn'],simple(Name),arbitrary(List),[')'], { nonvar(Name) , nonvar(List), ! } .
%%reifiableFN(['AssignmentFn',Name,_]) --> ['SKF'],simple(Name).
%reifiableFN(['AssignmentFn',Name,List]) --> ['('],simple(Name),arbitrary(List),[')'], { nonvar(Name) , nonvar(List),'surface-instance'(Name,'Function',_) ,! } .

skf_name(Num,SKFName):-!,number(Num), number_codes(Num,Codes),atom_codes(SKFName,[115,107|Codes]).

% Construct arbitrary list of args
          
arbitrary([]) -->  [].
arbitrary(VN)-->  ['?',A], { var_number(A,VN)   } . 
arbitrary([Head]) -->  slot(Head).
arbitrary([A|L]) --> slot(A) , many_slots(L).


variable(VN)-->  ['?',A], { var_number(A,VN)   } . 
variable(VN)-->  ['??'], { var_gen(A),var_number(A,VN)   } .     %Anonymous
variable(VN)-->  ['?'], { var_gen(A),var_number(A,VN)   } . 

idGen66(X):-flag(idGen,X,X+1).

% Makes up sequencial Variable names for anonymous cycl getPrologVars
var_gen(Atom):-idGen66(Number),number_codes(Number,Codes),atom_codes(Atom,[86,65,82|Codes]). % "VAR"

constant(Number) --> number(Number) .
   
constant(Unquoted) -->  [Unquoted] , {  nonvar(Unquoted), not((Unquoted='?';Unquoted='(';Unquoted=')')),! } .
     
var_number(A,'$VAR'(VN)):-numbered_var(A,'$VAR'(VN),_),!.
var_number(A,'$VAR'(VN)):-get_next_num(VN),assert(numbered_var(A,'$VAR'(VN),_)),!.

:-dynamic(numbered_var/3).

:-assert(var_counter(0)).

% This creates ISO Prolog getPrologVars w/in a CycL/STANDARD expression to be reconstrated as after parsing is complete 

get_next_num(VN):-!,retract(var_counter(VN)),NVN is VN +1,asserta(var_counter(NVN)).

cyclVarNums(LIST,'$VAR'(NUM),VAR,[=(SYM,VAR)]):-numbered_var(SYM,'$VAR'(NUM),_VAR),
               member(=(SYM,VAR),LIST).

cyclVarNums(_,Atom,Atom,[]):-atomic(Atom).
cyclVarNums(LIST,Term,NewTerm,VARLIST):-Term=..[F|ARGS],cyclVarNums_list(LIST,ARGS,VARARGS,VARLIST),NewTerm=..[F|VARARGS].

cyclVarNums_list(_LIST,[],[],[]).
cyclVarNums_list(LIST,[A|RGS],[V|ARARGS],VARLIST):-
            cyclVarNums(LIST,A,V,VARS1),
            cyclVarNums_list(LIST,RGS,ARARGS,VARS2),
            append(VARS1,VARS2,VARLIST).


unnumbervars_a2t(X,Y):-term_to_atom(X,A),atom_to_term(A,Y,_).

open_list(V,V):-var(V).
open_list(A,B):-append(A,_,B).

unnumbervars_nil(X,Y):-!,unnumbervars_a2t(X,Y).

collect_temp_vars(VARS):-!,(findall_nodupes(=(Name,Number),numbered_var(Name,Number,_),VARS);VARS=[]).

%================================================================
% STRING TOKENIZATION                            
%================================================================
:-assert(show_this_hide(tokenize,2)).

%tokenize_chars(M,['(',surf,')']):-nonvar(M),member(34,M),!.
tokenize_chars(X,Y):-once( tokenize3(X,Y) ).

tokenize3([],[]).
tokenize3([32|T],O):-!,tokenize3(T,O),!.
tokenize3(CharList,[Token|TList])  :- 
  append(_,[C|List],CharList), C \= 32,!,
  get_token([C|List],Token,Rest),!,
  tokenize3(Rest,TList),!.

get_token(List,Token,Rest)  :- 
  get_chars_type(List,Lchars,Rest,Type),!,
  type_codes(Type,Lchars,Token),!.

type_codes(num,CODES,Num):-catch(number_codes(Num,CODES),_,fail),!.
type_codes(_,[34|Lchars],string(S)):-!,atom_codes(S,[34|Lchars]).
type_codes(_,Lchars,Token):-!,atom_codes(Token,Lchars).

get_chars_type(L,S,L1,sep)  :-  separator(L,S,L1),!.
get_chars_type([C|L],[C|Lc],L1,S)  :- 
  check_start(S,C),
  get_word_chars(S,L,Lc,L1).

get_word_chars(S,L,Lc,L1)  :- 
  check_end(S,L,Lc,L1).
get_word_chars(S,[C|L],[C|Lc],L1)  :- 
  legal_char(S,C),
  get_word_chars(S,L,Lc,L1).

legal_char(num,C)    :-  digit(C).
legal_char(quote,C)  :-  not(bracket(_,C,_)).
legal_char(symb,C)   :-  valid_char(C).

check_start(Name,S):-bracket(Name,S,_E).
check_start(num, C)   :-  start_digit(C).
check_start(symb,C)   :- valid_char(C). %, 'not'(digit(C)).

check_end(_,[],[],[])  :-  !.
check_end(num, [C|L],[],[C|L])  :-  'not'(digit(C)),!.
check_end(Name,[E|L],[E],L)  :-  bracket(Name,S,E),!.
%check_end(symb,[C1,C2|L],[],[C1,C2|L])  :-  member([C1,C2],["Fn"]),!.
check_end(symb,[C|L],[],[C|L])  :-  'not'(valid_char(C)).

separator([C,D,E,F|L],[C,D,E],L)  :-member([C,D,E,F],["SKF-"]),!.
separator([C,D,E|L],[C,D,E],L)  :-member([C,D,E],[("<=>"),("=:="),("=\\="),"\\==","@=<","@>=","=..","-->","SKF"]),!.
separator([C,D|L],[C,D],L)  :-member([C,D],["=>",(":-"),"\\+","->","\\=","==","@<","@>","=<",">=","#$","//","??"]),!. %,"Fn"
separator([C|L],[C],L)  :- member(C,"*,():[];= < >^{}?%$#/"),!.

valid_char(C)  :-  letter(C); digit(C); C = 95 ; C=45 ; C=39.
letter(C)  :-   C=45 ; (97 =< C, C =< 122) ; (65 =< C, C =< 90) ; C = 95 .
start_digit(C)   :- member(C,"-01234567890").
digit(C)   :- member(C,"-.01234567890+eE").

%get_word([C|T],C,T)  :-  member(C,":,.?&%"),!. % ( : , . ?)
get_word([C|T],[C],T)  :- member(C,"=&"),!. % (=)
get_word([C,C1|T],[C,C1],T)  :- member([C,C1],["??"]),!. %"Fn",
get_word([C|T],[C|W],T2)  :-  bracket(_,C,C1),!,get_chars(0,C1,T,W,T2).
get_word([C|T],[C|W],T2)  :-  valid_start(C),!, get_chars(1,32,T,W,T2).

get_chars(K,C1,[C|T],[C|W],T2)  :-  valid_char(K,C,C1),!,get_chars(K,C1,T,W,T2).
get_chars(0,C,[C|T],[],T)  :- bracket(_,C,_), !.
get_chars(0,C,[C|T],[C],T)  :-  (C = 41; C = 93),!. % ) or ]
get_chars(1,_C1,[C|T],[],[C|T])  :-  member(C, [10,13|"=:,?"]).
%get_chars(2,_C1,[C,C2|T],[],[C,C2|T])  :-  member([C,C2], ["Fn"]).

valid_start(C)  :-  valid(C). %; C = 37.  % (%)
valid_char(K,C,C1)  :-  K = 0,!, C \= C1; K = 1, valid(C).

%bracket(quote,39,39).  % single quotes
bracket(quote,34,34).  % double quotes
%bracket(list,91,93).  % square brackets []
%bracket(quote,37,37).  % Literal Percent %%
%bracket(quote,35,35).  % Literal Percent ##

quote_found(0,B,B)  :-  member(B,[34]),!.
quote_found(Q,Q,0).

var_found(0,B,C)  :-  'not'(valid(B)),var_start(C).

var_start(C)  :-  (65 =< C,C =< 90);C = 95;C = 39.
valid(C)  :-   (65 =< C, C =< 90);    % A - Z
             (97 =< C, C =< 122);   % a - z
             (48 =< C, C =< 57);    % 0 - 9
             C = 95; C = 39;C = 45.  % underscore; hyphen


% ===================================================================

%%lowerCasePred(CycLPred)
:-export(lowerCasePred/1).
lowerCasePred(Pred):-atom(Pred),atom_codes(Pred,[_,_,C|_]),char_type(C,lower).


