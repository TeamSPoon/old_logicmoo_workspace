/* <module>
% an example of how simple parsing an inform7 like language is!
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- user:use_module(logicmoo(util/logicmoo_util_dcg)).

:-discontiguous(translation_spo/6).

glue_words(W):- member(W,[is,a,the,in,carries,'An','A','The',was,of,type]).

toCamelAtom(List,O):-notrace((not((member(IS,List),glue_words(IS))),toCamelAtom0(List,O))),!.

toCamelAtom0([A],O):-toPropercase(A,O),!.
toCamelAtom0([A|List],O):-toPropercase(A,AO),toCamelAtom(List,LO),atom_concat(AO,LO,O).


:-thread_local(loosePass/0).
:-thread_local(debugPass/0).

:-dynamic(parserVars/4).

asserta_parserVars(N,V,Type):-foc_current_agent(A),asserta(parserVars(A,N,V,Type)).
parserVars(N,V,Type):- foc_current_agent(A),
   (parserVars_local(A,N,V,Type)*->true;parserVars_falback(global,N,V,Type)).

parserVars_local(A,(N1;N2),V,Type):-!,parserVars_local(A,N1,V,Type);parserVars_local(A,N2,V,Type).
parserVars_local(A,N,V,Type):-parserVars(A,N,V,Type).

parserVars_falback(_,N,V,Type):-parserVars_local(global,N,V,Type).


isa(vRed,vtColor).

completelyAssertedCollection(vtValue).



isa(vtColor,ttValueType).
isa(X,ttValueType)=> (genls(X,vtValue),completelyAssertedCollection(X)).

isa(vtValue,ttValueType).

typeGenls(ttValueType,vtValue).


:-must(vtColor(vRed)).
:-must((isa(vRed,REDISA),genls(REDISA,vtValue))).

toCol(Txt,I,TCOL):-member(TCOL,[tCol,tObj,tSpatialThing,vtValue,ttTypeType]),show_call_success(toCol_0(Txt,I,TCOL)),!.

toCol_0(Txt,O,TCOL):-member(Pfx-Sfx-ISACISA, 
         [
          't'-''-'tSpec',
          ''-''-_,
          'tt'-'Type'-'ttTypeType',
          'vt'-''-'ttValueTypeType',
          'v'-''-'vtValue',
          'i'-'7'-'ttSpatialType',
          't'-'Able'-'ttAbilityType',
          'tt'-''-'ttTypeType'
           ]),atom_concat(Pfx,Txt,I),atom_concat(I,Sfx,O),isa(O,TCOL),!.

is_a --> is_was, [a].
is_a --> is_was.

is_was --> [is].
is_was --> [was].

is_in --> is_was, [in].
is_in --> is_was, [inside,of].
is_in --> is_was, [carried,by].

is_type_of --> is_a, [type,of].
is_type_of --> is_a, [type].

det(def) --> [the].
det(def) --> ['The'].
det(def) --> [some].
det(indef) --> [a].
det(indef) --> ['A'].
det(indef) --> [an].
det(indef) --> ['An'].

collection(I,Col,More)--> det(_),!,collection(I,Col,More).
collection(I,Col,true)--> collection0(I,Col).
collection(I,Col,More)--> attribute(Pred,I,Value,More),collection0(I,Col).
collection(I,Col,More)--> attribute(Pred,I,Value,More),{isa(I,Col)}.


collection0(I,Col)--> [A,B,C],{toCamelAtom([A,B,C],O),collection00(O,I,Col)}.
collection0(I,Col)--> [A,B],{toCamelAtom([A,B],O),collection00(O,I,Col)}.
collection0(I,Col)--> [O],{collection00(O,I,Col)}.

collection00(M,I,Col):-toCol(M,I,Col).
collection00(A,I,Col):-toPropercase(A,O),toCol(O,I,Col).
collection00(A,I,Col):-mudKeyword(M,W),string_equal_ci(A,W),toCol(M,I,Col).

subject(I,More)-->subject(I,_,More).
subject(I,T,true)-->(['This'];['this']),!,{must((parserVars(isThis,I,T);parserVars(_,I,T)))}.
subject(I,T,true)--> [IT],{string_equal_ci(IT,ITLC),parserVars(isParserVar(ITLC),I,T)},!.
subject(I,T,true)--> [IT],{string_equal_ci(IT,ITLC),parserVars((ITLC),I,T)},!.
subject(I,T,More)--> det(_),!,collection(I,T,More),{(asserta_parserVars(isThis,I,T))}.
subject(I,T,More)--> collection(I,T,More),{(asserta_parserVars(isThis,I,T))}.

object(I,More)-->object(I,_,More).
object(I,T,true)-->([it];['It'];['This'];['this']),!,{must((parserVars(object,I,T);parserVars(_,I,T)))}.
object(I,T,More)--> det(_),!,collection(I,T,More),{(asserta_parserVars(object,I,T))}.
object(I,T,More)--> collection(I,T,More),{(asserta_parserVars(object,I,T))}.

% big , red , flat, etc
attribute(Pred,I,C,t(Pred,I,C))--> [W],{ \+ glue_words(W),collection00(W,C,vtValue), isa(C,What),\=(What,vtValue),isa(What,ttValueType),argIsa(Pred,2,What)}.


dcgParse213(A1,A2,A3,S,E):-append([L|Left],[MidT|RightT],S),phrase(A2,[MidT|RightT],EE),     ((phrase(A1,[L|Left],[]),phrase(A3,EE,E))).
dcgParse213(A1,A2,A3,S,E):-debugPass,append([L|Left],[MidT|RightT],S),phrase(A2,[MidT|RightT],EE), trace, must((phrase(A1,[L|Left],[]),phrase(A3,EE,E))).

predicate(Pred,_Arg1Isa,_Arg2Isa)-->predicate0(Pred),{current_predicate(Pred/_),!}.
predicate(Pred,_Arg1Isa,_Arg2Isa)-->{loosePass},predicate0(Pred).
                               
predicate0(mudStowing)-->[carries].
predicate0(mudWielding)-->[wields].
predicate0(mudLikes)-->[likes].
predicate0(mudColor)-->[is,colored].
predicate0(localityOfObject)-->is_in.
predicate0(Pred)-->[has,Color],{i_name(mud,Color,Pred)}.
predicate0(isa)-->is_type_of.
predicate0(Pred)-->[is,the,Color],{i_name(mud,Color,Pred)}.
predicate0(Pred)-->[Likes],{atom_concat(Like,'s',Likes),i_name(mud,Like,Pred)}.
predicate0(Pred)-->[is,Colored],{atom_concat(Color,'ed',Colored),i_name(mud,Color,Pred)}.
predicate0(isa)-->is_a.
predicate0(mudRelates)-->is_was.
predicate0(isa)-->[is].

tCol('tRoom').

% :-ignore(show_call(phrase(collection(I,T,More),[red,room]))).

:-assertz_if_new(parserTest(iWorld7,"A television is in the living room.")).
translation_spo(Prolog,localityOfObject,I,C) --> dcgParse213(subject(I,More1),is_in,object(C,More2)),{conjoin(More1,More2,Prolog)}.


% :-assertz_if_new(parserTest(iKitchen7,"This is the red room.")).

:-assertz_if_new(parserTest(iWorld7,"The player carries the sack.")).
translation_spo(Prolog,Pred,I,C) --> dcgParse213(subject(I,Arg1Isa,More1),predicate(Pred,Arg1Isa,Arg2Isa),object(C,Arg2Isa,More2)),{conjoin(More1,More2,Prolog)}.

:-assertz_if_new(parserTest(iWorld7,"room is type of tRegion")).
translation_spo(Prolog,isa,I,C) --> dcgParse213(subject(I,tCol,More1),is_type_of,object(C,tCol,More2)),{conjoin(More1,More2,Prolog)}.

:-assertz_if_new(parserTest(iWorld7,"The Living room is a room.")).
tCol('tSack').

:-assertz_if_new(parserTest(iWorld7,"The sack is a container.")).
translation_spo(Prolog,isa,I,C) --> dcgParse213(subject(I,More1),is_a,object(C,tCol,More2)),{conjoin(More1,More2,Prolog)}.


:-assert_if_new(vtSize('vBulky')).

translation_spo(Prolog,isa,I,C) --> dcgParse213(subject(I,More1),is_was,object(C,_,More2)),{conjoin(More1,More2,Prolog)}.
translation_spo(Prolog,Pred,I,C) --> dcgParse213(subject(I,More1),is_was,attribute(Pred,I,C,More2)),{conjoin(More1,More2,Prolog)}.

:-assertz_if_new(parserTest(iWorld7,"A coffee table is in the living room.")).
%:-assertz_if_new(parserTest(iWorld7,"It is bulky.")).

tCol('tRemoteControl').
:-assertz_if_new(parserTest(iWorld7,"A remote control is in the living room.")).
:-assertz_if_new(parserTest(iWorld7,"A tv guide is a type of item.")).

tCol('tTvGuide').
:-assertz_if_new(parserTest(iWorld7,"A tv guide is in the living room.")).

%:-assertz_if_new(parserTest(iWorld7,"The paper clip is on the coffee table.")).

:-assertz_if_new(parserTest(iWorld7,"A tv guide is a type of book.")).

toplevel_type(CtxISA):-member(CtxISA,[tWorld,tRegion,tAgent,tItem,tObj,tCol,ftTerm]).

assert_text(CtxISA,String):- toplevel_type(CtxISA),
   must((isa(Ctx,CtxISA))),!,
   assert_text(Ctx,String).

assert_text(Ctx,String):-  
 must(show_call(once(((toplevel_type(CtxISA),
 isa(Ctx,CtxISA)))))),
 assert_text(Ctx,CtxISA,String).

assert_text(Ctx,CtxISA,String):-  
                            % context changed   and not the tWorld?                                                  % v this is for when there was no prior context
  (parserVars(context,Ctx0,_) -> (((Ctx0 \==Ctx),CtxISA\==tWorld) -> (asserta_parserVars(isThis,Ctx,CtxISA)); true) ; (asserta_parserVars(isThis,Ctx,CtxISA))), 
    with_assertions(parserVars(context,Ctx,CtxISA),assert_text_now(Ctx,CtxISA,String)).

assert_text_now(Ctx,CtxISA,String):-    
  % parse the string to attributed text
 to_word_list(String,WL),!,to_icase_strs(WL,IC),!, 
  
   must((phrase(translation_dbg_on_fail(Prolog),IC),show_call(onSpawn(Prolog)))).

to_icase_strs(WL,IC):-maplist(to_icase_str,WL,IC).

% to_icase_str(SL,IC):-string_to_atom(SL,SA),string_to_atom(SS,SA),when(?=(IC,Y),(trace,(Y=SA;Y=SS))).
to_icase_str(SL,IC):-string_to_atom(SL,SA),string_to_atom(SS,SA),when(nonvar(IC);?=(IC,IC),(IC=SA;IC=SS)).

translation(Prolog) --> translation_spo(More2,P,S,O),!,{conjoin(More2,t(P,S,O),Prolog)}.
translation(Prolog,WS,WE):-with_assertions(loosePass,translation_spo(More2,P,S,O,WS,WE)),conjoin(More2,t(P,S,O),Prolog).

translation_dbg_on_fail(Prolog)-->translation(Prolog),!.
translation_dbg_on_fail(Prolog,WS,WE):-with_assertions(debugPass,translation(Prolog,WS,WE)).

%:-assertz_if_new(parserTest(iWorld7,"Buffy the Labrador retriever is lounging here, shedding hair all over the place.")).
%:-assertz_if_new(parserTest(iWorld7,"You can also see a sugar candy doll house here.")).


user:type_action_info(tHumanPlayer,actAddText(isOptional(tTemporalThing,isThis),ftListFn(ftTerm)),"Development add some Text to a room.  Usage: addtext a sofa is in here").

user:agent_call_command(Agent,actAddText(What,StringM)):- 
 with_assertaions(parserVars(isThis,What,ftTerm),
   with_assertaions(parserVars(isSelfAgent,Agent,tAgent),   
       must(assert_text(What,StringM)))).

