/** <module> 
% Writer NPC_Interface  for supporting inforation based actions
%
%
% Douglas Miles
% Dec 13, 2035
%
%
*/
% :- module(world_text,[]).

:-export(fully_expand/2).

:- moo:begin_transform_moo_preds.

% live another day to fight (meaning repl_to_string/1 for now is in prolog)
% local_decl_db_prop(repl_writer(agent,term),[singleValued,default(default_repl_writer)]).
% local_decl_db_prop(repl_to_string(agent,term),[singleValued,default(default_repl_obj_to_string)]).

default_repl_writer(_TL,N,Type,V):-copy_term(Type,TypeO),ignore(TypeO=o),fmt('~q=(~w)~q.~n',[N,TypeO,V]).
default_repl_obj_to_string(O,Type,toString(TypeO,O)):-copy_term(Type,TypeO),ignore(TypeO=o).


% Check to see if last action was successful or not
:-export(success/2).
success(Agent,no) :-
	failure(Agent,_),!.
success(_,yes).

canUseEnglish:-true.

show_kb_preds(Agent,List):-
      ignore(atloc(Agent,LOC)),
      show_kb_preds(Agent,LOC,List).

show_kb_preds(Agent,LOC,List):-
      ignore(atloc(Agent,LOC)),
       locationToRegion(LOC,Region),
         once((thlocal:repl_writer(Agent,WPred);WPred=default_repl_writer)),
         once((thlocal:repl_to_string(Agent,ToSTR);ToSTR=default_repl_obj_to_string)),
        subst(List,region,Region,ListR),
        show_kb_via_pred(WPred,ToSTR,ListR),!.




show_kb_via_pred(_,_,[]).
show_kb_via_pred(WPred,ToSTR,[L|List]):-!,
   show_kb_via_pred(WPred,ToSTR,L),
   show_kb_via_pred(WPred,ToSTR,List).
show_kb_via_pred(WPred,ToSTR,L):-!,catch((ignore(show_kb_via_pred_0(WPred,ToSTR,L);dmsg(failed(show_kb_via_pred_0(WPred,L))))),E,dmsg(error_failed(E,show_kb_via_pred_0(WPred,L)))).

show_kb_via_pred_0(WPred,ToSTR,F=Call):- !,show_kb_via_pred_1(WPred,ToSTR,F,Call).
show_kb_via_pred_0(WPred,ToSTR,once(Call)):- !,functor(Call,F,_), show_kb_via_pred_1(WPred,ToSTR,F,once(Call)).
show_kb_via_pred_0(WPred,ToSTR,all(Call)):- !,functor(Call,F,_), show_kb_via_pred_1(WPred,ToSTR,F,all(Call)).
show_kb_via_pred_0(WPred,ToSTR,Call):- functor(Call,F,_), show_kb_via_pred_1(WPred,ToSTR,F,Call).

show_kb_via_pred_1(WPred,ToSTR,F,all(Call)):-!,show_kb_via_pred_2(WPred,ToSTR,F,Call).
show_kb_via_pred_1(WPred,ToSTR,F,once(Call)):-!,show_kb_via_pred_2(WPred,ToSTR,F,once(Call)).
show_kb_via_pred_1(_WPred,_ToSTR,_F,call(Call)):-!,debugOnError(call_expanded(Call)).
show_kb_via_pred_1(WPred,ToSTR,F,Call):-show_kb_via_pred_2(WPred,ToSTR,F,Call).

show_kb_via_pred_2(WPred0,ToSTRIn,F0,Call0):-
      wsubst(Call0,value(ToSTR),value,Call),
      ignore( ToSTR = (ToSTRIn) ),
      subst([WPred0,ToSTR,F0,Call],value,NewValue,[WPred,ToSTROut,F,GCall]),
      show_kb_via_pred_3(WPred,ToSTROut,F,_UnkType,GCall,NewValue).

show_kb_via_pred_3(WPred,ToSTR,fmt(SayIt),Type,GCall,NewValue):-!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(call_expanded(GCall),Error, NewValue=Error), 
             fmt(text(SayIt))),Count),
      (Count==[] ->
        fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f1,F,Type)); true),!.

show_kb_via_pred_3(WPred,ToSTR,fmt,Type,GCall,NewValue):-!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(call_expanded(GCall),Error, NewValue=Error), 
             fmt(GCall)),Count),
      (Count==[] ->
        fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f2,F,Type)); true),!.


show_kb_via_pred_3(WPred,ToSTR,output,Type,GCall,NewValue):-!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(call_expanded(GCall),Error, NewValue=Error), 
             fmt_holds_tcall(WPred,ToSTR,F,Type,NewValue)),Count),
      (Count==[] ->
        fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f3,F,Type)); true),!.


show_kb_via_pred_3(WPred,ToSTR,F,Type,GCall,NewValue):- canUseEnglish,!,
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(call_expanded(GCall),Error, NewValue=Error), 
             fmt(text(GCall))),Count),!,
      (Count==[] ->
        (fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f4,F,Type))); true),!.

show_kb_via_pred_3(WPred,ToSTR,F,Type,GCall,NewValue):-
  % dmsg(show_kb_via_pred_3(WPred,ToSTR,F,GCall,NewValue)),
      findall(NewValue,(catch(call_expanded(GCall),Error, NewValue=Error), 
             fmt_holds_tcall(WPred,ToSTR,F,Type,NewValue)),Count),
      (Count==[] ->
        fmt_holds_tcall(WPred,ToSTR,F,Type,notFound(f5,F,Type)); true),!.


fmt_holds_tcall(WPred,ToSTR,F,Type,NewValue):-nonvar(NewValue),flatten([NewValue],ValueList), [NewValue]\=ValueList,fmt_holds_tcall(WPred,ToSTR,F,Type,ValueList),!.
fmt_holds_tcall(WPred,ToSTR,N,Type,[V]):-fmt_holds_tcall_pred(WPred,ToSTR,N,Type,V),!.
fmt_holds_tcall(WPred,ToSTR,N,Type,[V|VV]):-remove_dupes([V|VV],RVs),reverse(RVs,Vs),fmt_holds_tcall_pred(WPred,ToSTR,N,Type,Vs),!.
fmt_holds_tcall(WPred,ToSTR,N,Type,V):-fmt_holds_tcall_pred(WPred,ToSTR,N,Type,V),!.

fmt_holds_tcall_pred(WPred,ToSTR,N,Type,[L|List]):-!, doall((member(V,[L|List]),fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V))).
fmt_holds_tcall_pred(WPred,ToSTR,N,Type,V0):-fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V0).

fmt_holds_tcall_pred_trans(WPred,ToSTR,N,Type,V0):-must((debugOnError(call(ToSTR,V0,Type,V)),!,debugOnError(call(WPred,_Tn,N,Type,V)))).

% ===========================================
% generatePhrase_local(+Term,-English).
% Generate english version of a message
% ===========================================
:-export(generatePhrase_local/2).

bugger:term_to_message_string(T,T):-var(T),!.
bugger:term_to_message_string(T,T):-!.
bugger:term_to_message_string(text(T),M):-debugOnError(generatePhrase_local(T,M)),!.
bugger:term_to_message_string(fmt(T),M):-debugOnError(generatePhrase_local(T,M)),!.
bugger:term_to_message_string(C,C):-compound(C),functor(C,F,_),is_leave_alone(F),!.
bugger:term_to_message_string((T),M):-failOnError(generatePhrase_local(T,M)),!.
bugger:term_to_message_string(T,T):-!.

is_leave_alone(exact_message).
is_leave_alone(todo).
is_leave_alone((error)).
is_leave_alone(parserm).
% is_leave_alone(F):- moo:is_db_prop(F,_,_),!,fail.
is_leave_alone(A):-failOnError((sub_atom(A,_,1,0,S),atom_number(S,_))),!.

moo:term_anglify(A,B):-local_term_anglify(A,B).
moo:term_anglify_np_last(Obj,T,String):- local_term_anglify_np_last(Obj,T,String).

generatePhrase_local(Term,String):- debugOnError(( fully_expand(Term,EnglishM),!,
          % fmt('FR0=~q~n',[fully_expand(Term,EnglishM)]),
          fully_expand(EnglishM,EnglishG),fix_grammar(EnglishG,English) , join_for_string(English,String))),!.

local_grammar_correction([are,is,here],[are,here]).
local_grammar_correction([you,is],[you,are]).
local_grammar_correction([you,Verb,is],[your,Verb,is]).

local_grammar_correction([at,right],[right]).
local_grammar_correction([room,are],[room,is]).
local_grammar_correction([in,region,here],[is,here]).
local_grammar_correction([X,X],[X]):-member(X,[is,are]).
 
get_grammar_correction(C1,C2):-
   local_grammar_correction(W1,W2),to_word_list(W1,C1),to_word_list(W2,C2).


fix_grammar(String,WordsO):-to_word_list(String,Ws),fix_grammar_0(Ws,Words),(Words\=Ws->fix_grammar_0(Words,WordsO);Words=WordsO),!.

fix_grammar_0([],[]).
fix_grammar_0(EnglishG,English):-
   get_grammar_correction(Before,After),
   append_ci(Before,Rest,EnglishG),
   append_ci(After,Rest,EnglishNew),
   fix_grammar_0(EnglishNew,English),!.
fix_grammar_0([Carry|EnglishG],[Carry|English]):-
   fix_grammar_0(EnglishG,English),!.

join_for_string(English,EnglishS):-failOnError(( flatten([English],EnglishF),list_to_atomics_list(EnglishF,EnglishA),atomics_to_string(EnglishA," ",EnglishS))),!.
join_for_string(English,English).

list_to_atomics_list(L,AL):-list_to_atomics_list0(L,AL),forall(member(E,AL),must(atomic(E))).

list_to_atomics_list0(Var,A):-var(Var),!,any_to_string(Var,A),!.
list_to_atomics_list0([E|EnglishF],[A|EnglishA]):-
   any_to_string(E,A),
   list_to_atomics_list0(EnglishF,EnglishA),!.
list_to_atomics_list0([],[]):-!.


fully_expand(I,OOF):-copy_term(I,C),flatten([C],FC),fully_expand_0(FC,O),flatten([O],OF),fully_expand_0(OF,OOF).

fully_expand_0(FC,O):-catch(fully_expand_1(FC,O),E,(trace,dmsg(exact_message(error_m(E,fully_expand_1(FC,O)))),fail)),!.
fully_expand_0(FC,O):-catch((trace,fully_expand_1(FC,O)),_,fail).

fully_expand_1(Var,Var):-var(Var),!.
fully_expand_1([],[]):-!.
% fully_expand_1(StringIsError,_Out):-string(StringIsError),!,trace,fail.
fully_expand_1([T|TT],FTAO):-local_term_anglify_first([T|TT],TA),flatten([TA],FTA),fully_expand_1(FTA,FTAO),!.
fully_expand_1([T|Term],Out):-!,
   fully_expand_2(T,E),
   fully_expand_1(Term,English),
   flatten_append(E,English,Out),!.

fully_expand_1(T,E):-fully_expand_2(T,E),!.

fully_expand_2(Var,Var):-var(Var),!.
fully_expand_2([],[]):-!.
% fully_expand_2(Var,Var):-not(compound(Var)),!.
fully_expand_2(T,FTAO):-local_term_anglify_first(T,TA),flatten([TA],FTA),fully_expand_1(FTA,FTAO).
fully_expand_2(StringIsOK,StringIsOK):-string(StringIsOK),!.
fully_expand_2([T|Term],Out):-!,
   fully_expand_2(T,E),
   fully_expand_1(Term,English),
   flatten_append(E,English,Out),!.
fully_expand_2(Pred,Pred):-!.
fully_expand_2(Pred,Out):-
   safe_univ(Pred,[F|ARGS]),
   fully_expand_1_l(ARGS,NEWARGS),
   safe_univ(Out,[F|NEWARGS]),!.

fully_expand_1_l([],[]):-!.
fully_expand_1_l([T|Term],[E|English]):-!,
   fully_expand_1(T,E),
   fully_expand_1_l(Term,English).


best_nl_phrase(List,Sorted):-predsort(best_nl_phrase,List,Sorted).

% longest_string(?Order, @Term1, @Term2)
best_nl_phrase(Order,TStr1,TStr2):-
   any_to_string(TStr1,Str1),string_length(Str1,L1),
   any_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L1-Str1,L2-Str2).

is_phrase_type(np).

local_term_anglify_first(T,TA):-compound(T),local_term_anglify(T,TA).
% local_term_anglify_first(FmtObj,String):-compound(FmtObj),functor(FmtObj,Fmt,_),isa_assert(FmtObj,Fmt,String),!.
local_term_anglify_first(T,TA):-enter_term_anglify(T,TA).


flatten_append(First,Last,Out):-flatten([First],FirstF),flatten([Last],LastF),append(FirstF,LastF,Out),!.

local_term_anglify(Var,[prolog(Var)]):- var(Var),!.
local_term_anglify([Var],[prolog([Var])]):- var(Var),!.

local_term_anglify(np(P),English):- local_term_anglify_np(P,English).
local_term_anglify(noun_phrase(P),English):- local_term_anglify_np(P,English).

local_term_anglify(notice(Who,What),[np(Who),notices,What]).
local_term_anglify(fN(Region,region),[(String)]):- nameStrings(Region,String),!.

local_term_anglify(fN(Region,region),[nameString1(String)]):- holds_t(nameStrings,Region,String),!.


local_term_anglify([P|L],English):-!, local_term_anglify(P,PE),local_term_anglify(L,LE),!,flatten_append(PE,LE,English),!.

local_term_anglify(HOLDS,English):-HOLDS=..[H,Pred,A|MORE],atom(Pred),is_holds_true(H),HOLDS2=..[Pred,A|MORE],!,local_term_anglify(HOLDS2,English).
local_term_anglify(HOLDS,[A,verbFn(Pred)|MORE]):-HOLDS=..[H,Pred,A|MORE],is_holds_true(H),!.
local_term_anglify(HOLDS,English):-HOLDS=..[H,Pred,A|MORE],is_holds_false(H),atom(Pred),HOLDS2=..[Pred,A|MORE],!,local_term_anglify(not(HOLDS2),English).
local_term_anglify(HOLDS,[false,that,A,verbFn(Pred)|MORE]):-HOLDS=..[H,Pred,A|MORE],is_holds_false(H),!.
local_term_anglify(not(HOLDS),[false,that,English]):-!,local_term_anglify(HOLDS,English).
local_term_anglify(notFound(FNum,F,Type),[no,FNum,TypeC,'-s',for,FC]):-copy_term(notFound(F,Type),notFound(FC,TypeC)),ignore(TypeC='type'),ignore(FC='whatever').
local_term_anglify(NPO,String):-NPO=..[NP,Obj],is_phrase_type(NP),!,enter_term_anglify(fN(Obj,NP),String).

local_term_anglify(fN(Obj,argIsaFn(_PathName,_NumTwo)),String):- enter_term_anglify(Obj,String),!.
local_term_anglify(cmdresult(Cmd,Whatnot),[the,command,result,of,Cmd,is,Whatnot]):-!.
local_term_anglify(string(Obj),[String]):-failOnError(any_to_string(Obj,StringUQ)),atomics_to_string(['"',StringUQ,'"'],"",String).
% enter_term_anglify(prolog(Obj),string(String)):-failOnError(any_to_string(Obj,StringUQ)),atomics_to_string(['(',StringUQ,')'],"",String).
local_term_anglify(atloc(Obj,LOC),String):-fully_expand([fN(Obj,np),is,at,fN(LOC,np)],String).
local_term_anglify(description(Obj,Term),[fN(Obj,np),description,contains,:,string(Term)]).
local_term_anglify(fN(Obj,X),String):- locationToRegion(Obj,Region), Obj \= Region, enter_term_anglify(fN(Region,X),String),!.
% should not have searched nouns yet
local_term_anglify(fN(Obj,T),String):- local_term_anglify_np(Obj,T,String),!.

local_term_anglify(done(Obj,Term),[fN(Obj,np),did,:,Term]).
local_term_anglify(failed(Obj,Term),[fN(Obj,np),didnt,:,Term]).
local_term_anglify(do(Obj,Term),[fN(Obj,np),begun,:,Term]).


% almost all else failed
local_term_anglify(fN(Obj,T),String):- anglify_noun_known(Obj,T,String),!.

% totally all else failed
% %enter_term_anglify(prolog(Obj),String):- any_to_string(Obj,StringUQ),!,atomics_to_string(['',StringUQ,''],"",String),!.
% %enter_term_anglify(Obj,Obj):-!.


moo:term_anglify_np(Obj,Hint,String):-local_term_anglify_np(Obj,Hint,String).

local_term_anglify_np(Obj,String):-isa(Obj,Isa),local_term_anglify_np(Obj,Isa,String),!.
local_term_anglify_np(Obj,String):-local_term_anglify_np(Obj,term,String).

% specific noun searching
local_term_anglify_np(Obj,Hint,String):- anglify_noun_known(Obj,Hint,String),!.
local_term_anglify_np(Obj,dir,Obj):- !.
local_term_anglify_np(string(Obj),string,Obj):- !.
local_term_anglify_np(Obj,string,Obj):- !.

local_term_anglify_np_last(Obj,Hint,String):- anglify_noun_known(Obj,Hint,String),!.
local_term_anglify_np_last(Obj,FT,String):- formattype(FT),correctFormatType(tell(_),Obj,FT,String),!.
local_term_anglify_np_last(Obj,Type,[prolog(Obj)]):-formattype(Type),!.
local_term_anglify_np_last(Obj,Type,[the,Type,prolog(Obj)]):-!.
local_term_anglify_np_last(apath(Region,Dir),_,[a,fN(Dir,dir),'-ern',way,from,fN(Region,np)]):-!.
local_term_anglify_np_last(Obj,Type,[prolog(Obj),fN,Type]):-!.
local_term_anglify_np_last(Obj,_,[the,noun,with,token,Obj]):-!.


% anglify_noun_known(Self,_Hint,[you]):- current_agent(Self),!.
anglify_noun_known(Obj,FT,String):- formattype(FT),correctFormatType(tell(_),Obj,FT,String),!.
anglify_noun_known(explorer(StringO),_Hint, [StringO]).
anglify_noun_known(Obj,_Hint,[right,here]):- current_agent(Self),atloc(Self,Obj),!.
anglify_noun_known(Obj,_Hint,[here]):- current_agent(Self),req(inRegion(Self,Obj)),!.
anglify_noun_known(Obj,_Hint,StringO):- findall(String,holds_t(nameStrings,Obj,String),List),List\=[],sort_by_strlen(List,[StringO|_]),!.
%anglify_noun_known(Obj,_Hint,String):-
%nameStrings(X,Y,_,_)


:-  moo:end_transform_moo_preds.


end_of_file.




someplace:
  the far end of roomOrName
  theMiddleOf roomOrName
  theMiddleOf roomOrName

roomOrName:
 the room
 main engineering
 Geordi's quarters

someone:
  you
  he
  she

buggers:
  sees
  fiddles with

something:

coverSurround:
  cover
  surround

Youfindyourself:
 You Find Yourself
 You're 
 You are

At someplace someone buggers  something,
   a something or another.

 Plurals coverSurround someRoomParts,
   
 and a large Whatnot built into the someRoomParts2 postures someplace.

 roomOrName is longerWider than it is longerWider2, and it has fairly low roomParts.  

 Youfindyourself in someplace. 

At the far end of the room you see the warp core, 
   a large pulsating vertical tube.  
      Computer terminals cover all the walls, 
      and a large table built into the floor sits in theMiddleOf the room.  
      Room is longer than it is wide, and it has fairly low ceilings.  
      YouFindYourself in theMiddleOf main engineering.  
A corridor is North.

A neatly made bed has been placed against the northern wall.  
A small personal computer sits on a desk against the western wall, in between two windows that look out into space.  
room is sparsely decorated, due to the fact that Geordi is blind.  
Youfindyourself in someplace.  
A corridor is East.




description('Area1000', "Main Engineering").
description('Area1000', "YouFindYourself in theMiddleOf main engineering").
description('Area1000', "room is longer than it is wide, and it has fairly low ceilings").
description('Area1000', "Computer terminals cover all the walls, and a large table built into the floor sits in theMiddleOf the room").
description('Area1000', "At the far end of the room you see the warp core, a large pulsating vertical tube").
description('Area1002', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1002', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1001', "Geordi's Quarters").
description('Area1001', "You're in theMiddleOf Geordi's quarters").
description('Area1001', "room is sparsely decorated, due to the fact that Geordi is blind").
description('Area1001', "small personal computer sits on a desk against the western wall, in between two windows that look out into space").
description('Area1001', "neatly made bed has been placed against the northern wall").
description('Area1005', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1005', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1005', "You notice a tiny computer panel embedded into the wall").
description('Area1003', "Data's Quarters").
description('Area1003', "You're in theMiddleOf Data's quarters").
description('Area1003', "Some easils and paintings have been left scattered around the southern part of the room, while a huge computer screen showing a cross section of the Enterprise covers the entire northern wall").
description('Area1003', "In front of the screen is a large desk, which is covered in computer controls").
description('Area1003', "You can't see a bed in this room, but you figure it's because Data doesn't sleep").
description('Area1004', "You're in the dimly lit Brig").
description('Area1004', "Three fairly large cells can been seen in the southern part of the room, and they're all empty").
description('Area1004', "panel says:\n\n***************************************************\n*                                                 *\n*            NCC-1701-D - ENTERPRISE              *\n*                                                 *\n*              *****                              *\n*      **********************                     *\n*      ***********************  _________         *\n*              *****        ***(___  ____(        *\n*                            ***** \\ \\*           *\n*                             **********          *\n*                                                 *\n*          You are currently on deck 1            *\n*                                                 *\n***************************************************\n").
description('Area1008', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1008', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1008', "You see the holodeck's control panel beside the holodeck door, and it has some information on it").
description('Area1006', "Transporter Room").
description('Area1006', "You're in the Enterprise transporter room").
description('Area1006', "Eight round transport pads have been arranged in a circle, on a raised platform against the northern wall").
description('Area1042', "Transporter Beam").
description('Area1042', "YouFindYourself in a transporter beam").
description('Area1042', "All you can see is blue flashing light").
description('Area1042', "It feels as though your body is racing around at high speeds").
description('Area1042', "As you try to look down at your body, you realize that there's nothing there!").
description('Area1007', "You step through the doors and find yourself in a large school room").
description('Area1007', "Various tables and chairs are set up all around the room, and many paintings and drawings have been attached to the walls").
description('Area1007', "Several computer consoles with a children's interface on them can be seen on the tables").
description('Area1010', "You're in the turbolift").
description('Area1010', "turbolift walls have been rounded off, making it in the shape of a tube").
description('Area1010', "Several vertical rows of lights make this place very well lit").
description('Area1010', "From here, you can access the other decks on the Enterprise").
description('Area1009', "Holodeck 2").
description('Area1009', "You're now on Holodeck 2").
description('Area1009', "room is just a large cube, with jet black walls and a yellow grid painted on the floors, the walls, and the ceiling").
description('Area1009', "Right now, this holodeck is not functioning").
description('Area1009', "\n***************************************************\n*                                                 *\n*            NCC-1701-D - \"ENTERPRISE\"            *\n*                    HOLODECK 2                   *\n*                                                 *\n*              STATUS : Inactive                  *\n*     CURRENT PROGRAM : N/A                       *\n*            SAFETIES : N/A                       *\n*                                                 *\n*    NOTE: Starfleet is not responsible for       *\n*          any injuries incurred while on this    *\n*          holodeck!                              *\n*                                                 *\n* WARNING: While the safeties are disabled, you   *\n*          CAN be injured, or even killed.        *\n*                                                 *\n***************************************************").
description('Area1011', "You're in the turbolift").
description('Area1011', "turbolift walls have been rounded off, making it in the shape of a tube").
description('Area1011', "Several vertical rows of lights make this place very well lit").
description('Area1011', "From here, you can accessthe other decks on the Enterprise").
description('Area1013', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1013', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1013', "You notice a tiny computer panel embedded into the wall").
description('Area1032', "You're in the turbolift").
description('Area1032', "turbolift walls have been rounded off, making it in the shape of a tube").
description('Area1032', "Several vertical rows of lights make this place very well lit").
description('Area1032', "From here, you can access the other decks on the Enterprise").
description('Area1012', "Cargo Bay 1").
description('Area1012', "You're in the main cargo bay of the Enterprise").
description('Area1012', "It's quite a large room, with a very high ceiling and a lot of floor space").
description('Area1012', "You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling").
description('Area1016', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1016', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1016', "You see the holodeck's control panel beside the holodeck door, and it has some information on it").
description('Area1014', "Riker's Quarters").
description('Area1014', "You've arrived in Riker's quarters").
description('Area1014', "room is very neat and tidy, with a couch and several chairs aranged around a coffee table by the eastern wall").
description('Area1014', "small partition at the northern part of the room seperates his sleeping area with the rest of the room").
description('Area1015', "Sick Bay").
description('Area1015', "You're in theMiddleOf the Enterprise's Sick Bay").
description('Area1015', "About a dozen beds are arranged along the walls of the room, while several carts covered with medical supplies are scattered around the room").
description('Area1015', "large glass window in the northern part of the room separates the doctor's office with the rest of the room").
description('Area1015', "\n***************************************************\n*                                                 *\n*            NCC-1701-D - \"ENTERPRISE\"            *\n*                    HOLODECK 4                   *\n*                                                 *\n*              STATUS : Active                    *\n*     CURRENT PROGRAM : Sherlock Holmes (19th     *\n*                       century London)           *\n*            SAFETIES : Disabled                  *\n*                                                 *\n*    NOTE: Starfleet is not responsible for       *\n*          any injuries incurred while on this    *\n*          holodeck!                              *\n*                                                 *\n* WARNING: While the safeties are disabled, you   *\n*          CAN be injured, or even killed.        *\n*                                                 *\n*             ---ENTER WHEN READY---              *\n*                                                 *\n***************************************************\n").
description('Area1019', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1019', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1017', "Holodeck 4 Entrance - A Narrow Alley").
description('Area1017', "You emerge into a dark narrow alley").
description('Area1017', "Tall dark brick buildings block your way north and south").
description('Area1017', "You can see that the windows on the buildings are fairly high, and some have been boarded up").
description('Area1017', "smell from the rotting food and garbage mixing with the foul water on the ground is unbearable").
description('Area1017', "You can hear the sounds of a bustling marketpace to the east").
description('Area1017', "archway leading out of the holodeck is west").
description('Area1018', "Crusher's Quarters").
description('Area1018', "You're in Doctor Crusher's quarters").
description('Area1018', "Several different paintings are attached to the walls, and you also notice a few sculptures").
description('Area1018', "neatly made bed is located by the northern wall, in between two large windows looking out into space").
description('Area1021', "Ten Forward").
description('Area1021', "You're now in Ten Forward, the entertainment room of the Enterprise").
description('Area1021', "entire northern wall is covered with windows looking out into space, while two large wooden doors with the Starfleet insignia stamped on them face south").
description('Area1021', "Many round metal tables are scattered around the room, surrounded by metal chairs").
description('Area1021', "long bar spans almost the entire length of the southern part of the room, and about two dozen bar stools are sitting in front of it").
description('Area1020', "Enterprise Security").
description('Area1020', "You're standing in the dimly lit Enterprise Security").
description('Area1020', "Weapons lockers cover all of the walls, except along the northern wall, where a large glass window protects dozens of different phasors, blaster rifles, and other high tech weapons").
description('Area1020', "Three long tables surrounded by chairs stretch across the room").
description('Area1022', "Shuttle Bay").
description('Area1022', "You're in the main shuttle bay of the Enterprise").
description('Area1022', "It's quite a large room, with a very high ceiling and a lot of floor space").
description('Area1022', "You can see three different shuttle crafts sitting here, waiting to be flown").
description('Area1022', "large grey door leads into space").
description('Area1024', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1024', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1024', "You notice a tiny computer panel embedded into the wall").
description('Area1039', "Outer Space by the Enterprise").
description('Area1039', "You're floating in outer space right beside the USS Enterprise").
description('Area1039', "You feel very cold").
description('Area1039', "large grey door leads into the Enterprise's Shuttle Bay").
description('Area1023', "Troi's Quarters").
description('Area1023', "You're in Counselor Deanna Troi's quarters").
description('Area1023', "Several different paintings have been hung from the walls, and a small couch and a recliner are positioned around a coffee table").
description('Area1023', "neatly made bed is partially hidden behind a curtain at the northern part of the room").
description('Area1027', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1027', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1027', "\n***************************************************\n*                                                 *\n*            NCC-1701-D - ENTERPRISE            *\n*                                                 *\n*              *****                              *\n*      **********************                     *\n*      ***********************  _________         *\n*              *****        ***(___  ____(        *\n*                            ***** \\ \\*           *\n*                             **********          *\n*                                                 *\n*          You are currently on deck 3            *\n*                                                 *\n***************************************************\n").
description('Area1025', "Worf's Quarters").
description('Area1025', "You're in Worf's quarters").
description('Area1025', "something1 is sitting in theSoutheasternCorner, and on it is a small potted plant").
description('Area1025', "impressive selection of Klingon weapons have been mounted on the northern wall, and a partition splits this room with Worf's bedroom to the east").
description('Area1026', "Enterprise Gym").
description('Area1026', "You emerge into the Enterprise gym").
description('Area1026', "room is quite large, with a soft grey floor").
description('Area1026', "set of lockers against the southern wall contain all of the necessary equipment needed for using the gym").
description('Area1026', "thick stack of mats have been piled high in one corner, which can be used for different activities").
description('Area1026', "Captain Picard likes to come here to practice his fencing").
description('Area1030', "YouFindYourself in theMiddleOf a well lit corridor on the Enterprise").
description('Area1030', "It isn't very wide, and the light beige walls have been rounded, making the corridor an oval shape").
description('Area1028', "Picard's Quarters").
description('Area1028', "YouFindYourself standing by the door of Captain Picard's quarters").
description('Area1028', "He isn't very fond of visitors, but you decide to stay and have a look around").
description('Area1028', "You can see several different ancient artifacts on tables and small pedestals, and a large wooden wardrobe is facing south").
description('Area1028', "comfortable looking recliner with a matching footrest sits beside the door, along with a bright reading lamp and end table").
description('Area1028', "Two large windows offer a great view of space").
description('Area1028', "small partition at the northern part of the room contains Picard's sleeping area").
description('Area1029', "Science Lab").
description('Area1029', "You're in the Enterprise science lab").
description('Area1029', "strange looking machine sits in theMiddleOf the room, up on a slightly raised platform").
description('Area1029', "It looks as though something(or someone) could be placed inside, hooked up to the multitude of wires and cables, and have scientific tests performed on it(or them)").
description('Area1029', "complex looking computer console is facing this machine").
description('Area1029', "Around the rest of the room are counterops with with the odd computer terminal").
description('Area1031', "Cargo Bay 2").
description('Area1031', "You're in the cargo bay 2 of the Enterprise").
description('Area1031', "It's quite a large room, with a very high ceiling and a lot of floor space").
description('Area1031', "You can see several hundred plastic crates and barrels with the Starfleet insignia on them stacked right up to the ceiling").
description('Area1033', "You're in the turbolift").
description('Area1033', "turbolift walls have been rounded off, making it in the shape of a tube").
description('Area1033', "Several vertical rows of lights make this place very well lit").
description('Area1033', "From here, you can access the other decks on the Enterprise").
description('Area1034', "You're in the turbolift").
description('Area1034', "turbolift walls have been rounded off, making it in the shape of a tube").
description('Area1034', "Several vertical rows of lights make this place very well lit").
description('Area1034', "From here, you can access the other decks on the Enterprise").
description('Area1036', "Main Bridge - Upper Half").
description('Area1036', "YouFindYourself on the upper half of the main bridge of the USS Enterprise").
description('Area1036', "Directly in front of you is a thick railing that contains many different computer panels used for the tactical systems of the ship").
description('Area1036', "Two small curved ramps on either side of the room lead north to the lower part of the bridge, and a large circular skylight shows the space outside the ship").
description('Area1035', "Picard's Ready Room").
description('Area1035', "You're standing in Captain Picard's ready room").
description('Area1035', "long couch has been placed beside the door, while a large U shaped desk is located by the northern wall").
description('Area1035', "small computer screen is sitting on the desk, as well as several other papers and documents").
description('Area1035', "single high window beside the desk looks into space, and a fish tank is located in the northwestern corner of the room").
description('Area1038', "Main Bridge - Lower Half").
description('Area1038', "YouFindYourself on the lower half of the main bridge of the USS Enterprise").
description('Area1038', "enormous view screen covers almost the entire northern wall, and is currently displaying a view of the stars rushing by").
description('Area1038', "Three large chairs in the northern part of the room, in front of the railing, face the screen").
description('Area1038', "Two computer consoles with built in chairs rest about ten feet in front of the chairs, also facing the view screen").
description('Area1037', "Conference Room").
description('Area1037', "You're in the conference room of the Enterprise").
description('Area1037', "large glass rectangular table sits in theMiddleOf the room, surrounded by about a dozen comfortable looking office chairs").
description('Area1037', "entire eastern wall is covered with windows, looking out into space").
description('Area1040', "Outer Space").
description('Area1040', "You're floating in outer space right above the USS Enterprise").
description('Area1040', "You feel very cold").
description('Area1041', "Outer Space").
description('Area1041', "You're floating in outer space right above the USS Enterprise").
description('Area1041', "You feel very cold").


