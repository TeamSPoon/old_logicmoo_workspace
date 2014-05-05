/** <module> 
% Game loading Utils
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- module(moo_loader, [
          finish_processing_game/0, 
          game_assert/1,
          isa_assert/3,
          gload/0,
          correctArgsIsa/2,
          pgs/1,
          load_game/1
          ]).

:- meta_predicate show_call(0).

:- include(logicmoo('vworld/moo_header.pl')).

dbadd0(C0):-db_op(a,C0).

load_game(File):-absolute_file_name(File,Path),see(Path),
   world_clear(current),
   repeat,
   read_term(Term,[double_quotes(string)]),
   game_assert(Term),
   Term=end_of_file,seen,!,
   finish_processing_game.

:-dynamic(in_finish_processing_game/0).

finish_processing_game:- in_finish_processing_game,!.
finish_processing_game:- assert(in_finish_processing_game),fail.
finish_processing_game:- ignore(scan_db_prop),fail.
finish_processing_game:- retract(moo:call_after_load(A)),once(must(A)),fail.
finish_processing_game:- ignore(scan_db_prop),fail.
finish_processing_game:- retract(moo:call_after_load(A)),once(must(A)),fail.
finish_processing_game:- savedb,fail.
finish_processing_game:- retract(in_finish_processing_game).
finish_processing_game.


% gload:- load_game(savedb),!.
gload:- load_game(logicmoo('rooms/startrek.all.pl')).

savedb:-
   dbase_mod(DBM),
   tell(savedb),listing(DBM:_),told.

discoverAndCorrectArgsIsa(_Prop,_N1,[],[]):-!.
discoverAndCorrectArgsIsa(Prop,N1,[A|Args],[AA|AArgs]):-
   %dbase:
   argIsa_call(Prop,N1,Type),
   must(isa_assert_g(A,Type,AA)),
   N2 is N1+1,
   discoverAndCorrectArgsIsa(Prop,N2,Args,AArgs).

isa_assert_g(A,Type,AA):-
  cmust(ground(A:Type)),
  gmust(ground(AA),isa_assert(A,Type,AA)).
  


fisa_assert(A,integer,AA):-!,isa_assert(A,int,AA).
fisa_assert(A,int,AA):- must(any_to_number(A,AA)).
fisa_assert(A,number,AA):- must(any_to_number(A,AA)).
fisa_assert(A,string,AA):- must(text_to_string(A,AA)).
fisa_assert(A,dir,AA):- must(string_to_atom(A,AA)).


isa_assert(A,Type,A):- once(var(A);var(Type)),!,trace,throw(failure(isa_assert(A,Type))).
isa_assert(A,Type,AA):-fisa_assert(A,Type,AA),!.
isa_assert(A,Type,AA):-format_complies(A,Type,AA),!.
isa_assert(O,argIsaFn(_,_),O):-!. %any_to_value(O,V).  %missing
isa_assert(A,type,A):-atom(A),define_type(A).
isa_assert(A,term,A):-!. %% must(ground(A)).
isa_assert([A|AA],list(T),LIST):-!,findall(OT,((member(O,[A|AA]),isa_assert_g(O,T,OT))),LIST).
isa_assert(A,list(T),[OT]):-!,isa_assert_g(A,T,OT).
isa_assert([],[],[]):-!.
isa_assert(A,Type,A):-atom(Type),
      dmsg(todo(isa_assert_type(Type))),
      define_type(Type),
      C=..[Type,A],game_assert(C),!.
isa_assert([H|T],[H2|T2],[H3|T3]):-!,
   isa_assert_g(H,H2,H3),isa_assert(T,T2,T3).
isa_assert(Args,Types,NewArgs):-compound(Args), compound(Types),
   functor(Args,F,N),functor(Types,F,N),functor(NewArgs,F,N),
   Args=..[F|ArgsL],
   Types=..[F|TypesL],
   NewArgs=..[F|NewArgsL],
   isa_assert(ArgsL,TypesL,NewArgsL).
isa_assert(Arg,Props,NewArg):- compound(Props),
   Props=..[F|TypesL],
   C=..[F,Arg|TypesL],
   correctArgsIsa(C,CC),
   CC=..[F,NewArg|_].
isa_assert(A,C,A):-must(ground(A)),dmsg(todo(define(isa_assert(A,C,'ConvertedArg')))),throw(retry(_)).

isa_assert(A,Type,_NewArg):-throw(failure(isa_assert(A,Type))).



holdsFunctor(k).
holdsFunctor(p).
holdsFunctor(holds).

correctArgsIsa(A,AA):-
   functor(A,_,1),!,
   must(any_to_value(A,AA)),
   cmust(ground(AA)).

correctArgsIsa(M:A,M:AA):-!,correctArgsIsa(A,AA).

correctArgsIsa(A,AA):-A =..[KP,Prop|Args],atom(Prop),holdsFunctor(KP),!,
   discoverAndCorrectArgsIsa(Prop,1,Args,AArgs),
   AA =..[Prop|AArgs].

correctArgsIsa(A,AA):-A =..[Prop|Args],
   discoverAndCorrectArgsIsa(Prop,1,Args,AArgs),
   AA =..[Prop|AArgs].

game_assert((':-'(A))):-hotrace(A),!.
game_assert(A):-must(once(correctArgsIsa(A,AA))),must(once(pgs(AA))),!.

% pgs(A):- fail, A=..[SubType,Arg], moo:createableType(SubType,Type),!,AA =.. [Type,Arg],
%      dbadd0(AA), assert_if_new(moo:call_after_load(create_instance(Arg,SubType,[debugInfo(moo:createableType(AA,SubType,Type))]))).   

pgs(somethingIsa(A,List)):-forall_member(E,List,game_assert(ofclass(A,E))).
pgs(somethingDescription(A,List)):-forall_member(E,List,game_assert(description(A,E))).
pgs(objects(Type,List)):-forall_member(I,List,game_assert(isa(I,Type))).
pgs(sorts(Type,List)):-forall_member(I,List,game_assert(subclass(I,Type))).
pgs(predicates(List)):-forall_member(T,List,assert(db_prop_g(T))).

pgs(description(A,E)):- must(once(add_description(A,E))).
pgs(nameStrings(A,S0)):- determinerRemoved(S0,String,S),!,game_assert(nameStrings(A,S)),game_assert(determinerString(A,String)).

% skip formatter types
pgs(A):- A=..[SubType,_],member(SubType,[string,action,dir]),!.
pgs(C):- C=..[SubType,Arg],isa_mc(FT,formattype),functor(FT,SubType,A),(A==0->true;functor(Arg,_,A)),!.

pgs(A):- A=..[SubType,Arg], member(SubType,[agent,item, type, region]),!,
      dbadd0(A),
      assert_if_new(moo:call_after_load(create_instance(Arg,SubType,[]))).   

pgs(A):- A=..[SubType,_],
         define_type(SubType),
         fail.

pgs(A):- A=..[SubType,Arg], 
      member(SubType,[wearable]),
      assert_if_new(moo:call_after_load(create_instance(Arg,item,[isa(A,SubType)]))).   

pgs(A):- A=..[SubType,_],dmsg(todo(ensure_creatabe(SubType))),dbadd0(A),!.

pgs(W):-functor(W,F,A),functor(WW,F,A),db_prop_game_assert(WW),throw_safe(todo(pgs(W))).
pgs(W):-dbadd0(W).
pgs(A):-fmt('skipping ~q.',[A]).

/*
"Lieutenant",
"Commander",
"Human",
"Player",
"Explorer Player",
"ACT_NICE_THIEF",
"AWARE",
"NOBACKSTAB",
"ACT_STAY_ZONE",
"MEMORY",
"HELPER",
"ACT_FRIEND",
"NOCHARM",
"NOSUMMON",
"NOSLEEP",
"NOBASH",
"NOBLIND",
"NPC_DETECT_INVIS",
"NPC_NOTRACK",
"+mudToHitArmorClass0: 1",
"mudMaxHitPoints: 18d18+4000",
"#$PunchingSomething mudBareHandDamage: 10d10+75",
"Player",
"Player",
"Human",
"Logged on player character",
"burgandy",
"starfleet",
"command",
"uniform",
"a burgandy Starfleet command uniform",
"A neatly folded burgandy Starfleet command uniform is lying here",
"armorLevel: 10",
"These uniforms are worn by command officers on Federation starships. It's kind of tight, but it looks pretty good",
"Red Uniform",
"a burgandy Starfleet command uniform"

*/
detWithSpace(WithSpace,String):-ddeterminer0(String),atom_concat(String,' ',WithSpace).
detWithSpace(WithSpace,String):-ddeterminer1(String),atom_concat(String,' ',WithSpace).
determinerRemoved(S0,Det,S):- detWithSpace(WithSpace,String),string_concat(WithSpace,S,S0),string_lower(String,Det).
add_description(A,S0):-string_concat('#$PunchingSomething ',S,S0),!,add_description(A,S).

add_description(A,S0):-determinerRemoved(S0,String,S),!,add_description(A,S),game_assert(determinerString(A,String)).
add_description(A,S0):-
   string_to_atom(S0,S),
   atomic_list_concat(Words,' ',S),
   atomic_list_concat(Sents,'.',S),!,
   length(Words,Ws),
   must(add_description(A,S,S0,Ws,Sents,Words)).
% "NOBACKSTAB","ACT_STAY_ZONE","MEMORY"
add_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,  
   atomic_list_concat([K,V],': ',S),!,add_description_kv(A,K,V).
add_description(A,S,_S0,Ws,_Sents,_Words):- Ws<3,
   atomic_list_concat([K,V],'=',S),!,add_description_kv(A,K,V).
%#$PunchingSomething mudBareHandDamage: 10d10+75

add_description(A,_S,_S0,1,_,[Word]):-add_description_word(A,Word).

add_description(A,S,S0,Ws,Sents,['#$PunchingSomething',B|C]):-add_description(A,S,S0,Ws,Sents,[B|C]).
add_description(A,S,S0,Ws,Sents,[Det,B|C]):-ddeterminer(Det,L),add_description(A,S,S0,Ws,Sents,[B|C]),game_assert(determinerString(A,L)).
add_description(A,S,S0,Ws,_Sents,_Words):-Ws>3,is_here_String(S),!,show_call(dbadd0(descriptionHere(A,S0))).
add_description(A,_S,S0,_Ws,_Sents,_Words):-show_call(dbadd0(description(A,S0))).

is_here_String(S):- atomic_list_concat_safe([_,is,_,here,_],S).
is_here_String(S):- atomic_list_concat_safe([_,here],S).
is_here_String(S):- atomic_list_concat_safe([_,is,here,_],S).


ddeterminer1('A').
ddeterminer1('An').
ddeterminer1('The').
ddeterminer0(a).
ddeterminer0(an).
ddeterminer0(the).
ddeterminer(L,L):-ddeterminer0(L).
ddeterminer(U,L):-string_lower(U,L),U\=L,!,ddeterminer0(L).

add_description_word(A,Word):- string_upper(Word,Word),string_lower(Word,Flag),string_to_atom(Flag,Atom),show_call(game_assert(isa(A,Atom))).
add_description_word(A,Word):- string_lower(Word,Word),show_call(game_assert(keyword(A,Word))).
add_description_word(A,Word):- string_lower(Word,Lower),show_call(game_assert(keyword(A,Lower))).


add_description_kv(A,K,V):- atom_concat('#$PunchingSomething ',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):- atom_concat('+',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):-atom_to_value(V,Term),C=..[K,A,Term],show_call(game_assert(C)).


% =======================================================

show_call(dbadd0(A)):-
   correctArgsIsa(A,AA),
   show_call0(dbadd0(AA)).
show_call(game_assert(A)):-
   correctArgsIsa(A,AA),
   show_call0(game_assert(AA)).
show_call(C):-show_call0(C).
show_call0(C):-debugOnError(C). %% dmsg(show_call(C)),C.      

% :- finish_processing_game.

:- include(logicmoo('vworld/moo_footer.pl')).


