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
          % isa_assert/3,
          gload/0,
          pgs/1,
          load_game/1
          ]).

:- meta_predicate show_load_call(0).

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
finish_processing_game:- ignore(scan_mpred_prop),fail.
finish_processing_game:- dmsg(begin_finish_processing_game),fail.
finish_processing_game:- retract(moo:call_after_load(A)),once(must(A)),fail.
finish_processing_game:- ignore(scan_mpred_prop),fail.
finish_processing_game:- retract(moo:call_after_load(A)),once(must(A)),fail.
finish_processing_game:- once(rescan_dbase_t),fail.
finish_processing_game:- once(scan_default_props),fail.
finish_processing_game:- once(rescan_dbase_t),fail.
finish_processing_game:- dmsg(end_finish_processing_game),fail.
finish_processing_game:- savedb,fail.
finish_processing_game:- retract(in_finish_processing_game).
finish_processing_game.

rescandb:-finish_processing_game.

% gload:- load_game(savedb),!.
gload:- load_game(logicmoo('rooms/startrek.all.pl')).

savedb:-
 catch((
   dbase_mod(DBM),
   tell('/tmp/savedb'),
   listing(DBM:_),told),E,dmsg(E)).

game_assert(':-'(A)):- must(A),!.
game_assert(d(s)):-dtrace.
game_assert(A):-must(once(correctArgsIsa(tell(game_assert),A,AA))),must(once(pgs(AA))),!.

% pgs(A):- fail, A=..[SubType,Arg], moo:createableType(SubType,Type),!,AA =.. [Type,Arg],
%      dbadd0(AA), assert_if_new(moo:call_after_load(create_instance(Arg,SubType,[debugInfo(moo:createableType(AA,SubType,Type))]))).   

pgs(somethingIsa(A,List)):-forall_member(E,List,game_assert(ofclass(A,E))).
pgs(somethingDescription(A,List)):-forall_member(E,List,game_assert(description(A,E))).
pgs(objects(Type,List)):-forall_member(I,List,game_assert(isa(I,Type))).
pgs(sorts(Type,List)):-forall_member(I,List,game_assert(subclass(I,Type))).
pgs(predicates(List)):-forall_member(T,List,assert(mpred_prop_g(T))).

pgs(description(A,E)):- must(once(add_description(A,E))).
pgs(nameStrings(A,S0)):- determinerRemoved(S0,String,S),!,game_assert(nameStrings(A,S)),game_assert(determinerString(A,String)).

% skip formatter types
pgs(A):- A=..[SubType,_],member(SubType,[string,action,dir]),!.
pgs(C):- C=..[SubType,Arg],isa(FT,formattype),functor(FT,SubType,A),(A==0->true;functor(Arg,_,A)),!.

pgs(A):- A=..[SubType,Arg], moo:createableType(SubType),!,
      dbadd0(A),
      assert_if_new(moo:call_after_load(create_instance(Arg,SubType,[]))).   

pgs(A):- A=..[SubType,_],
         define_type(SubType),
         fail.

pgs(A):- A=..[SubType,Arg], 
      member(SubType,[wearable]),
      assert_if_new(moo:call_after_load(create_instance(Arg,item,[isa(A,SubType)]))).   

pgs(A):- A=..[SubType,_],dmsg(todo(ensure_creatabe(SubType))),dbadd0(A),!.

pgs(W):-functor(W,F,A),functor(WW,F,A),mpred_prop_game_assert(WW),throw_safe(todo(pgs(W))).
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

:-export(determinerRemoved/3).
determinerRemoved(S0,Det,S):- detWithSpace(WithSpace,String),string_concat(WithSpace,S,S0),string_lower(String,Det).


:-export(assert_description/2).
assert_description(A,B):-add_description(A,B).

:-export(add_description/2).
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
add_description(A,S,S0,Ws,_Sents,_Words):-Ws>3,is_here_String(S),!,show_load_call(dbadd0(descriptionHere(A,S0))).
add_description(A,_S,S0,_Ws,_Sents,_Words):-show_load_call(dbadd0(description(A,S0))).

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

add_description_word(A,Word):- string_upper(Word,Word),string_lower(Word,Flag),string_to_atom(Flag,Atom),show_load_call(game_assert(isa(A,Atom))).
add_description_word(A,Word):- string_lower(Word,Word),show_load_call(game_assert(keyword(A,Word))).
add_description_word(A,Word):- string_lower(Word,Lower),show_load_call(game_assert(keyword(A,Lower))).


add_description_kv(A,K,V):- atom_concat('#$PunchingSomething ',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):- atom_concat('+',Key,K),!,add_description_kv(A,Key,V).
add_description_kv(A,K,V):-atom_to_value(V,Term),C=..[K,A,Term],show_load_call(game_assert(C)).


% =======================================================

show_load_call(dbadd0(A)):-
   correctArgsIsa(A,AA),
   show_call0(dbadd0(AA)).
show_load_call(game_assert(A)):-
   correctArgsIsa(A,AA),
   show_call0(game_assert(AA)).
show_load_call(C):-show_loader_call0(C).
show_loader_call0(C):-debugOnError(C). %% dmsg(show_load_call(C)),C.      

% :- finish_processing_game.

:- include(logicmoo('vworld/moo_footer.pl')).


