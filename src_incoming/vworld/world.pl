% vworld.01
% May 18, 1996
% written by John Eikenberry
% interface by Martin Ronsdorf
% general assistance Dr. Donald Nute
%
% Any questions about this mail to jae@ai.uga.edu
% Dec 13, 2035
% Douglas Miles
%
/** <module>
% Common place to reduce redundancy World utility prediates
% **************IMPORTANT**************
% To customize your world, you need to modify wstart.pl
%
% *.objects.pl / *.map.pl is defined in wstart.pl for ease of setting up and testing.
% ****************************************
*/
:- module(world,
	[
        call_agent_command/2,
        call_agent_action/2,
            mud_isa/2,
            isa_any/2,
            put_in_world/1,
            get_session_id/1,
            pathBetween_call/3,
            obj_memb/2,
            prop_memb/2,            
            move_dir_target/3,
            create_instance/2,create_instance/3,
            create_agent/1,
            create_agent/2,
            in_world_move/3, check_for_fall/3,
            agent_into_corpse/1, display_stats/1,
            reverse_dir/2,
            same/2,
            round_loc/8,
            round_loc_target/8,
            dir_offset/5,
            number_to_dir/3,
            isa_mc/2,
            list_agents/1,
            agent_list/1,
            check_for_fall/3,
            list_object_dir_sensed/4,
            list_object_dir_near/3,
            num_near/3,
            asInvoked/2,
            define_type/1,
            findall_type_default_props/3,
         init_location_grid/1,
         grid_dist/3,
         to_3d/2,
         is_3d/1,
         in_grid/2,
         loc_to_xy/4,
         grid_size/4,
         doorLocation/5,
         foc_current_player/1,
         locationToRegion/2,
         init_location_grid/2,
         set_stats/2,
         worth/3,
         spread/0,
         growth/0,
         isaOrSame/2

 ]).


:- dynamic  agent_list/1.

:- include(logicmoo('vworld/vworld_header.pl')).

:- include(logicmoo('vworld/events.pl')).
:- include(logicmoo('vworld/room_grids.pl')).
:- include(logicmoo('vworld/effects.pl')).
:- include(logicmoo('vworld/spawning.pl')).

:- register_module_type(utility).


get_session_id(main):-thread_self(main),!.
get_session_id(ID):-thread_self(ID),thlocal:current_agent(ID,_),!.
get_session_id(O):-current_input(O),!.

foc_current_player(P):- get_session_id(O),thlocal:current_agent(O,P),!.
foc_current_player(P):- get_session_id(O),generate_new_player(P), !, asserta(thlocal:current_agent(O,P)).

generate_new_player(P) :-
  gensym(player,N),
   P=explorer(N),
   must(create_agent(P)).


% Lists all the agents in the run. Except for other monsters.
list_agents(Agents) :- agent_list(Agents), !.
list_agents(Agents) :- % build cache
	findall(NearAgent,req(agent(NearAgent)),Agents),
	assert(agent_list(Agents)),!.

% When an agent dies, it turns into a corpse.
% corpse is defined as an object in the *.objects.pl files
agent_into_corpse(Agent) :-
	del(atloc(Agent,LOC)),
	clr(str(Agent,_)),
	clr(height(Agent,_)),
	clr(stm(Agent,_)),
	clr(spd(Agent,_)),
	add(atloc(corpse(Agent),LOC)).

% Displays all the agents stats. Used at end of a run.
display_stats(Agents) :-
	forall(member(Agent,Agents),
	          (charge(Agent,Chg),
		  damage(Agent,Dam),
		  score(Agent,Scr),
		  findall(Obj,possess(Agent,Obj),Inv),
		  write('Agent = '), write(Agent), nl,
		  write('Charge = '), write(Chg), write('  '),
		  write('Dam= ' ), write(Dam), write('  '),
		  write('Score = '), write(Scr), nl,
		  write('Inventory = '), write(Inv), nl)).

is_property(P,A):- db_prop(_,C),functor(C,P,A2),A is A2-1.

is_type(O):-is_type0(O).
is_type0(T):-moo:label_type_props(_,T,_).
is_type0(T):-moo:type_default_props(_,T,_).
is_type0(OT):- moo:decl_subclass(OT,_); moo:decl_subclass(_,OT).
is_type0(food).
is_type0(explorer).
is_type0(predator).
is_type0(prey).
is_type0(monster).
is_type0(dir).
is_type0(agent).

isaOrSame(A,B):-A==B,!.
isaOrSame(A,B):-mud_isa(A,B).

intersect(A,EF,B,LF,Tests,Results):-findall( A-B, ((member(A,EF),member(B,LF),once(Tests))), Results),[A-B|_]=Results.
% is_property(P,_A),PROP=..[P|ARGS],CALL=..[P,Obj|ARGS],req(CALL).
obj_memb(E,L):-member(E,L).
isa_any(E,L):-flatten([E],EE),flatten([L],LL),!,intersect(A,EE,B,LL,isaOrSame(A,B),_Results).
prop_memb(E,L):-flatten([E],EE),flatten([L],LL),!,intersect(A,EE,B,LL,isaOrSame(A,B),_Results).

exists(O):-item(O).
exists(O):-agent(O).
exists(O):-region(O).
anyInst(O):-type(O).
anyInst(O):-exists(O).

metaclass(regiontype).
metaclass(agenttype).
metaclass(itemtype).
metaclass(formattype).


isa_mc(region,regiontype).
isa_mc(agent,agenttype).
isa_mc(item,itemtype).

isa_mc(FT,formattype):-moo:decl_ft(FT,_).

moo:decl_subclass(SubType,formattype):-isa_mc(SubType,formattype).

not_mud_isa(agent,formattype).
cached(G):-catch(G,_,fail).

mud_isa(O,T):- O==T,!.
mud_isa(O,T):- var(O),var(T),!,isa_mc(T,_),anyInst(O),mud_isa(O,T).
mud_isa(O,T):-cached(not_mud_isa(O,T)),!,fail.
mud_isa(O,T):- props(O,classof(T)).
mud_isa(O,T):- props(O,isa(T)).
mud_isa(O,T):- compound(O),once(var(T);atom(T)),functor(O,T,_).
mud_isa(O,T):- atom(O),atom(T),not(is_type(O)),is_type(T),atom_concat(T,_Int,O),!. %catch(number_codes(Int,_),_,fail),!.

define_subtype(O,T):-assert_if_new(moo:decl_subclass(O,T)).

findall_type_default_props(Inst,Type,TraitsO):-findall(Props,moo:type_default_props(Inst,Type,Props),Traits),flatten(Traits,TraitsO),!.

create_meta(T,P,C,MT):-
   must(split_name_type(T,P,C)),
   define_subtype(C,MT),
   OP =.. [MT,P],
   assert_if_new(OP),
   must(forall_member(E,[OP,classof(P,MT),classof(P,C)],must(add(E)))),
   must(findall_type_default_props(P,C,Props)),!,
   must(padd(P,Props)),!.

rez_to_inventory(Whom,T,P):-
   create_meta(T,P,_,item),
   padd(Whom,possess(P)).


create_instance(P,What):-create_instance(P,What,[]).

moo:decl_subclass(wearable,item).
moo:decl_subclass(knife,item).
moo:decl_subclass(food,item).

moo:decl_createableType(FT):- formattype(FT),!,fail.
moo:decl_createableType(item).
moo:decl_createableType(S,T):- moo:createableType(T),req(subclass(S,T)).
moo:decl_createableType(T,T):-nonvar(T),moo:createableType(T).

:- style_check(-discontiguous).

moo:decl_subclass(int,formattype).
moo:decl_subclass(dir,formattype).
moo:decl_subclass(number,formattype).
moo:decl_subclass(string,formattype).


create_agent(P):-create_agent(P,[]).
create_agent(P,List):-must(create_instance(P,agent,List)).

formattype(FormatType):-moo:decl_subclass(FormatType,formattype).
formattype(FormatType):-mud_isa(FormatType,formattype).

define_type(Spec):-create_instance(Spec,type,[]).
create_instance(What,FormatType,List):- FormatType\==type,
   formattype(FormatType),!,
   throw(formattype(FormatType,create_instance(What,FormatType,List))).

create_instance(SubType,type,List):-!,
   add(isa(SubType,type)),
   dbase_mod(M),
   A = M:type(SubType),
   assert_if_new(A),
   padd(SubType,List).

moo:decl_createableType(agent).
moo:decl_subclass(actor,agent).
create_instance(T,agent,List):-!,
   retractall(agent_list(_)),
   must(create_meta(T,P,_,agent)),
   must(padd(P,List)),
   clr(atloc(P,_)),
   must(put_in_world(P)),
   rez_to_inventory(P,food,_Food),
   max_charge(NRG),
   max_damage(Dam),
   add(charge(P,NRG)),
   add(damage(P,Dam)),
   add(act_turn(P,0)),
   add(score(P,0)),
   set_stats(P,List),
   add(memory(P,directions([n,s,e,w,ne,nw,se,sw,u,d]))),!.

moo:decl_createableType(region).
create_instance(T,Type,List):- moo:createableType(Type),
   create_meta(T,P,_,Type),!,
   padd(P,List).

create_instance(T,Type,List):-moo:subclass(Type,MetaType),moo:createableType(MetaType),
   create_meta(T,P,_,MetaType),
   padd(P,isa(Type)),
   padd(P,List),
   clr(atloc(P,_)),
   put_in_world(P).

moo:decl_createableType(type).

split_name_type(T,T,C):-compound(T),functor(T,C,_),!.
split_name_type(T,T,C):-atom(T),atom_codes(T,AC),last(AC,LC),is_digit(LC),append(Type,Digits,AC),catch(number_codes(_,Digits),_,fail),atom_codes(C,Type),!.
split_name_type(C,P,C):-atom(C),C==food,gensym(C,P),!.
split_name_type(C,P,C):-atom(C),trace,gensym(C,P),!.
split_name_type(C,P,C):-string(C),trace,gensym(C,P),!.

same(X,Y):- X=Y,!.
same(X,Y):- compound(X),arg(1,X,Y),!.
same(X,Y):- compound(Y),arg(1,Y,X),!.

moo:type_default_props(_,agent,last_command(stand)).


:- include(logicmoo('vworld/vworld_footer.pl')).
