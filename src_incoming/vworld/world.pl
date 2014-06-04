/** <module>
% Common place to reduce redundancy World utility prediates
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
% Special thanks to code written on
% May 18, 1996
% written by John Eikenberry
% interface by Martin Ronsdorf
% general assistance Dr. Donald Nute
%
*/

:- module(world,
	[
        call_agent_command/2,
        call_agent_action/2,
            world_mud_isa/2,
            isa_any/2,
          create_meta/4,
            put_in_world/1,
            get_session_id/1,
            pathBetween_call/3,
            obj_memb/2,
            prop_memb/2,
            move_dir_target/3,
            in_world_move/3, check_for_fall/3,
            agent_into_corpse/1, display_stats/1,
            reverse_dir/2,
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

          show_kb_via_pred/3,
          default_repl_obj_to_string/3,
          default_repl_writer/4,
          show_kb_preds/2,show_kb_preds/3,success/2,
         init_location_grid/1,
         % test_te/0,
         grid_dist/3,
         to_3d/2,
         is_3d/1,
         in_grid/2,
         in_grid_rnd/2,
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
         isaOrSame/2,
         current_agent_or_var/1

 ]).

:-discontiguous create_instance_0/3.

:-export((
          create_instance/2,create_instance/3,
          create_instance_0/3,
          create_agent/1,
          create_agent/2)).

:- dynamic  agent_list/1.

:- include(logicmoo(vworld/moo_header)).
:- moodb:register_module_type(utility).

:- include(logicmoo('vworld/world_2d')).
:- include(logicmoo('vworld/world_agent')).
:- include(logicmoo('vworld/world_text')).
:- include(logicmoo('vworld/world_effects')).
:- include(logicmoo('vworld/world_events')).
:- include(logicmoo('vworld/world_spawning')).

:- meta_predicate intersect_pred(+,+,+,+,?,-).
:- meta_predicate cached(0).

is_property(P,A):- moo:db_prop(_,C),functor(C,P,A2),A is A2-1.

is_type(O):-is_type0(O).
is_type0(T):-holds_t(label_type_props,_,T,_).
is_type0(T):- holds_t(type_default_props,_,T,_).
is_type0(OT):- holds_t(subclass,OT,_); holds_t(subclass,_,OT).
is_type0(food).
is_type0(explorer).
is_type0(predator).
is_type0(prey).
is_type0(monster).
is_type0(dir).
is_type0(agent).

isaOrSame(A,B):-A==B,!.
isaOrSame(A,B):-world_mud_isa(A,B).

intersect_pred(A,EF,B,LF,Tests,Results):-findall( A-B, ((member(A,EF),member(B,LF),once(Tests))), Results),[A-B|_]=Results.
% is_property(P,_A),PROP=..[P|ARGS],CALL=..[P,Obj|ARGS],req(CALL).
obj_memb(E,L):-member(E,L).
isa_any(E,L):-flatten([E],EE),flatten([L],LL),!,intersect_pred(A,EE,B,LL,isaOrSame(A,B),_Results).
prop_memb(E,L):-flatten([E],EE),flatten([L],LL),!,intersect_pred(A,EE,B,LL,isaOrSame(A,B),_Results).


exists(O):-  dbase:holds_t(item,O).
exists(O):-  dbase:holds_t(agent,O).
exists(O):-  dbase:holds_t(region,O).
anyInst(O):-  dbase:holds_t(type,O).
anyInst(O):-exists(O).


metaclass(regiontype).
metaclass(agenttype).
metaclass(itemtype).
metaclass(formattype).


isa_mc(region,regiontype).
isa_mc(agent,agenttype).
isa_mc(item,itemtype).

isa_mc(FT,formattype):-moo:ft_info(FT,_).

moo:subclass(SubType,formattype):-isa_mc(SubType,formattype).

not_mud_isa(agent,formattype).
cached(G):-catch(G,_,fail).

world_mud_isa(O,T):- req(mud_isa(O,T)).
world_mud_isa(O,T):- O==T,!.
world_mud_isa(O,T):- var(O),var(T),!,isa_mc(T,_),anyInst(O),req(mud_isa(O,T)).
world_mud_isa(O,T):- cached(not_mud_isa(O,T)),!,fail.
world_mud_isa(O,T):- props(O,ofclass(T)).
world_mud_isa(O,T):- props(O,mud_isa(T)).
world_mud_isa(_,T):- (atom(T);var(T)),!,fail.
world_mud_isa(O,T):- compound(O),!,functor(O,T,_).
world_mud_isa(O,T):- atom(O),!,mud_isa_atom(O,T).

mud_isa_atom(O,T):- atomic_list_concat([T,_|_],'-',O),!.
mud_isa_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.

define_subtype(O,T):- add(moo:subclass(O,T)).

create_meta(T,P,C,MT):-
   must(split_name_type(T,P,C)),
   define_subtype(C,MT),
   OP =.. [MT,P],
   dbase_mod(M),
   assert_if_new(M:OP),
   must(forall_member(E,[OP,ofclass(P,MT),ofclass(P,C)],must(add(E)))),
   must(findall_type_default_props(P,C,Props)),!,
   must(padd(P,Props)),!.

rez_to_inventory(Whom,T,P):-
   create_meta(T,P,_,item),
   padd(Whom,possess(P)).


create_instance(P,What):-create_instance(P,What,[]).

moo:subclass(wearable,item).
moo:subclass(knife,item).
moo:subclass(food,item).

moo:createableType(FT):- formattype(FT),!,fail.
moo:createableType(item).
moo:createableSubclassType(S,T):- moo:createableType(T),moo:subclass(S,T).
moo:createableSubclassType(T,T):-nonvar(T),moo:createableType(T).

moo:subclass(int,formattype).
moo:subclass(dir,formattype).
moo:subclass(number,formattype).
moo:subclass(string,formattype).


create_agent(P):-create_agent(P,[]).
create_agent(P,List):-must(create_instance(P,agent,List)).

formattype(FormatType):-moo:subclass(FormatType,formattype).
formattype(FormatType):-dbase:holds_t(isa, FormatType, formattype).

define_type(Spec):-create_instance(Spec,type,[]).

create_instance(What,Type,Props):- create_instance_0(What,Type,Props),!.

:-discontiguous create_instance_0/3.

create_instance_0(What,FormatType,List):- FormatType\==type,
   formattype(FormatType),!,
   throw(formattype(FormatType,create_instance(What,FormatType,List))).

create_instance_0(SubType,type,List):-!,
   add(mud_isa(SubType,type)),
      dbase_mod(M),
      A = M:type(SubType),
   assert_if_new(A),
   padd(SubType,List).

moo:createableType(agent).
moo:subclass(actor,agent).

create_instance_0(T,agent,List):-!,
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

moo:createableType(region).
create_instance_0(T,Type,List):- moo:createableType(Type),
   create_meta(T,P,_,Type),!,
   padd(P,List).

create_instance_0(T,Type,List):-moo:subclass(Type,MetaType),moo:createableType(MetaType),
   create_meta(T,P,_,MetaType),
   padd(P,mud_isa(Type)),
   padd(P,List),
   clr(atloc(P,_)),
   put_in_world(P).

moo:createableType(type).
f(X,Y):- hotrace(((functor_safe(X,XF,_),functor_safe(Y,YF,_),string_equal_ci(XF,YF)))).

moo:type_default_props(_,agent,last_command(stand)).


:- include(logicmoo(vworld/moo_footer)).
