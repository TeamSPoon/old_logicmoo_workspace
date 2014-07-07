/** <module> 
% Common place to reduce redundancy World utility prediates
%
% Project Logicmoo: A MUD server written in Prolog
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
       % call_agent_action/2,
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
            list_agents/1,
            agent_list/1,
            check_for_fall/3,
            list_object_dir_sensed/4,
            list_object_dir_near/3,
            num_near/3,
            asInvoked/2,
            define_type/1,
            findall_type_default_props/3,
                       
          show_kb_via_pred/3,
          default_repl_obj_to_string/3,
          default_repl_writer/4,
          show_kb_preds/2,show_kb_preds/3,success/2,          
         init_location_grid/1,
         samef/2,
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
         isaOrSame/2,
         current_agent_or_var/1

 ]).


:-discontiguous create_instance_0/3.

:-export((
          create_instance/2,
          create_instance/3,
          create_instance_0/3,
          create_agent/1,
          create_agent/2)).

:- dynamic  agent_list/1.


:- include(logicmoo('vworld/moo_header.pl')).
:- register_module_type(utility).


:- include(logicmoo('vworld/world_2d.pl')).
:- include(logicmoo('vworld/world_agent.pl')).
:- include(logicmoo('vworld/world_text.pl')).
:- include(logicmoo('vworld/world_effects.pl')).
:- include(logicmoo('vworld/world_events.pl')).
:- include(logicmoo('vworld/world_spawning.pl')).




/*
is_property(P,A):- mpred_prop(_,C),functor(C,P,A2),A is A2-1.
is_type(O):-is_type0(O).
is_type0(T):-moo:label_type_props(_,T,_).
is_type0(T):-moo:default_type_props(_,T,_).
is_type0(OT):- moo:subclass(OT,_); moo:subclass(_,OT).
is_type0(food).
is_type0(explorer).
is_type0(predator).
is_type0(prey).
is_type0(monster).
is_type0(dir).
is_type0(agent).
*/

isaOrSame(A,B):-A==B,!.
isaOrSame(A,B):-isa(A,B).

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


moo:isa(X,Y):-loop_check(mud_isa(X,Y),moo:is_asserted(isa(X,Y))).
moo:isa(region,regiontype).
moo:isa(agent,agenttype).
moo:isa(item,itemtype).

%moo:subclass(SubType,formattype):-isa(SubType,formattype).

not_mud_isa(agent,formattype).
cached(G):-catch(G,_,fail).

mud_isa(O,T):- loop_check(mud_isa_lc(O,T),fail).

mud_isa_lc(O,T):- stack_check(300),O==T,!.
mud_isa_lc(O,T):- var(O),var(T),!,isa(T,type),anyInst(O),isa(O,T).
mud_isa_lc(O,T):- cached(not_mud_isa(O,T)),!,fail.
mud_isa_lc(O,T):- atom(T),Call =.. [T,O],predicate_property(Call,_),!,Call.
mud_isa_lc(O,T):- req(isa(O,T)).
mud_isa_lc(_,T):- (atom(T);var(T)),!,fail.
mud_isa_lc(O,T):- compound(O),!,functor(O,T,_).
mud_isa_lc(O,T):- atom(O),atom(T),!,mud_isa_atom(O,T).

mud_isa_atom(O,T):- atomic_list_concat_catch([T,_|_],'-',O),!.
mud_isa_atom(O,T):- atom_concat(T,Int,O),catch(atom_number(Int,_),_,fail),!.

define_subtype(O,T):-assert_if_new(moo:subclass(O,T)).

findall_type_default_props(Inst,Type,TraitsO):-
   findall(Props,each_default_type_props(Inst,Type,Props),Traits),flatten_set(Traits,TraitsO),!.

each_default_type_props(Inst,Type,Props):-call_no_cuts(moo:default_type_props(Inst,Type,Props)).

create_meta(T,P,C,MT):-
   must(split_name_type(T,P,C)),
   define_subtype(C,MT),
   OP =.. [MT,P],
   dbase_mod(M),
   assert_if_new(M:OP),
   % must(forall_member(E,[OP,isa(P,MT),isa(P,C)],must(add(E)))),
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

moo:createableType(FT):- nonvar(FT),formattype(FT),!,fail.
moo:createableType(item).
moo:createableType(SubType):-member(SubType,[agent,item, type, formattype, region]).

moo:createableSubclassType(S,T):- moo:createableType(T),req(subclass(S,T)).
%moo:createableType(T,T):-nonvar(T),moo:createableType(T).

moo:subclass(int,formattype).
moo:subclass(dir,formattype).
moo:subclass(number,formattype).
moo:subclass(string,formattype).


create_agent(P):-create_agent(P,[]).
create_agent(P,List):-must(create_instance(P,agent,List)).

% define_type(Spec):-create_instance(Spec,type,[]).

create_instance(What,Type,Props):- create_instance_0(What,Type,Props),!.

:-discontiguous create_instance_0/3.

:- export(is_created/1).
:- dynamic(is_created/1).

create_instance_0(I,_,_):-is_created(I),!.
create_instance_0(I,_,_):-asserta(is_created(I)),fail.

create_instance_0(What,FormatType,List):- FormatType\==type,
   formattype(FormatType),!,
   throw(formattype(FormatType,create_instance(What,FormatType,List))).

create_instance_0(SubType,type,List):-!,
   add(isa(SubType,type)),
      dbase_mod(M),
      A = M:type(SubType),
   assert_if_new(A),
   padd(SubType,List).

moo:createableType(agent).
moo:subclass(actor,agent).
moo:subclass(explorer,agent).


moo:max_charge(T,NRG):- moo:type_max_charge(AgentType,NRG),isa(T,AgentType).
moo:max_damage(T,Dam):- moo:type_max_damage(AgentType,Dam),isa(T,AgentType).

create_instance_0(T,agent,List):-!,
   retractall(agent_list(_)),
   must(create_meta(T,P,_,agent)),
   must(padd(P,List)),   
   must(put_in_world(P)),
   rez_to_inventory(P,food,_Food),
   req(max_charge(T,NRG)),
   moo:max_damage(T,Dam),
   add(charge(P,NRG)),
   add(damage(P,Dam)),
   add(agent_turnnum(P,0)),
   add(score(P,0)),
   set_stats(P,List),
   add(memory(P,directions([n,s,e,w,ne,nw,se,sw,u,d]))),!.

moo:createableType(region).
create_instance_0(T,Type,List):- moo:createableType(Type),
   create_meta(T,P,_,Type),!,
   padd(P,List).

create_instance_0(T,Type,List):-
   moo:subclass(Type,MetaType),
   moo:createableType(MetaType),
   create_meta(T,P,_,MetaType),
   padd(P,isa(Type)),
   padd(P,List),
   clr(atloc(P,_)),
   put_in_world(P).

moo:createableType(type).


same(X,Y):- X=Y,!.
same(X,Y):- compound(X),arg(1,X,Y),!.
same(X,Y):- compound(Y),arg(1,Y,X),!.
same(X,Y):- samef(X,Y).


samef(X,Y):- X=Y,!.
samef(X,Y):- notrace(((functor_safe(X,XF,_),functor_safe(Y,YF,_),string_equal_ci(XF,YF)))).

moo:default_type_props(self,agent,last_command(stand)).


:- include(logicmoo(vworld/moo_footer)).
