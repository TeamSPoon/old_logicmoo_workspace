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
            %mud_isa/2,
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
            decl_type/1,
            
                       
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


isaOrSame(A,B):-A==B,!.
isaOrSame(A,B):-isa(A,B).

intersect(A,EF,B,LF,Tests,Results):-findall( A-B, ((member(A,EF),member(B,LF),once(Tests))), Results),[A-B|_]=Results.
% is_property(P,_A),PROP=..[P|ARGS],CALL=..[P,Obj|ARGS],req(CALL).
obj_memb(E,L):-member(E,L).
isa_any(E,L):-flatten([E],EE),flatten([L],LL),!,intersect(A,EE,B,LL,isaOrSame(A,B),_Results).
prop_memb(E,L):-flatten([E],EE),flatten([L],LL),!,intersect(A,EE,B,LL,isaOrSame(A,B),_Results).

exisitingThing(O):-item(O).
exisitingThing(O):-agent(O).
exisitingThing(O):-region(O).
anyInst(O):-type(O).
anyInst(O):-exisitingThing(O).

:-decl_type(metaclass).

argsIsa(typeGenls(type,metaclass)).

hook:decl_database_hook(assert(_),typeGenls(_,MC)):-assert_isa(MC,metaclass).

hook:deduce_facts(typeGenls(T,MC),hook:deduce_facts(subclass(S,T),isa(S,MC))).

typeGenls(region,regiontype).
typeGenls(agent,agenttype).
typeGenls(item,itemtype).

subclass(sillyitem,item).

metaclass(formattype).


/*
moo:isa(region,regiontype).
moo:isa(agent,agenttype).
moo:isa(item,itemtype).
*/

%moo:subclass(SubType,formattype):-isa(SubType,formattype).

cached(G):-catch(G,_,fail).


:-export(create_meta/4).
% if SuggestedName was 'food666' it'd like the SuggestedClass to be 'food' and the stystem name will remain 'food666'
% if SuggestedName was 'food' it'd like the SuggestedClass to be 'food' and the stystem name will become a gensym like 'food1'
create_meta(SuggestedName,SuggestedClass,BaseClass,SystemName):-
   must_det(split_name_type(SuggestedName,SystemName,NewSuggestedClass)),
   ignore(SuggestedClass=NewSuggestedClass),   
   assert_subclass_safe(SuggestedClass,BaseClass),
   assert_subclass_safe(NewSuggestedClass,BaseClass),
   assert_isa_safe(SystemName,BaseClass),
   assert_isa_safe(SystemName,NewSuggestedClass),
   assert_isa_safe(SystemName,SuggestedClass).


moo:subclass('Area',region).
moo:nonCreatableType(int).
moo:nonCreatableType(term).
moo:nonCreatableType(type).

create_instance(P,What):-create_instance(P,What,[]).

moo:subclass(wearable,item).
moo:subclass(knife,item).
moo:subclass(food,item).

moo:creatableType(FT):- nonvar(FT),formattype(FT),!,fail.
moo:creatableType(FT):- nonvar(FT),nonCreatableType(FT),!,fail.
moo:creatableType(item). %  type, formattype, 
moo:creatableType(SubType):-member(SubType,[agent,item,region]).
moo:creatableType(S):- is_asserted(creatableType(T)), transitive_subclass(S,T).

moo:createableSubclassType(S,T):- moo:creatableType(T),is_asserted(subclass(S,T)).
moo:createableSubclassType(T,'TemporallyExistingThing'):- moo:creatableType(T).


moo:isa(int,formattype).
moo:isa(dir,type).
% moo:isa(dir,valuetype).
moo:isa(number,formattype).
moo:isa(string,formattype).


create_agent(P):-create_agent(P,[]).
create_agent(P,List):-must_det(create_instance(P,agent,List)).

% decl_type(Spec):-create_instance(Spec,type,[]).

create_instance(What,Type,Props):- loop_check(create_instance_0(What,Type,Props),dmsg(already_create_instance(What,Type,Props))).

:-discontiguous create_instance_0/3.

:- export(is_creating_now/1).
:- dynamic(is_creating_now/1).

create_instance_0(What,Type,List):- (var(What);var(Type);var(List)),trace_or_throw((var_create_instance_0(What,Type,List))).
create_instance_0(I,_,_):-is_creating_now(I),!.
create_instance_0(I,_,_):-asserta_if_new(is_creating_now(I)),fail.
create_instance_0(What,FormatType,List):- FormatType\==type, formattype(FormatType),!,trace_or_throw(formattype(FormatType,create_instance(What,FormatType,List))).
create_instance_0(SubType,type,List):-decl_type(SubType),padd(SubType,List).

moo:creatableType(agent).
moo:subclass(actor,agent).
moo:subclass(explorer,agent).

:-dynamic_multifile_exported(moo:max_damage/2).
:-dynamic_multifile_exported(moo:max_charge/2).
:-dynamic_multifile_exported(moo:type_max_charge/2).
:-dynamic_multifile_exported(moo:type_max_charge/2).

moo:max_charge(T,NRG):- moo:type_max_charge(AgentType,NRG),isa(T,AgentType).
moo:max_damage(T,Dam):- moo:type_max_damage(AgentType,Dam),isa(T,AgentType).

punless(Cond,Action):- once((call(Cond);call(Action))).

create_instance_0(T,agent,List):-
  must_det_l([
   retractall(agent_list(_)),
   create_meta(T,_,agent,P),
   must_det(isa(P,agent)),
   padd(P,List),   
   punless(possess(P,_),rez_to_inventory(P,food,_Food)),
   %reset_values(P),
   padd(P, [ max_damage(500),
                       max_charge(200),
                       damage(500),
                       charge(200),
                       facing("n"),
                       agent_turnnum(0),
                       score(1)]),   
   set_stats(P,[]),
   put_in_world(P),
   add_missing_instance_defaults(P)]).
   
/*
reset_values(I):- forall(moo:valueReset(To,From),reset_value(I,To,From)).

reset_value(I,To,From):- prop(I,From,FromV), padd(I,To,FromV),!.
reset_value(I,To,From):- prop(I,From,FromV), padd(I,To,FromV),!.
   
   (contains_term(V,value),get_value(P,V,Result)) -> subst(V,P,self)
   argIsa(P,SVArgNum,Type),
   is_term_ft(V,Type),

moo:valueReset(score,0).
moo:valueReset(damage,max_damage).
moo:valueReset(charge,max_charge).

*/

moo:creatableType(region).

create_instance_0(T, item, List):-
   isa(T,What),What\=item, moo:creatableType(What),!,create_instance_0(T, What, List).

create_instance_0(T,Type,List):-
  moo:createableSubclassType(Type,MetaType),
  must_det_l([
   create_meta(T,Type,MetaType,P),
   padd(P,List),
   add_missing_instance_defaults(P)]). 

create_instance_0(T,MetaType,List):-  
  must_det_l([
   create_meta(T,Type,MetaType,P),
   padd(P,List),
   add_missing_instance_defaults(P)]). 


create_instance_0(T,MetaType,List):-
 dmsg(create_instance_0(T,MetaType,List)),
leash(+call),trace,
  must_det_l([
   create_meta(T,Type,MetaType,P),
   padd(P,List),
   put_in_world(P),   
   add_missing_instance_defaults(P)]). 

create_instance_0(What,Type,Props):- leash(+call),trace,dtrace,trace_or_throw(dmsg(assumed_To_HAVE_creted_isnance(What,Type,Props))),!.

%moo:creatableType(type).



same(X,Y):- X=Y,!.
same(X,Y):- compound(X),arg(1,X,Y),!.
same(X,Y):- compound(Y),arg(1,Y,X),!.
same(X,Y):- samef(X,Y).


samef(X,Y):- X=Y,!.
samef(X,Y):- notrace(((functor_safe(X,XF,_),functor_safe(Y,YF,_),string_equal_ci(XF,YF)))).

moo:default_type_props(region,opaqueness(1)).
moo:default_type_props(obj,opaqueness(100)).
moo:default_type_props(item,listPrice(0)).
moo:default_type_props(agent,last_command(stand)).
moo:default_type_props(agent,[
                       max_damage(500),
                       max_charge(200),
                       damage(500),
                       charge(200),
                       facing("n"),
                       agent_turnnum(0),
                       score(1),
                       memory(P,directions([n,s,e,w,ne,nw,se,sw,u,d]))]).












:- include(logicmoo(vworld/moo_footer)).
