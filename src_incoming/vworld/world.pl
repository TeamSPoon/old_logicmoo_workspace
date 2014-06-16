
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
        call_agent_command_maybe_fail/3,

            isa_any/2,
          create_meta/4,
            put_in_world/1,
            get_session_id/1,
            pathBetween_call/3,
            obj_memb/2,
            prop_memb/2,

            
            
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

         foc_current_player/1,
         locationToRegion/2,
         init_location_grid/2,
         set_stats/2,
         worth/3,
         spread/0,
         growth/0,
         isaOrSame/2,
         mud_isa/2,
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

:- include(logicmoo(vworld/moo_header)).
:- moo:register_module_type(utility).

:- meta_predicate intersect_pred(+,+,+,+,?,-).
% :- meta_predicate cached(0).
:- meta_predicate show_kb_via_pred_3(*,*,*,*,^,?).


isaOrSame(A,B):-A==B,!.
isaOrSame(A,B):-mud_isa(A,B).

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

dyn:subclass(SubType,formattype):-isa_mc(SubType,formattype).

mud_isa(O,T):-req(isa(O,T)).

create_meta(T,P,C,MT):-
   must(split_name_type(T,P,C)),
   ignore((ifThen(ground(C:MT),add(subclass(C,MT))))),
   must(findall_type_default_props(P,C,Props)),!, 
   dmsg(done(create_meta(T,P,C,MT,Props))),!,
   must(padd(P,[isa(MT),isa(C)|Props])),!.

rez_to_inventory(Whom,T,P):-
   create_meta(T,P,_,item),
   padd(Whom,possess(P)).


create_instance(P,What):-create_instance(P,What,[]).

dyn:subclass(wearable,item).
dyn:subclass(knife,item).
dyn:subclass(food,item).

dyn:createableType(FT):- formattype(FT),!,fail.
dyn:createableType(item).
dyn:createableSubclassType(S,T):- dyn:createableType(T),dyn:subclass(S,T).
dyn:createableSubclassType(T,T):-nonvar(T),dyn:createableType(T).

dyn:subclass(int,formattype).
dyn:subclass(dir,formattype).
dyn:subclass(number,formattype).
dyn:subclass(string,formattype).


create_agent(P):-create_agent(P,[]).
create_agent(P,List):-must(create_instance_0(P,agent,List)).

formattype(FormatType):-dyn:subclass(FormatType,formattype).

create_instance(What,Type,Props):-number(What),trace_or_throw(create_instance(What,Type,Props)).
create_instance(What,Type,Props):-loop_check(create_instance_0(What,Type,Props),true),!.
create_instance(What,Type,Props):-dmsg(todo(create_instance(What,Type,Props))),!.

:-discontiguous create_instance_0/3.

create_instance_0(What,FormatType,List):- FormatType\==type,
   formattype(FormatType),!,
   throw(formattype(FormatType,create_instance(What,FormatType,List))).


create_instance_0(SubType,type,List):-!,grtrace,
   define_type(SubType),
   add(props(SubType,[isa(type)|List])).
  


dyn:createableType(agent).
dyn:subclass(actor,agent).
dyn:subclass(explorer,agent).

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

dyn:createableType(region).
create_instance_0(T,Type,List):- dyn:createableType(Type),!,
   create_meta(T,P,_,Type),!,
   padd(P,List).

create_instance_0(T,Type,List):-
   dyn:subclass(Type,MetaType),
   dyn:createableType(MetaType),
   create_meta(T,P,_,MetaType),
   padd(P,isa(Type)),
   padd(P,List),
   clr(atloc(P,_)),
   put_in_world(P).

%dyn:createableType(type).
%f(X,Y):- hotrace(((functor_safe(X,XF,_),functor_safe(Y,YF,_),string_equal_ci(XF,YF)))).

moo:type_default_props(self,agent,last_command(stand)).

:- include(logicmoo('vworld/world_2d')).
:- include(logicmoo('vworld/world_agent')).
:- include(logicmoo('vworld/world_text')).
:- include(logicmoo('vworld/world_effects')).
:- include(logicmoo('vworld/world_events')).
:- include(logicmoo('vworld/world_spawning')).

:- include(logicmoo(vworld/moo_footer)).
