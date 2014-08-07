% :- module(user). 
:- module(create, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% damage(Agent,Dam) = damage
% success(Agent,Suc) = checks success of last action (actually checks the failure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

% ====================================================
% show the stats system
% ====================================================
moo:action_info(create(list(term)), "Creates a new object of Type1 + props").

moo:agent_call_command(Agent,create(SWhat)):-  create_new_object(Agent,SWhat,1).

create_new_object(Agent,[S|What]):-create_new_object(Agent,[S|What],1).

:-decl_mpred_prolog(authorWas(term,term)).
:-decl_mpred_prolog(current_pronoun(agent,string,term)).

moo:agent_call_command(Agent,rez(NewType)):- nonvar(NewType), atloc(Agent,LOC), create_instance(NewType,item,[atloc(LOC)]).

:-export(create_new_object/3).
create_new_object(Agent,[S|What],ArgAt):-
   split_name_type(S,I,C),
   show_call(add(isa(I,C))),
   padd(I,authorWas(Agent)),
   padd(Agent,stowed,I),
   padd(Agent,current_pronoun("it",I)),
   get_inst_default_props(I,_All,Need),
   show_call(padd(I,Need)),
   ArgAt2 is ArgAt+1,
   addPropInfo(Agent,I,What,ArgAt2),!.
   

addPropInfo(Agent,I,What,ArgAt2):-dmsg(todo(addPropInfo(Agent,I,What,ArgAt2))).


:- include(logicmoo(vworld/moo_footer)).

