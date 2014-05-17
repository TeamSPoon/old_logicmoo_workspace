
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the agents action of eating. 
% Very simple... but kept separate to maintain modularity
%
% This uses the worth/2 predicate from take.pl
% Will (theoretically) only be used in conjuction with take action
%
% It will destroy something, even if it is not food... talk about a garbage disposal. 
*/

:- module(where, []).

:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(command).

moo:decl_action(_,where(object),"Tells where something is").
moo:subclass(agent,object).
moo:subclass(item,object).

moo:agent_text_command(Agent,[where,X],Agent,where(X)).
moo:agent_text_command(Agent,[where,BE,X],Agent,where(X)):-member(BE,[is,are,be,were]).


% where 
moo:agent_call_command(_Agent,where(SObj)) :-
	atloc(Obj,LOC),
        object_match(SObj,Obj),
        fmt(cmdresult(where,atloc(Obj,LOC))).



moo:decl_action(agent,who(optional(agent,_)),"Lists who is online (where they are at least)").

moo:agent_call_command(_Gent,who(Agnt2)) :- C = (agent(Agnt2),dbase_true(inRegion,Agnt2,Where)), forall(db_forall_query(C),fmt(cmdresult(who(Agnt2),inRegion(Agnt2,Where)))).


:- include(logicmoo('vworld/moo_footer.pl')).


