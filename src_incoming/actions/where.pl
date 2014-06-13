
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

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

moo:action_help(where(object),"Tells where something is").

dyn:subclass(agent,object).
dyn:subclass(item,object).

moo:agent_text_command(Agent,[where,X],Agent,where(X)).
moo:agent_text_command(Agent,[where,BE,X],Agent,where(X)):-memberchk(BE,[is,are,be,were]).


% where 
moo:agent_call_command(_Agent,where(SObj)) :-
    forall(
     (atloc(Obj,LOC), object_match(SObj,Obj)),
        fmt(cmdresult(where,atloc(Obj,LOC)))).


moo:agent_text_command(Agent,[who],Agent,who(_)).

moo:action_help(who(optional(agent,_)),"Lists who is online (where they are at least)").

get_inRegion(Agnt,Where):-inRegion(Agnt,Where).
get_inRegion(Agnt,Where):-atloc(Agnt,Where).

moo:agent_call_command(_Gent,who(Agent)) :- 
     forall(agent(Agent),
      once((get_inRegion(Agent,Where),
            fmt(cmdresult(who(Agent),inRegion(Agent,Where)))))).

:- include(logicmoo(vworld/moo_footer)).


