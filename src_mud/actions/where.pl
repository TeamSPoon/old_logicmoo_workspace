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

% :-swi_module(user). 
:-swi_module(where_cmd, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).


mudSubclass(tAgentGeneric,tObj).
mudSubclass(tItem,tObj).


% where 
agent_text_command(Agent,[actWhere,BE,X],Agent,actWhere(X)):-memberchk(BE,[is,are,be,were]).
action_info(actWhere(tObj),"Tells where something is").
agent_call_command(_Agent,actWhere(SObj)) :-
    forall(
     (mudAtLoc(Obj,LOC), match_object(SObj,Obj)),
        fmt(cmdresult(actWhere,mudAtLoc(Obj,LOC)))).


action_info(actWho(optional(tAgentGeneric,world)),"Lists who is online (where they are at least)").

agent_call_command(_Gent,actWho(W)) :- mud_cmd_who(W).

mud_cmd_who(world):-!,mud_cmd_who_1(_).
mud_cmd_who(Who):- mud_cmd_who_1(Who).

get_inRegion(Agnt,Where):- inRegion(Agnt,Where),!.
get_inRegion(Agnt,Where):- mudAtLoc(Agnt,Where),!.
get_inRegion(Agnt,Where):- localityOfObject(Agnt,Where),!.

mud_cmd_who_1(Who):-
     forall(tAgentGeneric(Who),
      once((get_inRegion(Who,Where),
            fmt(cmdresult(actWho(Who),localityOfObject(Who,Where)))))).

:- include(logicmoo(vworld/moo_footer)).
