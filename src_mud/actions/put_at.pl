/** <module>
% put.pl
%
% This file defines how an agent puts 
%
%  Test of the new concise syntax:
% 
%   props(Agent,charge>10),
%
% Comments below document the basic idea.
%
% Dec 13, 2035
% Douglas Miles
*/
% :-swi_module(user). 
:-swi_module(put, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).

% put
action_info(put(tThrowable,prepstr_spatial,tHasobjs),"put [obj] [onto|inside] [somewhere]").

verb_alias(set,put).
verb_alias(place,put).
verb_alias(actHide,put).
verb_alias(display,put).
verb_alias(actStow,put).

%targeted
agent_call_command(_Agent,put(Other,_Prep,Where)):-
   coerce(Other,tObj,Target),
   coerce(Where,tHasobjs,Location),
   clr(localityOfObject(Target,_)),
   clr(mudAtLoc(Target,_)),
   to_3d(Location,Where3D),
   add(mudAtLoc(Where3D,Location)).


:- include(logicmoo(vworld/moo_footer)).
