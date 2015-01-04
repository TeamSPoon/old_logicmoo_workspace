/** <module>
% actPut.pl
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
:-swi_module(actPut, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(mtCommand).

% actPut
action_info(actPut(tThrowable,prepstr_spatial,tHasobjs),"actPut [obj] [onto|inside] [somewhere]").

verb_alias(set,actPut).
verb_alias(place,actPut).
verb_alias(actHide,actPut).
verb_alias(display,actPut).
verb_alias(actStow,actPut).

%targeted
agent_call_command(_Agent,actPut(Other,_Prep,Where)):-
   coerce(Other,tObj,Target),
   coerce(Where,tHasobjs,Location),
   clr(localityOfObject(Target,_)),
   clr(mudAtLoc(Target,_)),
   to_3d(Location,Where3D),
   add(mudAtLoc(Where3D,Location)).


:- include(logicmoo(vworld/moo_footer)).
