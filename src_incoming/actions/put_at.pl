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

:- register_module_type(command).

% put
action_info(put(throwable,prepstr_spatial,hasobjs),"put [obj] [onto|inside] [somewhere]").

verb_alias(set,put).
verb_alias(place,put).
verb_alias(hide,put).
verb_alias(display,put).
verb_alias(stow,put).

%targeted
agent_call_command(_Agent,put(Other,_Prep,Where)):-
   coerce(Other,obj,Target),
   coerce(Where,hasobjs,Location),
   clr(localityOfObject(Target,_)),
   clr(atloc(Target,_)),
   to_3d(Location,Where3D),
   add(atloc(Where3D,Location)).


:- include(logicmoo(vworld/moo_footer)).


