/** <module>
% teleport.pl
%
% This file defines how an agent teleports 
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
:-swi_module(teleport, []).

:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

% teleport
action_info(teleport(optional(and([obj,not(region)]),self),optionalStr("to"),optional(region,random(region))),"teleport [obj] [to] [somewhere]").

verb_alias(tp,teleport).

%targeted
agent_call_command(_Agent,teleport(Other,_TO,Where)):-
   coerce(Other,obj,Target),
   coerce(Where,region,Location),
   clr(localityOfObject(Target,_)),
   clr(atloc(Target,_)),
   to_3d(Location,Where3D),
   add(atloc(Target,Where3D)).


:- include(logicmoo(vworld/moo_footer)).


