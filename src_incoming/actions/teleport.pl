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
% :- module(user). 
:- module(teleport, []).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

% teleport
moo:action_info(teleport(optional(and([obj,not(region)]),self),optionalStr("to"),optional(region,random(region))),"teleport [obj] [to] [somewhere]").

moo:verb_alias(tp,teleport).

%targeted
moo:agent_call_command(_Agent,teleport(Other,_TO,Where)):-
   moo:coerce(Other,obj,Target),
   moo:coerce(Where,region,Location),
   clr(localityOfObject(Target,_)),
   clr(atloc(Target,_)),
   to_3d(Location,Where3D),
   add(atloc(Target,Where3D)).


:- include(logicmoo(vworld/moo_footer)).


