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
:- module(teleport, []).

:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(command).

% teleport
moodb:agent_text_command(Agent,[tp],Agent,teleport).
moodb:agent_text_command(Agent,[tp,Other,Where],Agent,teleport_to(Other,Where)).
moodb:agent_text_command(Agent,[tp,Where],Agent,teleport_to(self,Where)).

moo:action_help(teleport,"randomly teleport somewhere").
moo:action_info(teleport(item,region)).

%random
moodb:agent_call_command(Agent,teleport):-
   props(Agent,charge>10),
   clr(atloc(Agent,_)),
   clr(inRegion(Agent,_)),
   put_in_world(Agent).

%targeted
moodb:agent_call_command(_Agent,teleport_to(Other,Where)):-   
   moodb:coerce(Other,agent,Target),
   moodb:coerce(Where,region,Location),
   clr(inRegion(Target,_)),
   clr(atloc(Target,_)),
   to_3d(Location,Where3D)
   add(atloc(Target,Where3D)).


%targeted
moodb:agent_call_command(Agent,tp(Where)):-
   moodb:coerce(Agent,agent,Target),
   moodb:coerce(Where,region,Location),
   clr(inRegion(Target,_)),
   clr(atloc(Target,_)),
   to_3d(Location,Where3D)
   add(atloc(Target,Where3D)).

:- include(logicmoo(vworld/moo_footer)).


