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

%   moo:decl_action(_,tick(agent),"Makes some agent do something brilliant")  
moo:decl_action(_,where(actor),"Tells where actor is").
moo:decl_action(_,where(item),"Tells where item is").

/*
moo:decl_subclass(actor,object).
moo:decl_subclass(item,object).
moo:decl_action(_,where(object),"Tells where item is").
*/

% where 
moo:agent_call_command(Agent,where(SObj)) :-
	atloc(SObj,LOC),
        dmsg(command(where(atloc(SObj,LOC)))).

:- include(logicmoo('vworld/moo_footer.pl')).

