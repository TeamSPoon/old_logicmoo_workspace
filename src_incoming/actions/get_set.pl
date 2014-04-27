:- module(get_set, []).
/** <module> Agent changes one of their own variables
% Douglas Miles 2014

*/
:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

moo:decl_action('@get'(optional(term,self),atom,term),text("@get term to a property")).
moo:decl_action('@set'(optional(term,self),atom,term),text("@sets term to a property")).

moo:agent_call_command(_Agent,'@set'(Obj,Prop,Value)) :- add(k(Prop,Obj,Value)).
moo:agent_call_command(_Agent,'@get'(Obj,Prop,Value)) :- req(k(Prop,Obj,Value)).

:- include(logicmoo('vworld/vworld_footer.pl')).
