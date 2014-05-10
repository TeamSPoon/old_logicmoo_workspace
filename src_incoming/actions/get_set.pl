:- module(get_set, []).
/** <module> Agent changes one of their own variables
% Douglas Miles 2014

*/
:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(command).

moo:decl_action('@get'(optional(term,self),term),text("@get term to a property")).
moo:decl_action('@set'(optional(term,self),atom,term),text("@sets term to a property")).

moo:agent_call_command(_Agent,'@set'(Obj,Prop,Value)) :- game_assert(k(Prop,Obj,Value)).
moo:agent_call_command(_Agent,'@get'(Obj,Prop)) :- catch((findall(Value,(req(k(Prop,Obj,Value)),fmt(k(Prop,Obj,Value))),L),(L==[_|_]->true;fmt(k(Prop,Obj,missing)))),E,fmt('@get Error ~q',[E])).

:- include(logicmoo('vworld/moo_footer.pl')).
