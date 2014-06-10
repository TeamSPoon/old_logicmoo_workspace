:- module(get_set, []).
/** <module> Agent changes one of their own variables
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(command).

moo:action_help('@get'(optional(term,self),term),text("@get term to a property")).
moo:action_help('@set'(optional(term,self),atom,term),text("@sets term to a property")).

moodb:agent_call_command(Agent,'@set'(Obj0,Prop0,Value0)) :- subst(game_assert(k(Prop0,Obj0,Value0)),self,Agent,K),dmsg(K),debugOnError(K).

moodb:agent_call_command(Agent,'@get'(Obj0,Prop0)) :- subst(k(Prop0,Obj0,Value),self,Agent,K), 
                                                        catch((findall(Value,(req(K),fmt(K)),L),
                                                          (L==[_|_]->true;fmt(missing(K)))),E,fmt('@get Error ~q',[E:K])).

:- include(logicmoo(vworld/moo_footer)).
