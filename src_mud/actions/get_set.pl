% :-swi_module(user). 
:-swi_module(get_set, []).
/** <module> Agent changes one of their own variables
% Douglas Miles 2014

*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(tCommand).

action_info(actGet(optional(ftTerm,self),ftTerm),ftText("@get term to a property")).
action_info(actSet(optional(ftTerm,self),atom,ftTerm),ftText("@sets term to a property")).

agent_text_command(Agent,[actSet,Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
agent_text_command(Agent,[actGet,Prop0],Agent,actGet(Agent,Prop0)).
agent_text_command(Agent,['@','set',Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
agent_text_command(Agent,['@','get',Prop0],Agent,actGet(Agent,Prop0)).
agent_text_command(Agent,['@_set',Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
agent_text_command(Agent,['@_get',Prop0],Agent,actGet(Agent,Prop0)).

agent_call_command(Agent,actSet(Obj0,Prop0,Value0)) :- subst(add(dbase_t(Prop0,Obj0,Value0)),self,Agent,K),dmsg(K),debugOnError(K).

agent_call_command(Agent,actGet(Obj0,Prop0)) :- subst(dbase_t(Prop0,Obj0,Value),self,Agent,K), 
                                                        ccatch((findall(Value,(req(K),fmt(K)),L),
                                                          (L==[_|_]->true;fmt(missing(K)))),E,fmt('@get Error ~q',[E:K])).

:- include(logicmoo(vworld/moo_footer)).
