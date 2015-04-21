% :-swi_module(user). 
:-swi_module(get_set, []).
/** <module> Agent changes one of their own variables
% Douglas Miles 2014

*/
:- include(library(prologmud/server/mud_header)).

% :- register_module_type (mtCommand).

action_info(actGet(isOptional(ftTerm,isSelfAgent),tPred),ftText("@get term to a property")).
action_info(actSet(isOptional(ftTerm,isSelfAgent),tPred,ftTerm),ftText("@sets term to a property")).

user:agent_text_command(Agent,["@set",Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
user:agent_text_command(Agent,["@get",Prop0],Agent,actGet(Agent,Prop0)).
user:agent_text_command(Agent,["@","set",Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
user:agent_text_command(Agent,["@","get",Prop0],Agent,actGet(Agent,Prop0)).
user:agent_text_command(Agent,["@_set",Prop0,Value0],Agent,actSet(Agent,Prop0,Value0)).
user:agent_text_command(Agent,["@_get",Prop0],Agent,actGet(Agent,Prop0)).

user:agent_call_command(Agent,actSet(Obj0,Prop0,Value0)) :- coerce(Prop0,tPred,Prop,Prop0),subst(add(dbase_t(Prop,Obj0,Value0)),isSelfAgent,Agent,K),dmsg(K),debugOnError(K).

user:agent_call_command(Agent,actGet(Obj0,Prop0)) :- subst(dbase_t(Prop0,Obj0,Value),isSelfAgent,Agent,K), 
                                                        ccatch((findall(Value,(req(K),fmt(K)),L),
                                                          (L==[_|_]->true;fmt(wasMissing(K)))),E,fmt('@get Error ~q',[E:K])).

% :- include(library(prologmud/server/mud_footer)).
