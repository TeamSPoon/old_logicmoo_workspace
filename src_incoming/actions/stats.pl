:- module(stats, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% damage(Agent,Dam) = damage
% success(Agent,Suc) = checks success of last action (actually checks the failure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(command).

% ====================================================
% show the stats system
% ====================================================
moo:action_help(stats(optional(term,self)), "Examine MUD stats of something").
moo:agent_call_command(Agent,stats(SWhat)):- 
   term_listing(SWhat),
   doall((parse_for(optional(agent,self),SWhat,What,_LeftOver),
   show_kb_preds(Agent,[
         charge(What,value),
         str(What,value),
         score(What,value),
         damage(What,value),
         height(What,value)]))).


:- include(logicmoo(vworld/moo_footer)).

