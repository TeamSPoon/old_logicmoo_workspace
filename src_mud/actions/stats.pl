% :-swi_module(user). 
:-swi_module(stats, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% health(Agent,Dam) = damage
% success(Agent,Suc) = checks success of last action (actually checks the cmdfailure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(logicmoo(vworld/moo_header)).

:- register_module_type(command).

% ====================================================
% show the stats system
% ====================================================
action_info(stats(optional(obj,self)), "Examine MUD stats of something").
agent_call_command(Agent,stats(What)):-    
   show_kb_preds(Agent,[
         charge(What,value),
         str(What,value),
         stm(What,value), % stamina
         score(What,value),
         health(What,value),
         height(What,value)]),!,
   term_listing(What),!.


action_info(list(term),"grep for a term").



/*
There are 7 aptitudes in Eclipse Phase:
• Cognition (COG) is your aptitude for problemsolving,
logical analysis, and understanding. It
also includes memory and recall.

• Coordination (COO) is your skill at integrating
the actions of different parts of your morph
to produce smooth, successful movements. It
includes manual dexterity, fine motor control,
nimbleness, and balance.
• Intuition (INT) is your skill at following your
gut instincts and evaluating on the fly. It includes
physical awareness, cleverness, and cunning.
• Reflexes (REF) is your skill at acting quickly. This
encompasses your reaction time, your gut-level
response, and your ability to think fast.
• Savvy (SAV) is your mental adaptability, social
intuition, and proficiency for interacting
with others. It includes social awareness and
manipulation.
• Somatics (SOM) is your skill at pushing your
morph to the best of its physical ability, including
the fundamental utilization of the morph’s strength,
endurance, and sustained positioning and motion.
• Willpower (WIL) is your skill for self-control,
your ability to command your own destiny.
*/

action_info(list(optional(term,self)), "Examine MUD listing of something").
agent_call_command(_Gent,list(Obj)):- term_listing(Obj).

:- include(logicmoo(vworld/moo_footer)).

