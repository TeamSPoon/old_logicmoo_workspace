% :-swi_module(user). 
:-swi_module(modStats, []).
/** <module> A command to  ...
% charge(Agent,Chg) = charge (amount of charge agent has)
% health(Agent,Dam) = damage
% wasSuccess(Agent,Suc) = checks success of last action (actually checks the cmdfailure predicate)
% score(Agent,Scr) = score
% to do this.
% Douglas Miles 2014
*/
:- include(prologmud(mud_header)).

% :- register_module_type (mtCommand).

% ====================================================
% show the stats system
% ====================================================
action_info(actStats(isOptional(tObj,isSelfAgent)), "Examine MUD stats of something").
user:agent_call_command(Agent,actStats(What)):-    
   show_kb_preds(Agent,[
         mudEnergy(What,value),
         mudStr(What,value),
         mudStm(What,value), % stamina
         mudScore(What,value),
         mudHealth(What,value),
         mudHeight(What,value)]),!,
   term_listing(What),!.


action_info(actGrep(ftTerm),"grep for a term").



/*
There are 7 aptitudes in Eclipse Phase:
� Cognition (COG) is your aptitude for problemsolving,
logical analysis, and understanding. It
also includes memory and recall.

� Coordination (COO) is your skill at integrating
the actions of different parts of your morph
to produce smooth, successful movements. It
includes manual dexterity, fine motor control,
nimbleness, and balance.
� Intuition (INT) is your skill at following your
gut instincts and evaluating on the fly. It includes
physical awareness, cleverness, and cunning.
� Reflexes (REF) is your skill at acting quickly. This
encompasses your reaction time, your gut-level
response, and your ability to think fast.
� Savvy (SAV) is your mental adaptability, social
intuition, and proficiency for interacting
with others. It includes social awareness and
manipulation.
� Somatics (SOM) is your skill at pushing your
morph to the best of its physical ability, including
the fundamental utilization of the morph�s strength,
endurance, and sustained positioning and motion.
� Willpower (WIL) is your skill for self-control,
your ability to command your own destiny.
*/

action_info(actGrep(isOptional(ftTerm,isSelfAgent)), "Examine MUD listing of something").
user:agent_call_command(_Gent,actGrep(Obj)):- term_listing(Obj).

% :- include(prologmud(mud_footer)).
