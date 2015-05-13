/** <module>  
% Initial IRC/Text console 
% ALL IRC client MUD logic is here (removed from everywhere else!)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- include(prologmud(mud_header)).

:-dynamic(hooks:irc_event_hooks/3).
:-multifile(hooks:irc_event_hooks/3).

:-dynamic(hooks:deliver_event/2).
:-multifile(hooks:deliver_event/2).

:-dynamic(user:irc_user_plays/3).
:-multifile(user:irc_user_plays/3).

prologHybrid(user:irc_user_plays(tAgent,ftAtom,ftAtom)).
prologOrdered(agent_action_queue(tAgent,ftTerm)).


hooks:irc_event_hooks(Channel,User,Stuff):-irc_mud_event_hook(Channel,User,Stuff).

irc_mud_event_hook(Channel,User,Stuff):- string(User),string_to_atom(User,AUser),!,irc_mud_event_hook(Channel,AUser,Stuff).
irc_mud_event_hook(Channel,User,Stuff):- string(Channel),string_to_atom(Channel,AChannel),!,irc_mud_event_hook(AChannel,User,Stuff).

irc_mud_event_hook(Channel,User,say(TODO)):- user:irc_user_plays(Agent,User,Channel),nonvar(Agent),add(agent_action_queue(Agent,TODO)).

irc_mud_event_hook(Channel,User,call('?-'(foc_current_agent(Agent)),_Vs)):- foc_current_agent(Agent),add(user:irc_user_plays(Agent,User,Channel)).

hooks:deliver_event(Agent,Event):- user:irc_user_plays(Agent,User,Channel) -> eggdrop:say(Channel,[Agent,': ',NewEvent]) ; nop(eggdrop:say(Agent,Event)).

user:irc_user_plays(Agent,User,Channel)/
  ( user:irc_user_plays(Agent,User,Other), Other\=Channel )
   =>
   ~user:irc_user_plays(Agent,User,Other).


