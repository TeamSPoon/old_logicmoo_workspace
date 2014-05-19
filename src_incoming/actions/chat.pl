/** <module>  This file defines the predicates for the agent to socialize
% Dec 13, 2035
% Douglas Miles
%
*/

:- module(chat, [do_social/4]).

:- include(logicmoo('vworld/moo_header.pl')).

:- moo:register_module_type(command).

do_social(Agent,Say,Whom,Text):- 
   atloc(Agent,Where),
   asInvoked(Cmd,[Say,Agent,Whom,Text]),
   raise_location_event(Where,notice(reciever,Cmd)).

moo:decl_action(Say,text("invokes",Does)):-socialCommand(Say,_SocialVerb,Does).

socialCommand(Say,SocialVerb,chat(optional(verb,SocialVerb),optional(channel,here),string)):-socialVerb(SocialVerb),
   Say =.. [SocialVerb,optional(channel,here),string].
socialVerb(SocialVerb):-member(SocialVerb,[say,whisper,emote,tell,ask,shout,gossup]).

moo:agent_text_command(Agent,[Say|What],Agent,CMD):-
      socialVerb(Say),
      once(chat_to_callcmd(Agent,Say,What,CMD)).

% ask joe about some text
chat_to_callcmd(Agent,ask,What,CMD):-append([Whom,about],About,What),!,chat_command_parse_2(Agent,ask,Whom,About,CMD).
% ask joe some text
chat_to_callcmd(Agent,ask,What,CMD):-append([Whom],About,What),mud_isa(Whom,agent),!,chat_command_parse_2(Agent,ask,Whom,About,CMD).
% say to joe some text 
chat_to_callcmd(Agent,Say,What,CMD):-append([to,Whom],Text,What),!,chat_command_parse_2(Agent,Say,Whom,Text,CMD).
% say some text to joe
chat_to_callcmd(Agent,Say,What,CMD):-append(Text,[to,Whom],What),!,chat_command_parse_2(Agent,Say,Whom,Text,CMD).
% say some text
chat_to_callcmd(Agent,Say,What,CMD):-atloc(Agent,Where),chat_command_parse_2(Agent,Say,Where,What,CMD).

chat_command_parse_2(Agent,Say,Where,Text,do_social(Agent,Say,Where,Text)).

moo:agent_call_command(Agent,do_social(Agent,Say,Whom,Text)):- must(do_social(Agent,Say,Whom,Text)).

:- include(logicmoo('vworld/moo_footer.pl')).


