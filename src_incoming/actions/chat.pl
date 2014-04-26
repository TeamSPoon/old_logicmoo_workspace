% say.pl
% May 18, 1996
% John Eikenberry
%
% Dec 13, 2035
% Douglas Miles
%
/** <module> 
% This file defines the predicates for the agent to say
%
*/

:- module(chat, []).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(command).

moo:agent_text_command(Agent,[Dir],Agent,move(Dir)):-
        specifier_text(Dir,dir),!.

moo:decl_action(Say,text("invokes",Does)):-socialCommand(Say,_SocialVerb,Does).

agentOrRegionOrGossup(A):-region(A).
agentOrRegionOrGossup(A):-agent(A).
agentOrRegionOrGossup(gossup).

socialCommand(Say,SocialVerb,chat(SocialVerb,agentOrRegionOrGossup,string)):-socialVerb(SocialVerb), Say =.. [SocialVerb,agentOrRegion,string].
socialVerb(SocialVerb):-member(SocialVerb,[say,whisper,emote,tell,ask,shout,gossup]).

moo:agent_text_command(Agent,[Say|What],Agent,CMD):-
      socialVerb(Say),
      chat_to_callcmd(Agent,Say,What,CMD),!.

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

chat_command_parse_2(Agent,Say,Where,What,prologCall(do_social(Agent,Say,Where,What))).

do_social(Agent,Say,Whom,Text):-
   atloc(Agent,Where),
   asInvoked([Say,Agent,Whom,Text],Cmd),
   raise_location_event(Where,notice(reciever,Cmd)).

:- include(logicmoo('vworld/vworld_footer.pl')).


