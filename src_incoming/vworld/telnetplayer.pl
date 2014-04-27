%
% Dec 13, 2035
% Douglas Miles
%
/** <module>  Initial Telnet/Text console ALL text client bussiness logic is here and removed from everywhere else!
%
*/

:- module(telnetplayer, [
                  agent_message_stream/3,
                  do_player_action/1,
                   login_and_run/0]).

:- dynamic(agent_message_stream/3).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).

% ===========================================================
% TELNET REPL + READER
% ===========================================================

login_and_run:-
  foc_current_player(P),
   fmt('~n~n~nHello ~w! Welcome to the MUD!~n',[P]),
   run_player_telnet(P),!,
   fmt('~n~n~Goodbye ~w! ~n',[P]).

run_player_telnet(P) :- repeat,foc_current_player(P),with_assertions(thlocal:current_agent(P),once(read_and_do_telnet(P))), retract(wants_logout(P)).

read_and_do_telnet(P):-
            must(ignore(look_brief(P))),!,
   current_input(Input),
   current_output(Output),
   retractall(agent_message_stream(P,_,_)),
   assert(agent_message_stream(P,Input,Output)),
            notrace((sformat(S,'~w>',[P]),prompt_read(S,List))),!,
            must(once(do_player_action(List))),!.

prompt_read(Prompt,Atom):-
        current_input(In),
        fresh_line,
        fmt0('~n~w ',[Prompt]),
	read_line_to_codes(In,Codes),
        foc_current_player(P),
         (is_list(Codes)-> atom_codes(Atom,Codes);
           assert(wants_logout(P))),!.

tick_tock:-fmt('tick tock',[]),sleep(1),!.


% ===========================================================
% USES PACKRAT PARSER 
% ===========================================================

do_player_action(VA):- foc_current_player(Agent), call_player_action(Agent,VA),!.


call_player_action(Agent,CMD):-var(CMD),!,fmt('unknown_var_command(~q,~q).',[Agent,CMD]).
call_player_action(_,end_of_file):-tick_tock.
call_player_action(_,''):-tick_tock.
call_player_action(Agent,CMD):-do_player_action(Agent, CMD),!.
% call_player_action(Agent,CMD):-do_player_action(Agent, CMD),!.
call_player_action(Agent,CMD):-fmt('unknown_call_command(~q,~q).',[Agent,CMD]).


% execute a prolog command including prolog/0
do_player_action(Agent,[VERB|ARGS]):-
      debugOnError((moo:agent_text_command(Agent,[VERB|ARGS],NewAgent,CMD))),
      debugOnError(moo:agent_call_command(NewAgent,CMD)),!,
      atloc(Agent,Where),
      raise_location_event(Where,notice(reciever,do(Agent,CMD))),!.

% lists
do_player_action(A,Atom):-atom(Atom),atomSplit(Atom,List),do_player_action(A,List).
% prolog command
do_player_action(_Gent,Atom):- atom(Atom), catch(((once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),!,
      fmt('doing command ~q~n',[OneCmd]))), ignore((OneCmd,fmt('Yes: ~w',[VARS]),fail)))),_,fail).

% remove period at end
do_player_action(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,do_player_action(A,New).



:- include(logicmoo('vworld/vworld_footer.pl')).


