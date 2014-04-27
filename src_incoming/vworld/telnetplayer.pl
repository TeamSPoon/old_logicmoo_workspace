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
                  look_brief/1,
                  telnet_look/1,
                  show_room_grid/1,
                  display_grid_labels/0,
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


scan_updates:-ignore(catch(make,_,true)).



% ===========================================================
% USES PACKRAT PARSER 
% ===========================================================

do_player_action(VA):- foc_current_player(Agent), call_player_action(Agent,VA),!.


call_player_action(Agent,CMD):-var(CMD),!,fmt('unknown_var_command(~q,~q).',[Agent,CMD]).
call_player_action(_,end_of_file):-tick_tock.
call_player_action(_,''):-tick_tock.
call_player_action(Agent,CMD):-do_player_action(Agent, CMD),!.
% call_player_action(Agent,CMD):- trace, do_player_action(Agent, CMD),!.
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



% ===========================================================
% DEFAULT TELNET "LOOK"
% ===========================================================

look_brief(Agent):- prop(Agent,last_command,X),functor(X,look,_),!.
look_brief(Agent):- telnet_look(Agent).


telnet_look(Agent):-
        scan_updates,!,
        must(atloc(Agent,LOC)),!,
        locationToRegion(LOC,Region),
        must(telnet_print_grid_and_region_name(Agent,Region)),!,
        ignore(telnet_print_exits(Agent,LOC)),!,
        must(telnet_print_region_objects(Agent,LOC)),!,
        must(deliver_location_events(Agent,LOC)),!,
        ignore(get_all(Agent,Chg,Dam,Suc,Scr,Percepts,Inv)),
        ignore(height(Agent,Ht)),
        ignore(facing(Agent,Facing)),
        fmt('Agent=~w Ht=~w Facing=~w LOC=~w Charge=~w Dam=~w Success=~w Score=~w Inventory=~q ~n', [Agent,Ht,Facing,LOC,Chg,Dam,Suc,Scr,Inv]),
	ignore((nonvar(Percepts),write_pretty(Percepts))),
        must(ignore(((get_near(Agent,Stuff),
        fmt('STUFF=~q',[Stuff]))))),!.

telnet_print_exits(_Agent,LOC):-
  locationToRegion(LOC,Region),
   setof(D-E,pathBetween_call(Region,D,E),Set),
   forall_member(D-E,Set,once(telnet_print_path(Region,D,E))).

telnet_print_path(Region,D,E):-
   req(pathName(Region,D,S)) -> fmt('~w.',[S]) ;
   nameStrings(E,NS) -> fmt('~w is ~w',[D,NS]) ;
   fmt('~w ~w',[D,E]).   

telnet_print_grid_and_region_name(Agent,Region):-
      must(show_room_grid(Region)),
      must(telnet_print_object_desc(Agent,Region,4,6,'the name of this place',99)),!.

telnet_print_object_desc(_Agent,O,LOW,_GOOD,WhatString,_Max):-
   order_descriptions(O,LOW-199,[Region|IST]) -> forall_member(M,[Region|IST],fmt0('~w.  ',[M])) ;
   setof(S,nameStrings(O,S),[Region|IST]) ->   forall_member(M,[Region|IST],fmt0('~w is ~w. ',[M,WhatString])) ;
   setof(S,mud_isa(O,S),List), fmt('~q is ~w',[mud_isa(O,List),WhatString]).

telnet_print_region_objects(Agent,Region):- !,ignore((
     setof(O,inRegion(O,Region),Set),
       forall_member(O,Set,telnet_print_object_desc(Agent,O)))).

telnet_print_region_objects(Agent,Region):-
       props(Agent,[id(X)]),
       telnet_print_region_objects_except(Agent,Region,[same(X),classof(invisible)]).

telnet_print_region_objects_except(Agent,Region,ExceptFor):-
 setof(O,inRegion(O,Region),Set),
 forall(member(O,Set),(divide_match(O,ExceptFor,_,[]),telnet_print_object_desc(Agent,O))).


telnet_print_object_desc(Agent,O):-
 telnet_print_object_desc(Agent,O,4,6,'is here.',1).

%% divide_match(-O,-InList,+Matches,+NotMatches)
divide_match(O,InList,Matches,NotMatches):-
   divide_match0(O,InList,MatchesI,NotMatchesI),!,
   NotMatches=NotMatchesI,Matches=MatchesI.

divide_match0(_,More,[],[]):-More==[],!.
divide_match0(O,[Test|For],True,False):-
   props(O,Test ) ->
   divide_match(O,For,[Test|True],False);
   divide_match(O,For,True,[Test|False]).

deliver_location_events(_Agent,_LOC):-true.

order_descriptions(O,DescSpecs,ListO):-findall(S,(description(O,S),meets_desc_spec(S,DescSpecs)),Rev),reverse(Rev,List),delete_repeats(List,ListO).

delete_repeats([],[]):-!.
delete_repeats([Region|List],[Region|ListO]):-delete(List,Region,ListM), delete_repeats(ListM,ListO),!.

meets_desc_spec(S0,_L):- string_to_atom(S0,A),atomic_list_concat([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(S,From-To):- desc_len(S,Region),!, Region =< To, Region >= From.
meets_desc_spec(_,_).

desc_len(S0,Region):-string_to_atom(S0,S),
   atomic_list_concat(Words,' ',S),length(Words,Ws),atomic_list_concat(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.

% Display what the agent sees in a form which
% makes sense to me
write_pretty([]).
write_pretty(Percepts) :-
	write_pretty_aux(Percepts, Rest, 0),
	nl,
	write_pretty(Rest).

write_pretty_aux(Rest,Rest,5).
write_pretty_aux([[]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	label_type(Obj,0),
	write(Obj), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[dark]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	write('dk '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[Head]|Tail], Return, Column) :-
	Ctemp is Column + 1,
	label_type(Map,Head),
	write(Map), write(' '),
	write_pretty_aux(Tail, Return, Ctemp).
write_pretty_aux([[Agent]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	mud_isa(Agent,agent),
	write('Ag'), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[_|_]|Tail],Return,Column) :-
	Ntemp is Column + 1,
	write('A+'), write(' '),
	write_pretty_aux(Tail,Return,Ntemp).



show_room_grid(Room) :-show_room_grid_new(Room),!.
% show_room_grid(Room) :-show_room_grid_old(Room),!.

% ===================================================================
% show_room_grid_new(Room)
% ===================================================================
show_room_grid_new(Room):-
   grid_size(Room,Xs,Ys,_Zs),
   Ys1 is Ys+1,Xs1 is Xs+1,
   between(0,Ys1,Y),
   nl, between(0,Xs1,X),
   loc_to_xy(Room,X,Y,LOC),
   write(' '),
   OutsideTest = (not(between(1,Xs,X));not(between(1,Ys,Y))),
   once(show_room_grid_single(Room,LOC,OutsideTest)),fail.
show_room_grid_new(_):-nl.


door_label(R,Dir,'  '):-pathBetween_call(R,Dir,SP),atomic(SP).

show_room_grid_single(Room, xyz(Room,X,Y,Z),OutsideTest):- OutsideTest, doorLocation(Room,X,Y,Z,Dir), door_label(Room,Dir,Label),write(Label),!.
show_room_grid_single(_Room,_LOC,OutsideTest):-OutsideTest,!,write('[]'),!.
show_room_grid_single(_Room,LOC,_OutsideTest):- atloc(Obj,LOC),inst_label(Obj,Label), write(Label), !.
show_room_grid_single(_Room,LOC,_OutsideTest):- atloc(_Obj,LOC),write('..'), !.
show_room_grid_single(_Room,_LOC,_OutsideTest):- write('--'), !.

inst_label(Obj,Label):-label_type(Label,Obj),!.
inst_label(Obj,Label):-  props(Obj,nameStrings(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(Obj,Label):-  props(Obj,named(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(Obj,Label):-  props(Obj,isa(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(Obj,SLabe2):-term_to_atom(Obj,SLabel),sub_atom(SLabel,1,2,_,SLabe2),!.
inst_label(Obj,SLabe2):-term_to_atom(Obj,SLabel),sub_atom(SLabel,0,2,_,SLabe2),!.
inst_label(_Obj,'&&').

% ===================================================================
% show_room_grid_old(Room)
% ===================================================================
% Display world
show_room_grid_old(Room) :-  
	grid(Room,1,G,_),
	length(G,N),
	M is N + 1,
	show_room_grid(Room,1,1,M).

show_room_grid(Room,Old,N,N) :-
	New is Old + 1,
	\+ grid(Room,New,N,_),
	nl,
	!.

show_room_grid(Room,Old,N,N) :-
	New is Old + 1,
	nl,
	!,
	show_room_grid(Room,New,1,N).
show_room_grid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	atloc(Obj,LOC),
        props(Obj,classof(agent)),
	list_agents(Agents),
	obj_memb(Agent,Agents),
	atloc(Agent,LOC),
	write('Region1+'), write(' '),
	XX is X + 1,
	!,
	show_room_grid(Room,Y,XX,N).
show_room_grid(Room,Y,X,N) :-
        loc_to_xy(Room,X,Y,LOC),
	atloc(Obj,LOC),
        prop(Obj,classof,Class),
	label_type(Label,Class),
	write(Label), write(' '),
	XX is X + 1,
	!,
	show_room_grid(Room,Y,XX,N).
show_room_grid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	atloc(Agent,LOC),
	mud_isa(Agent,agent),
	write('Ag'), write(' '),
	XX is X + 1,
	!,
	show_room_grid(Room,Y,XX,N).


% Used to display the labels of the grid locations. (the key to the map).
% Used at end of run.
display_grid_labels :-
	findall([Label,Name],label_type(Label,Name),List),
	forall(prop_memb([Label,Name],List),
	           (write(Label), write('='), write(Name), write(' '))),
		   nl.


:- include(logicmoo('vworld/vworld_footer.pl')).

