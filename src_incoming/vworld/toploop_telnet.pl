/** <module>  
% Initial Telnet/Text console 
% ALL telnet client business logic is here (removed from everywhere else!)
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- module(toploop_telnet, [
                  agent_message_stream/3,
                  do_player_action/1,
                  look_brief/1,
                  telnet_look/1,
                  show_room_grid/1,
                  inst_label/2,
                  display_grid_labels/0,
                  start_mud_telent/1,
                  read_and_do_telnet/1,
                  remove_dupes/2,
                  telnet_fmt_shown/3,
                   login_and_run/0]).

:- dynamic agent_message_stream/3, telnet_fmt_shown/3.

:- meta_predicate toploop_telnet:show_room_grid_single(*,*,0).

:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

% ===========================================================
% TELNET REPL + READER
% ===========================================================
start_mud_telent(Port):- telnet_server(Port, [allow(_ALL),call_pred(login_and_run)]),!.

login_and_run:-
  foc_current_player(P),
   fmt('~n~n~nHello ~w! Welcome to the MUD!~n',[P]),
   run_player_telnet(P),!,
   fmt('~n~n~Goodbye ~w! ~n',[P]).

run_player_telnet(P) :-    
   repeat,foc_current_player(P),
      get_session_id(O),
      with_assertions(thlocal:current_agent(O,P),once(read_and_do_telnet(P))), retract(wants_logout(P)).

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

do_player_action(VA):- debug, foc_current_player(Agent), do_player_action(Agent,VA),!.

do_player_action(Agent,CMD):-var(CMD),!,fmt('unknown_var_command(~q,~q).',[Agent,CMD]).
do_player_action(_,end_of_file):-tick_tock.
do_player_action(_,''):-tick_tock.
do_player_action(Agent,CMD):- call_agent_command(Agent, CMD),!.
% do_player_action(Agent,CMD):- trace, call_agent_command(Agent, CMD),!.
do_player_action(Agent,CMD):-fmt('unknown_call_command(~q,~q).',[Agent,CMD]).


% ===========================================================
% DEFAULT TELNET "LOOK"
% ===========================================================

look_brief(Agent):- prop(Agent,last_command,X),functor(X,look,_),!.
look_brief(Agent):- telnet_look(Agent).

look_via_pred(_,[]).
look_via_pred(Pred,[L|List]):-!,
   catch((ignore(look_via_pred_0(Pred,L);dmsg(failed(look_via_pred_0(Pred,L))))),E,dmsg(error_failed(E,look_via_pred_0(Pred,L)))),
   look_via_pred(Pred,List).

look_via_pred_0(Pred,F=Call):- !,look_via_pred_1(Pred,F,Call).
look_via_pred_0(Pred,once(Call)):- !,functor(Call,F,_), look_via_pred_1(Pred,F,once(Call)).
look_via_pred_0(Pred,all(Call)):- !,functor(Call,F,_), look_via_pred_1(Pred,F,all(Call)).
look_via_pred_0(Pred,Call):- functor(Call,F,_), look_via_pred_1(Pred,F,Call).

look_via_pred_1(Pred,F,all(Call)):-!,look_via_pred_2(Pred,F,Call).
look_via_pred_1(Pred,F,once(Call)):-!,look_via_pred_2(Pred,F,once(Call)).
look_via_pred_1(Pred,F,Call):-look_via_pred_2(Pred,F,Call).

look_via_pred_2(Pred,F,Call0):-
      wsubst(Call0,value(Transform),NewValue,Call),
      ignore(Transform = (object_string_fmt) ),
      wsubst(Call,value,NewValue,GCall),
      doall((catch(call(GCall),Error, NewValue=Error), 
             fmt_call(Pred,Transform,F,NewValue))).


object_string_fmt(Obj,String):- object_string(Obj,Str), String = Str.

fmt_call(Pred,Transform,F,NewValue):-flatten([NewValue],ValueList), NewValue\=ValueList,fmt_call(Pred,Transform,F,ValueList).
fmt_call(Pred,Transform,N,[V]):-fmt_call_pred(Pred,Transform,N,V),!.
fmt_call(Pred,Transform,N,[V|VV]):-remove_dupes([V|VV],RVs),reverse(RVs,Vs),fmt_call_pred(Pred,Transform,N,Vs),!.
fmt_call(Pred,Transform,N,V):-fmt_call_pred(Pred,Transform,N,V),!.

fmt_call_pred(Pred,Transform,N,[L|List]):-!, doall((member(V,[L|List]),fmt_call_pred_trans(Pred,Transform,N,V))).
fmt_call_pred(Pred,Transform,N,V0):-fmt_call_pred_trans(Pred,Transform,N,V0).

fmt_call_pred_trans(Pred,Transform,N,V0):-must((debugOnError(call(Transform,V0,V)),!,debugOnError(call(Pred,N,V)))).



remove_dupes(In,Out):-remove_dupes(In,Out,[]).

remove_dupes([],[],_):-!.
remove_dupes([I|In],Out,Shown):-member(I,Shown),!,remove_dupes(In,Out,Shown).
remove_dupes([I|In],[I|Out],Shown):-remove_dupes(In,Out,[I|Shown]).


telnet_look_objs([]).
telnet_look_objs([O|Objs]):-!,telnet_look_obj(O),!,telnet_look_objs(Objs).

telnet_look_obj(O):-number(O).
telnet_look_obj(error(_,_)).
telnet_look_obj(O):-fresh_line,telnet_print_object_desc(_,O),!.

telnet_look(Agent):-
        scan_updates,!,
        must(atloc(Agent,LOC)),!,
        locationToRegion(LOC,Region),
        must(show_room_grid(Region)),
        must(telnet_print_object_desc(Agent,Region,4,6,'the name of this place',99)),!,
        setof(D-E,pathBetween_call(Region,D,E),Set),
        forall_member(D-E,Set,once(telnet_print_path(Region,D,E))),
        must(deliver_location_events(Agent,LOC)),!,
         gensym(telnet_fmt,TL),
         look_via_pred(telnet_fmt(TL),
         [
         charge(Agent,value(=)),
         movedist(Agent,value),
         damage(Agent,value(=)),
         success=look:success(Agent,value),
         score(Agent,value),
         inventory(Agent,value),
         all(get_feet(Agent,value)),
         get_near(Agent,value),
         height(Agent,value),
         facing(Agent,value),
         height_on_obj(Agent,value),
         get_percepts(Agent,value),
         inRegion(value,Region)
         ]),
      retractall(telnet_fmt_shown(TL,_,_)).

telnet_print_path(Region,D,E):-
   req(pathName(Region,D,S)) -> fmt('~w.',[S]) ;
   nameStrings(E,NS) -> fmt('~w is ~w',[D,NS]) ;
   fmt('~w ~w',[D,E]).   

telnet_fmt(TL,N,V):-telnet_fmt_shown(TL,N,V),!.
telnet_fmt(TL,N,V):-assert_if_new(telnet_fmt_shown(TL,N,V)),fmt('~q.~n',[N=V]).
values_shown(TL,Vs):-findall(V,(telnet_fmt_shown(TL,_,VV),flatten([VV],VVV),member(V,VVV)),Vs).
values_shown_strings(TL):-values_shown(TL,Vs),remove_dupes(Vs,Objs),reverse(Objs,RObjs),telnet_look_objs(RObjs).


telnet_print_object_desc(_Agent,O,LOW,_GOOD,WhatString,_Max):-
   order_descriptions(O,LOW-199,[Region|IST]) -> forall_member(M,[Region|IST],fmt0('~w.  ',[M])) ;
   setof(S,nameStrings(O,S),[Region|IST]) ->   forall_member(M,[Region|IST],fmt0('~w is ~w. ',[M,WhatString])) ;
   ((setof(S,((mud_isa_a(O,S),nonvar(S))),List),fmt('~q is ~w',[mud_isa(O,List),WhatString]))).

mud_isa_a(O,S):-mud_isa(O,S).
mud_isa_a(O,classof(O)).


telnet_print_object_desc(Agent,O):-
 telnet_print_object_desc(Agent,O,4,6,'is here.',1).

%% divide_match(+O,+InList,-Matches,-NotMatches)
divide_match(O,InList,Matches,NotMatches):-
   divide_match0(O,InList,MatchesI,NotMatchesI),!,
   NotMatches=NotMatchesI,Matches=MatchesI.

divide_match0(_,More,[],[]):-More==[],!.
divide_match0(O,[Test|For],True,False):-
   props(O,Test ) ->
   divide_match(O,For,[Test|True],False);
   divide_match(O,For,True,[Test|False]).

deliver_location_events(_Agent,_LOC):-true.

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
        props(Obj,ofclass(agent)),
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
        prop(Obj,ofclass,Class),
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



:- use_module(library(socket)).

%%	telnet_server(?Port, +Options)
%
%	Create a TCP/IP based server  on  the   given  Port,  so you can
%	telnet into Prolog and run an  interactive session. This library
%	is intended to provide access for   debugging  and management of
%	embedded servers.
%
%	Currently defined options are:
%
%		* allow(IP)
%		Allow access from IP, a term of the format ip(A,B,C,D).
%		Multiple of such terms can exist and access is granted
%		if the peer IP address unifies to one of them.  If no
%		allow option is provided access is only granted from
%		ip(127,0,0,1) (localhost).
%
%	For example:
%
%		==
%		?- telnet_server(4000, []).
%
%		% telnet localhost 4000
%		Welcome to the SWI-Prolog server on thread 3
%
%		1 ?-
%		==
%
%	@bug As the connection does not involve a terminal, command history
%	and completion are not provided. Neither are interrupts
%	(Control-C).  To terminate the Prolog shell one must enter the
%	command "end_of_file."


telnet_server(_Port, _Options) :- thread_property(X, status(running)),X=telnet_server,!.

telnet_server(Port, Options) :-
	tcp_socket(ServerSocket),
	tcp_setopt(ServerSocket, reuseaddr),
	tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5),
	thread_create(server_loop(ServerSocket, Options), _,
		      [ alias(telnet_server)
		      ]).

server_loop(ServerSocket, Options) :-
	tcp_accept(ServerSocket, Slave, Peer),
	tcp_open_socket(Slave, InStream, OutStream),
	set_stream(InStream, close_on_abort(false)),
	set_stream(OutStream, close_on_abort(false)),
	tcp_host_to_address(Host, Peer),
	(   Postfix = []
	;   between(2, 1000, Num),
	    Postfix = [-, Num]
	),
	atomic_list_concat(['client@', Host | Postfix], Alias),
	catch(thread_create(
		  service_client(InStream, OutStream, Peer, Options),
		  _,
		  [ alias(Alias)
		  ]),
	      error(permission_error(create, thread, Alias), _),
	      fail), !,
	server_loop(ServerSocket, Options).

service_client(InStream, OutStream, Peer, Options) :-
	allow(Peer, Options), !,
        call_pred(Call, Options), !,
	thread_self(Id),
	set_prolog_IO(InStream, OutStream, OutStream),
	set_stream(InStream, tty(true)),
	set_prolog_flag(tty_control, false),
	current_prolog_flag(encoding, Enc),
	set_stream(user_input, encoding(Enc)),
	set_stream(user_output, encoding(Enc)),
	set_stream(user_error, encoding(Enc)),
	set_stream(user_input, newline(detect)),
	set_stream(user_output, newline(dos)),
	set_stream(user_error, newline(dos)),
	'format'(user_error,'Welcome to the SWI-Prolog LogicMOO server on thread ~w~n~n', [Id]),
	call_cleanup(Call,
		     ( close(InStream),
		       close(OutStream),
		       thread_detach(Id))).
service_client(InStream, OutStream, _, _):-
	thread_self(Id),
	'format'(OutStream, 'Go away!!~n', []),
	close(InStream),
	close(OutStream),
	thread_detach(Id).

allow(Peer, Options) :-
	(   member(allow(Allow), Options)
	*-> Peer = Allow,
	    !
	;   Peer = ip(127,0,0,1)
	).

call_pred(Call, Options) :-
	(   member(call_pred(Allow), Options)
	*-> Call = Allow,
	    !
	;   Call = prolog
	).



:- include(logicmoo('vworld/moo_footer.pl')).

