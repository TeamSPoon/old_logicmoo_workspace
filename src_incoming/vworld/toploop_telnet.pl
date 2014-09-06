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
                  do_player_action/1,
                  look_brief/1,
                  show_room_grid/1,
                  inst_label/2,
                  display_grid_labels/0,
                  telnet_repl_writer/4,
                  telnet_repl_obj_to_string/3,
                  start_mud_telnet/1,
                  read_and_do_telnet/1,
                  run_player_telnet/1,
                   login_and_run/0]).

:- decl_thlocal wants_logout/1.

:- dynamic thglobal:agent_message_stream/3, telnet_fmt_shown/3.

:- meta_predicate toploop_telnet:show_room_grid_single(*,*,0).

:- include(logicmoo(vworld/moo_header)).

:- moo:register_module_type(utility).


% ===========================================================
% TELNET REPL + READER
% ===========================================================
start_mud_telnet(Port):- telnet_server(Port, [allow(_ALL),call_pred(login_and_run)]),!.

login_and_run:-
  foc_current_player(P),
   ensure_player_stream_local(P,_,_),
  threads,
   do_player_action(P,'who'),
   call_agent_command(P,'look'),
   fmt('~n~n~nHello ~w! Welcome to the MUD!~n',[P]),
   % sets some IO functions
   call_cleanup((
   with_assertions(repl_writer(P,telnet_repl_writer),
      with_assertions(repl_to_string(P,telnet_repl_obj_to_string),
     % runs the Telnet REPL
     run_player_telnet(P))),  
    fmt('~n~nGoodbye ~w! ~n',[P])),
     retractall(thlocal:session_agent(_,P))).


run_player_telnet(P) :-    
      foc_current_player(P),
      get_session_id(O),
      retractall(thlocal:wants_logout(P)),
      must(thlocal:repl_writer(P,_)),!,
    with_no_assertions(thglobal:use_cyc_database,
     with_no_assertions(thlocal:useOnlyExternalDBs, 
      with_assertions(thlocal:session_agent(O,P),
       ((repeat,
        once(read_and_do_telnet(P)), 
      retract(thlocal:wants_logout(P)),
        retractall(thglobal:agent_message_stream(P,_,_))))))).


ensure_player_stream_local(P,Input,Output):-
   current_input(Input),
   current_output(Output),
   (thglobal:agent_message_stream(P,Input,Output)->true;
      ((retractall(thglobal:agent_message_stream(P,_,_)),
     assert(thglobal:agent_message_stream(P,Input,Output))))),!.

read_and_do_telnet(P):-
   ensure_player_stream_local(P,_,_),
         must(ignore(look_brief(P))),!,
           sformat(S,'~w>',[P]),prompt_read_telnet(S,List),
            must(once(do_player_action(List))),!.


:-export(prompt_read/2).
prompt_read_telnet(Prompt,Atom):-
     foc_current_player(P),
      prompt_read(Prompt,IAtom),
      (IAtom==end_of_file -> (assert(thlocal:wants_logout(P)),Atom='quit') ; IAtom=Atom).

:-export(prompt_read/2).
prompt_read(Prompt,Atom):-
        fresh_line,
        fmt0('~n~w ',[Prompt]),
        current_input(In),
	read_line_to_codes(In,Codes),   
        (is_list(Codes)-> atom_codes(Atom,Codes); Atom=Codes),!.

:-export(scan_updates/0).

tick_tock:-
           scan_updates,!,fmt('tick tock',[]),sleep(0.1),!.

scan_updates:- !.
scan_updates:- ignore((thread_self(main),ignore((catch(make,E,dmsg(E)))))).

hook:decl_database_hook(Type,C):- current_agent(Agent),interesting_to_player(Type,Agent,C).

interesting_to_player(Type,Agent,C):- contains_var(C,Agent),dmsg(agent_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),contains_var(C,Region),dmsg(region_database_hook(Type,C)),!.
interesting_to_player(Type,Agent,C):-is_asserted(localityOfObject(Agent,Region)),is_asserted(localityOfObject(Other,Region)),contains_var(C,Other),!,dmsg(other_database_hook(Type,C)),!.

% ===========================================================
% USES PACKRAT PARSER 
% ===========================================================

:-export(do_player_action/1).
do_player_action(VA):- debug, foc_current_player(Agent),!, do_player_action(Agent,VA),!.

:-export(do_player_action/2).
do_player_action(Agent,CMD):-var(CMD),!,fmt('unknown_var_command(~q,~q).',[Agent,CMD]).
do_player_action(_,EOF):- end_of_file == EOF, !, tick_tock.
do_player_action(_,''):-!, tick_tock.
do_player_action(Agent,CMD):- call_agent_command(Agent, CMD),!.
do_player_action(Agent,CMD):-fmt('skipping_unknown_player_action(~q,~q).~n',[Agent,CMD]),!,fail.


% ===========================================================
% DEFAULT TELNET "LOOK"
% ===========================================================
look_brief(Agent):- prop(Agent,last_command,X),nonvar(X),functor(X,look,_),!.
look_brief(Agent):- not(prop(Agent,needs_look,true)),!.
look_brief(Agent):- must(prop(Agent,needs_look,true)),look_as(Agent).

telnet_repl_writer(_TL,call,term,Goal):-!,ignore(debugOnError(Goal)).
telnet_repl_writer( TL,text,Type,[V]):-telnet_repl_writer(TL,text,Type,V).
telnet_repl_writer( TL,text,Type,V):- is_list(V),merge_elements(V,L),V\=@=L,!,telnet_repl_writer( TL,text,Type,L).
telnet_repl_writer(_TL,text,Type,V):-copy_term(Type,TypeO),ignore(TypeO=t),fmt('text(~q).~n',[V]).
telnet_repl_writer(_TL,N,Type,V):-copy_term(Type,TypeO),ignore(TypeO=t),fmt('~q=(~w)~q.~n',[N,TypeO,V]).

telnet_repl_obj_to_string(O,_TypeHint,O):-!.
telnet_repl_obj_to_string(O,_TypeHint,S):- must(object_string(O,S)),!.
telnet_repl_obj_to_string(O,Type,toString(TypeO,O)):-copy_term(Type,TypeO),ignore(TypeO=s).

merge_elements(V,V):-not(is_list((V))),!.
merge_elements([],[]):-!.
merge_elements([E],[E]):-!.
merge_elements(V,V).
% merge_elements(V,M):-list_to_set(V,[E|More]),maplist(simply_

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
	moo:label_type(Obj,0),
	write(Obj), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[dark]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	write('dk '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[Head]|Tail], Return, Column) :-
	Ctemp is Column + 1,
	moo:label_type(Map,Head),
	write(Map), write(' '),
	write_pretty_aux(Tail, Return, Ctemp).
write_pretty_aux([[Agent]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	isa(Agent,agent),
	write('Ag'), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[_|_]|Tail],Return,Column) :-
	Ntemp is Column + 1,
	write('A+'), write(' '),
	write_pretty_aux(Tail,Return,Ntemp).



show_room_grid(Room) :- ignore(show_room_grid_new(Room)),!.
% show_room_grid(Room) :-show_room_grid_old(Room),!.

% ===================================================================
% show_room_grid_new(Room)
% ===================================================================
:-export(show_room_grid_new/1).
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

door_label(R,Dir,'  '):- pathBetween_call(R,Dir,SP),atomic(SP).

asserted_atloc(O,L):-is_asserted(atloc(O,L)).

show_room_grid_single(Room, xyz(Room,X,Y,Z),OutsideTest):- call(OutsideTest), doorLocation(Room,X,Y,Z,Dir),door_label(Room,Dir,Label),write(Label),!.
show_room_grid_single(_Room,_LOC,OutsideTest):- call(OutsideTest),!,write('[]'),!.
show_room_grid_single(_Room,LOC,_OutsideTest):- asserted_atloc(Obj,LOC),inst_label(Obj,Label), write(Label), !.
show_room_grid_single(_Room,LOC,_OutsideTest):- asserted_atloc(_Obj,LOC),write('..'), !.
show_room_grid_single(_Room,_LOC,_OutsideTest):- write('--'), !.

inst_label(Obj,SLabe2):-term_to_atom(Obj,SLabel),sub_atom(SLabel,1,2,_,SLabe2),!.
inst_label(Obj,SLabe2):-term_to_atom(Obj,SLabel),sub_atom(SLabel,0,2,_,SLabe2),!.
inst_label(Obj,Label):- moo:label_type(Label,Obj),!.
inst_label(Obj,Label):-  iprops(Obj,nameStrings(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(Obj,Label):-  iprops(Obj,named(Val)),Val\=Obj,!,inst_label(Val,Label),!.
inst_label(Obj,Label):-  iprops(Obj,isa(Val)),Val\=Obj,inst_label(Val,Label),!.
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
	asserted_atloc(Obj,LOC),
        props(Obj,isa(agent)),
	list_agents(Agents),
	obj_memb(Agent,Agents),
	asserted_atloc(Agent,LOC),
	write('Region1+'), write(' '),
	XX is X + 1,
	!,
	show_room_grid(Room,Y,XX,N).
show_room_grid(Room,Y,X,N) :-
        loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Obj,LOC),
        prop(Obj,isa,Class),
	moo:label_type(Label,Class),
	write(Label), write(' '),
	XX is X + 1,
	!,
	show_room_grid(Room,Y,XX,N).
show_room_grid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Agent,LOC),
	isa(Agent,agent),
	write('Ag'), write(' '),
	XX is X + 1,
	!,
	show_room_grid(Room,Y,XX,N).


% Used to display the labels of the grid locations. (the key to the map).
% Used at end of run.
display_grid_labels :-
	findall([Label,Name],moo:label_type(Label,Name),List),
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
	catch(tcp_host_to_address(Host, Peer),_,Host = Peer),
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



:- include(logicmoo(vworld/moo_footer)).

