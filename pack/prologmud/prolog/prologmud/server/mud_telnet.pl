/** <module>  
% Initial Telnet/Text console 
% ALL telnet client business logic is here (removed from everywhere else!)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:-module(mud_telnet, [                  
                  telnet_server/2,
                  setup_streams/2,
                  set_tty_control/1,
                  player_connect_menu/4,
                  look_brief/1,
                  cmdShowRoomGrid/1,
                  inst_label/2,
                  display_grid_labels/0,
                  telnet_repl_writer/4,
                  telnet_repl_obj_to_string/3,
                  start_mud_telnet/1,
                  run_session/0,
                  run_session/2,
                  login_and_run/0,
                  login_and_run/2,
                  session_loop/2,
                  get_session_io/2,
                  kill_naughty_threads/0,
                  set_player_telnet_options/1,
                  register_player_stream_local/3,
                  login_and_run_nodebug/0]).


% learnLaterWhenToCallProceedure(What):- ... code ...

%:-add(learnLaterWhenToCallProceedure(kill_naughty_threads)).

%:-add(unimpledTODO(learnLaterWhenToCallProceedure)).
%:-add(unimpledTODO(codeWithTODONextToIt)).

% instanceRecognizedBy(codeWithTODONextToIt,grovelSourceCodeLookingForComment).



kill_naughty_threads:-forall(thread_property(_,alias(ID)),sanify_thread(ID)).
% ignore main thread
sanify_thread(main):-!.
sanify_thread(ID):- ( \+ atom_concat('httpd',_,ID)),!,
   ignore(( thread_statistics(ID,local,Size),MSize is 200 * 1024, Size>MSize, dmsg(big_thread(ID,local,Size)))).
sanify_thread(ID):- 
   ignore(( thread_statistics(ID,local,Size),MSize is 200 * 1024, Size>MSize, 
     % thread_signal(ID,abort) maybe
     dmsg(killing_big_thread(ID,local,Size)), thread_exit(ID) )).


:- meta_predicate toploop_telnet:show_room_grid_single(*,*,0).

:- include(prologmud(mud_header)).

:- disable_mpreds_in_current_file.

% :- register_module_type (utility).

:-  use_module(library(threadutil)).

% ===========================================================
% TELNET REPL + READER
% ===========================================================
start_mud_telnet_4000:-start_mud_telnet(4000).
start_mud_telnet(Port):- 
  must(telnet_server(Port, [allow(_ALL),call_pred(login_and_run_nodebug)])),!.

:- volatile(main_thread_error_stream/1).
:- dynamic(main_thread_error_stream/1).

save_error_stream:-  ignore((thread_self(main),(quintus:current_stream(2, write, Err),asserta(main_thread_error_stream(Err))))).
:- initialization(save_error_stream).

get_main_thread_error_stream(user_error):-!.
get_main_thread_error_stream(ES):-main_thread_error_stream(ES),!.
get_main_thread_error_stream(user_error).

service_client_call(Call, Slave, In, Out, Host, Peer, Options):-
   thread_self(Id),
   get_main_thread_error_stream(Err),
   'format'(Err,'~n~n~q~n~n',[service_client_call(Call, Id, Slave, In, Out, Host, Peer, Options)]),
   call(Call).

get_session_io(In,Out):-
   current_input(In),
   current_output(Out),
   setup_streams(In, Out),
   set_tty_control(true).

login_and_run_nodebug:- nodebugx(login_and_run).


player_connect_menu(In,Out,Wants,P):-
 must_det_l((
   get_session_id(O),
   get_session_io(In,Out),
   fmt('~N~nHello ~q!~n',[O]),
   setup_streams(In, Out),set_tty_control(true),
   foc_current_agent(Wants),
   foc_current_agent(P),
   assert_isa(P,tHumanPlayer),
   register_player_stream_local(P,In,Out),
   fmt('~N~nWelcome to the MUD ~w!~n',[P]),
   colormsg([blink,fg(red)],"this is blinking red!"))),!.


login_and_run:-
   get_session_io(In,Out),!,
   login_and_run(In,Out).

login_and_run(In,Out):-
  player_connect_menu(In,Out,_,_),!,
  run_session(In,Out).

set_player_telnet_options(P):-
     add(repl_writer(P,telnet_repl_writer)),
     add(repl_to_string(P,telnet_repl_obj_to_string)).

goodbye_player:- 
     foc_current_agent(P3),
     deliver_event(P3,goodBye(P3)).

run_session:-
   get_session_io(In,Out),
   run_session(In,Out).

run_session(In,Out):-  
  must_det_l((get_session_id(O),
  get_session_io(In,Out),
  asserta(t_l:telnet_prefix([isSelfAgent,wants,to])),
  retractall(t_l:wants_logout(O)))),!,
  repeat,     
         once(session_loop(In,Out)),
      retract(t_l:wants_logout(O)),!,
      thread_self(Id),
      retractall(thglobal:session_io(_,_,_,Id)),      
      retractall(thglobal:session_io(O,_,_,_)),!.

session_loop(In,Out):-
  get_session_id(O),
  (current_agent(P)->true;player_connect_menu(In,Out,_,_);player_connect_menu(In,Out,_,P)),
  start_agent_action_thread,
  ignore(look_brief(P)),!,
  (t_l:telnet_prefix(Prefix)->(sformat(S,'~w ~w>',[P,Prefix]));sformat(S,'~w> ',[P])),
  prompt_read_telnet(In,Out,S,List),!,
  register_player_stream_local(P,In,Out),
  enqueue_session_action(P,List,O).


:-export(register_player_stream_local/3).
register_player_stream_local(P,In,Out):-
   set_player_telnet_options(P),
   get_session_id(O),thread_self(Id),
   retractall(thglobal:session_io(_,_,_,Id)),
   retractall(thglobal:session_io(O,_,_,_)),
   asserta_new(thglobal:session_io(O,In,Out,Id)),
   asserta_new(thglobal:session_agent(O,P)),
   asserta_new(thglobal:agent_session(P,O)), 
    (thread_self(main)->get_main_thread_error_stream(Err); Err=Out),
     (thread_util:has_console(Id,In, Out,Err)->true;
       ((retractall(thread_util:has_console(Id,_,_,_)),
          asserta(thread_util:has_console(Id,In,Out,Err))))).


:-export(enqueue_session_action/3).

%enqueue_session_action(A,[+, Text],S):- string(Text), must(assert_text(tWorld,Text)).
%enqueue_session_action(A,[W0,W1|WL],S):- string(Text),!,enqueue_session_action(A,[actSay,[W0,W1|WL]],S).
enqueue_session_action(A,L,S):- show_call(enqueue_agent_action(A,L,S)).


set_tty_control(TF):- 
  ignore((logOnFailure((
   set_prolog_flag(color_term,TF),
   set_stream(user_output, tty(TF)),
   set_stream(user_error, tty(TF)),
   set_stream(user_input, tty(TF)),
   set_prolog_flag(tty_control, TF))))),!.

user:deliver_event_hooks(A,Event):-subst(Event,reciever,you,NewEventM),subst(NewEventM,A,you,NewEvent),
      foreach(no_repeats(get_agent_sessions(A,O)),
         foreach(no_repeats(thglobal:session_io(O,In,Out,Id)),
          fmtevent(Out,NewEvent))).

fmtevent(Out,NewEvent):-string(NewEvent),!,format(Out,'~s',[NewEvent]).
fmtevent(Out,NewEvent):-format(Out,'~N~q.~n',[NewEvent]).

:-thread_local(t_l:telnet_prefix/1).

:-set_tty_control(true).

:-export(prompt_read/4).
prompt_read_telnet(In,Out,Prompt,Atom):-
      get_session_id(O),      
      prompt_read(In,Out,Prompt,IAtom),
      (IAtom==end_of_file -> (hooked_asserta(t_l:wants_logout(O)),Atom='quit') ; IAtom=Atom),!.

prompt_read(In,Out,Prompt,Atom):-         
        with_output_to(Out,ansi_format([reset,hfg(white),bold],'~w',[Prompt])),flush_output(Out),      
        get_session_io(In,Out),
        repeat,read_code_list_or_next_command_with_prefix(In,Atom),!.

local_to_words_list(Atom,Words):-var(Atom),!,Words = Atom.
local_to_words_list(end_of_file,end_of_file):-!.
local_to_words_list([],[]):-!.
local_to_words_list(Atom,Words):-to_word_list(Atom,Words),!.

maybe_prepend_prefix(Words,Words).

read_code_list_or_next_command_with_prefix(In,Words):- read_code_list_or_next_command(In,Atom),show_call(local_to_words_list(Atom,WordsM)),!,maybe_prepend_prefix(WordsM,Words).

read_code_list_or_next_command(Atom):-current_input(In),read_code_list_or_next_command(In,Atom),!.

read_code_list_or_next_command(In,end_of_file):- at_end_of_stream(In),!.
read_code_list_or_next_command(In,Atom):- 
 (var(In)->current_input(In);true), catch(wait_for_input([In], Ready, 1),_,fail),!,  member(In,Ready),
  read_pending_input(In,CodesL,[]),!,is_list(CodesL),CodesL\==[],
   ((last(CodesL,EOL),member(EOL,[10,13])) -> code_list_to_next_command(CodesL,Atom); 
    (read_line_to_codes(In,CodesR), (is_list(CodesR)-> (append(CodesL,CodesR,NewCodes),code_list_to_next_command(NewCodes,Atom)); Atom=CodesR))),!.

read_code_list_or_next_command(In,Atom):- 
  read_pending_input(In,CodesL,[]),is_list(CodesL),CodesL\==[],
   ((last(CodesL,EOL),member(EOL,[10,13])) -> code_list_to_next_command(CodesL,Atom); 
    (read_line_to_codes(In,CodesR), (is_list(CodesR)-> (append(CodesL,CodesR,NewCodes),code_list_to_next_command(NewCodes,Atom)); Atom=CodesR))),!.

code_list_to_next_command(end_of_file,end_of_file).
code_list_to_next_command(NewCodes,Atom):-append(Left,[EOL],NewCodes),EOL<33,!,code_list_to_next_command(Left,Atom).
code_list_to_next_command( [EOL|NewCodes],Atom):-EOL<33,!,code_list_to_next_command(NewCodes,Atom).
code_list_to_next_command( [],actLook).
code_list_to_next_command( [91|REST],TERM):- failOnError((atom_codes(A,[91|REST]),atom_to_term(A,TERM,[]))),!.
code_list_to_next_command(NewCodes,Atom):-atom_codes(Atom,NewCodes),!.

:-export(scan_src_updates/0).

tick_tock:-
           scan_src_updates,!,fmt('tick tock',[]),sleep(0.1),!.

scan_src_updates:- !.
scan_src_updates:- ignore((thread_self(main),ignore((catch(make,E,dmsg(E)))))).


% ===========================================================
% DEFAULT TELNET "LOOK"
% ===========================================================

telnet_repl_writer(_TL,call,ftTerm,Goal):-!,ignore(on_x_debug(Goal)).
telnet_repl_writer( TL,text,Type,[V]):-telnet_repl_writer(TL,text,Type,V).
telnet_repl_writer( TL,text,Type,V):- is_list(V),merge_elements(V,L),V\=@=L,!,telnet_repl_writer( TL,text,Type,L).
telnet_repl_writer(_TL,text,Type,V):-copy_term(Type,TypeO),ignore(TypeO=t),fmt('text(~q).~n',[V]).
telnet_repl_writer(_TL,N,Type,V):-copy_term(Type,TypeO),ignore(TypeO=t),fmt('~q=(~w)~q.~n',[N,TypeO,V]).

telnet_repl_obj_to_string(O,_TypeHint,O):-!.
telnet_repl_obj_to_string(O,_TypeHint,S):- must(object_string(O,S)),!.
telnet_repl_obj_to_string(O,Type,toString(TypeO,O)):-copy_term(Type,TypeO),ignore(TypeO=s).


% ===========================================================
% DEFAULT TEXT
% ===========================================================
look_brief(Agent):- prop(Agent,mudLastCommand,X),nonvar(X),functor(X,actLook,_),!.
look_brief(Agent):- not(prop(Agent,mudNeedsLook,vTrue)),!.
look_brief(Agent):- must(prop(Agent,mudNeedsLook,vTrue)),look_as(Agent),!.

merge_elements(V,V):-not(is_list((V))),!.
merge_elements([],[]):-!.
merge_elements([E],[E]):-!.
merge_elements(V,V).
% merge_elements(V,M):-list_to_set(V,[E|More]),maplist(simply_ )..

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
	typeHasGlyph(Obj,0),
	write(Obj), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[vDark]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	write('dk '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[Head]|Tail], Return, Column) :-
	Ctemp is Column + 1,
	typeHasGlyph(Map,Head),
	write(Map), write(' '),
	write_pretty_aux(Tail, Return, Ctemp).
write_pretty_aux([[Agent]|Tail],Return,Column) :-
	Ctemp is Column + 1,
	isa(Agent,tAgent),
	write('Ag'), write(' '),
	write_pretty_aux(Tail,Return,Ctemp).
write_pretty_aux([[_|_]|Tail],Return,Column) :-
	Ntemp is Column + 1,
	write('A+'), write(' '),
	write_pretty_aux(Tail,Return,Ntemp).




cmdShowRoomGrid(Room) :- ignore(show_room_grid_new(Room)),!.
% cmdShowRoomGrid(Room) :-show_room_grid_old(Room),!.

% ===================================================================
% show_room_grid_new(Room)
% ===================================================================
:-export(show_room_grid_new/1).
show_room_grid_new(Room):-
   grid_size(Room,Xs,Ys,_Zs),
   Ys1 is Ys+1,Xs1 is Xs+1,
   forall(between(0,Ys1,Y),
   ((nl, 
   forall(between(0,Xs1,X),
   ((loc_to_xy(Room,X,Y,LOC),
   write(' '),
   OutsideTest = (not(between(1,Xs,X));not(between(1,Ys,Y))),
   once(show_room_grid_single(Room,LOC,OutsideTest)))))))),!,nl.
show_room_grid_new(_):-nl.

door_label(R,Dir,'  '):- pathBetween_call(R,Dir,SP),atomic(SP).

asserted_atloc_for_map(O,L):-asserted_atloc(O,L),O\=apathFn(_,_).
asserted_atloc(O,L):-is_asserted(mudAtLoc(O,L)).

show_room_grid_single(Room, xyzFn(Room,X,Y,Z),OutsideTest):- call(OutsideTest), doorLocation(Room,X,Y,Z,Dir),door_label(Room,Dir,Label),write(Label),!.
show_room_grid_single(_Room,_LOC,OutsideTest):- call(OutsideTest),!,write('[]'),!.
show_room_grid_single(_Room,LOC,_OutsideTest):- asserted_atloc_for_map(Obj,LOC),inst_label(Obj,Label), write(Label), !.
show_room_grid_single(_Room,LOC,_OutsideTest):- asserted_atloc_for_map(_Obj,LOC),write('..'), !.
show_room_grid_single(_Room,_LOC,_OutsideTest):- write('--'), !.

atom_label(SLabel,SLab2):- atom_concat('NPC0',L,SLabel),!,atom_label(L,SLab2),!.
atom_label(SLabel,SLab2):- atom_concat('NPC',L,SLabel),!,atom_label(L,SLab2),!.
atom_label(SLabel,SLab2):- once(i_name(SLabel,L)),L\=SLabel,atom_label(L,SLab2),!.
%atom_label(SLabel,SLab2):- sub_atom(SLabel,2,2,_,SLab2),!.
atom_label(SLabel,SLab2):- sub_atom(SLabel,1,2,_,SLab2),!.
atom_label(SLabel,SLab2):- sub_atom(SLabel,0,2,_,SLab2),!.

inst_label(Obj,Label):-  typeHasGlyph(Obj,Label),!.
inst_label(Obj,SLab2):-  atom(Obj),atom_label(Obj,SLab2).
inst_label(Obj,SLab2):-  term_to_atom(Obj,SLabel),atom_label(SLabel,SLab2).
inst_label(Obj,Label):-  iprops(Obj,nameStrings(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(Obj,Label):-  iprops(Obj,mudNamed(Val)),Val\=Obj,!,inst_label(Val,Label),!.
inst_label(Obj,Label):-  iprops(Obj,isa(Val)),Val\=Obj,inst_label(Val,Label),!.
inst_label(_Obj,'&&').

% ===================================================================
% show_room_grid_old(Room)
% ===================================================================
% Display world
show_room_grid_old(Room) :-  
	gridValue(Room,1,G,_),
	length(G,N),
	M is N + 1,
	cmdShowRoomGrid(Room,1,1,M),!.

cmdShowRoomGrid(Room,Old,N,N) :-
	New is Old + 1,
	\+ gridValue(Room,New,N,_),
	nl,
	!.

cmdShowRoomGrid(Room,Old,N,N) :-
	New is Old + 1,
	nl,
	!,
	cmdShowRoomGrid(Room,New,1,N).
cmdShowRoomGrid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Obj,LOC),
        props(Obj,isa(tAgent)),
	list_agents(Agents),
	obj_memb(Agent,Agents),
	asserted_atloc(Agent,LOC),
	write('Region1+'), write(' '),
	XX is X + 1,
	!,
	cmdShowRoomGrid(Room,Y,XX,N).
cmdShowRoomGrid(Room,Y,X,N) :-
        loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Obj,LOC),
        prop(Obj,isa,Class),
	typeHasGlyph(Label,Class),
	write(Label), write(' '),
	XX is X + 1,
	!,
	cmdShowRoomGrid(Room,Y,XX,N).
cmdShowRoomGrid(Room,Y,X,N) :-
      loc_to_xy(Room,X,Y,LOC),
	asserted_atloc(Agent,LOC),
	isa(Agent,tAgent),
	write('Ag'), write(' '),
	XX is X + 1,
	!,
	cmdShowRoomGrid(Room,Y,XX,N).


% Used to display the labels of the grid locations. (the key to the map).
% Used at end of run.
display_grid_labels :-
	findall([Label,Name],typeHasGlyph(Name,Label),List),
	forall(prop_memb([Label,Name],List),
	           (write(Label), write('='), write(Name), write(' '))),
		   nl.





:- source_location(S,_),forall(source_file(H,S),ignore((  \+ (predicate_property(H,PP),member(PP,[(multifile),built_in])),  
 functor(H,F,A),module_transparent(F/A),export(F/A)))).
  
:- include(prologmud(mud_footer)).



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
        % tcp_setopt(ServerSocket, nodelay),
        % tcp_setopt(ServerSocket, dispatch(false)),        
	must((tcp_bind(ServerSocket, Port),
	tcp_listen(ServerSocket, 5))),
	thread_create(server_loop(ServerSocket, Options), _,
		      [ alias(telnet_server)
		      ]).


make_client_alias(Host,AliasH):- compound(Host),Host=..HostL, must(atomic_list_concat(['client@'| HostL],'-', AliasH)),!.
make_client_alias(Host,AliasH):- is_list(Host),must(atomic_list_concat(['client@'| Host], ' ', AliasH)),!.
make_client_alias(Host,AliasH):- term_to_atom(Host,AHost),must(atomic_list_concat(['client@', AHost], ' ', AliasH)).



server_loop(ServerSocket, Options) :-
	tcp_accept(ServerSocket, Slave, Peer),
	tcp_open_socket(Slave, In, Out),
	set_stream(In, close_on_abort(false)),
	set_stream(Out, close_on_abort(false)),
	catch(tcp_host_to_address(Host, Peer),_,Host = Peer),
	/*(   Postfix = []
	;   between(2, 1000, Num),
	    Postfix = [-, Num]
	),*/
	make_client_alias(Host,AliasH),
        gensym(AliasH,Alias),
	catch(thread_create(
		  service_client(Slave, In, Out, Host, Peer, Options),
		  ThreadID,
		  [ 
                      alias(Alias)
                     % detached(true)
		  ]),
	      error(permission_error(actCreate, thread, Alias/ThreadID), _),
	      fail), !,
	server_loop(ServerSocket, Options).


call_close_and_detatch(In, Out, Id, Call):-         
               setup_streams(In, Out),
                 call_cleanup(call(Call),
		     ( close_connection(In, Out),
		       ignore(thread_detach(Id))
                       )).

 

close_connection(In, Out) :-
        retractall(thread_util:has_console(_,In,Out,_)),
        ignore(catch(close(In, [force(true)]),_,true)),
        ignore(catch(close(Out, [force(true)]),_,true)).

strm_info(Out,Name,Strm):-nl,write(Out,Name = Strm),forall(stream_property(Strm,P),'format'(Out,', ~q',[P])),nl(Out).

setup_streams(In, Out):-
      Err=Out,
      set_prolog_IO(In, Out, Err),
      set_stream(In, close_on_abort(false)),
      set_stream(Out, close_on_abort(false)),!.

setup_streams_pt2(In, Out):-
      set_stream(In,  alias(user_input)),
      set_stream(Out, alias(user_output)),
      set_stream(Err, alias(user_error)),
      set_stream(In,  alias(current_input)),
      set_stream(Out, alias(current_output)),!.

setup_streams_pt3(In, Out):-
      thread_self(Id),
     % retractall(thread_util:has_console(Id, _, _, _)),
      assert(thread_util:has_console(Id, In, Out, Err)),
     % current_prolog_flag(encoding, Enc),
     % set_stream_ice(In, user_input, encoding(Enc)),
     % set_stream_ice(Out, user_output, encoding(Enc)),
     % set_stream_ice(Err, user_error, encoding(Enc)),
      set_stream_ice(In, user_input, newline(detect)),
      set_stream_ice(Out, user_output, newline(dos)),
      set_stream_ice(Err, user_error, newline(dos)),!.
  
set_stream_ice(Stream, Alias, NV):- catch(set_stream(Alias,NV),_,catch(set_stream(Stream,NV),E,nop(dmsg(E)))).

service_client(Slave, In, Out, Host, Peer, Options) :-
   allow(Peer, Options), !,
   call_pred(Call, Options), !,
   setup_streams(In, Out),
   format(user_error,'% Welcome ~q to the SWI-Prolog LogicMOO server on thread ~w~n~n', [Peer,service_client(Slave, In, Out, Host, Peer, call(Call,Options))]),
   call_close_and_detatch(In, Out, Id, service_client_call(Call, Slave, In, Out, Host, Peer, Options)).

service_client(_Slave, In, Out, Host, Peer, _Options):- 
   thread_self(Id),
    call_close_and_detatch(In, Out, Id,
       'format'(Out, 'Go away ~q!!~n', [Host:Peer])).

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


