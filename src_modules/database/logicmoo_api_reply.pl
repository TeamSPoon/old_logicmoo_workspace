:-include('logicmoo_utils_header.pl').
% =================================================================================
% Writeback to clients.. initial form is assumed Bach       and gets translated to the clients negatioted protocall
% =================================================================================
sayIfFeedback(Channel,Agent,Comment):- fmt(Comment).
:-dynamic(wantsEvents/5).
:-dynamic(current_say_to/1).
/*
% fmt/1
fmt(Format):-current_say_to(Where),!,call(Where,Format),!.
fmt(Format):-atom(Format),default_channel(Channel),sayLine(Channel,Format),!.
fmt(Format):-default_channel(Channel),fmt(Channel,Format),!.

% fmt/2
fmt(Format,ARGS):-current_say_to(Where),!,call(Where,Format,ARGS),!.
fmt(Format,[A|RGS]):-atom(Format),catch(sformat(String,Format,[A|RGS]),_,fail),!,fmt(String).

% fmt/3
fmt(Channel,Agent,Event):-current_say_to(Where),!,call(Where,Channel,Agent,Event),!.
fmt(Channel,Agent,Event):-wantsEvents(Channel,Agent,In,Out,'TELNETDX'),!,
   toEnglish(Channel,Agent,Event,English),fmt(Out,'~w~n> ',[English]),flush_output(Out).
fmt(Channel,Agent,Bach):-wantsEvents(Channel,Agent,In,Out,'Packed'),!,
   catch(bachToPackedChars(Bach,Chars),_,fail),
   wantsEvents(Channel,Agent,In,Out,Transport),!,
   fmt(user_error,'~n~q ~s~n~n',[Channel,Agent,Chars]),flush_output(user_error),
   catch((fmt(Out,'~s',[Chars]),flush_output(Out)),_,killClient(CC)).
*/

toEnglish(Channel,Agent,map(Event),English):-member(parents:list([OP]),Event),
      (sameString(OP,"info");sameString(OP,"sight");sameString(OP,"sound")),
      member(args:RealE,Event),!,toEnglish(Channel,Agent,RealE,English).     
toEnglish(Channel,Agent,Event,English):-
      flag(indent,_,0),
      toMarkupFormula(bach,Event,[],English).
toEnglish(Channel,Agent,English,English).

/*
fmt(Channel,Format):-expandFormat(Format,GO),ignore(rememberSaidIt(GO)),!,sayLine(Channel,GO),!.

%expandFormat(Format,GO):-computeAnswer(5,Format,G,_),flatten([G],GO),!.
expandFormat(Format,Format).
%expandFormat(Format,GO):-toMarkup(5,Format,G,_),flatten([G],GO),!.

% fmt/3
fmt(Channel,Format,Args):-is_file_descriptor(Channel),!,!,catch(fmt(Channel,Format,Args),_,true).
fmt(Channel,Format,[A|RGS]):-atom(Format),catch(sformat(String,Format,[A|RGS]),_,fail),!,sayLine(Channel,String).
%fmt(Channel,Format,Args):-!,sformat(String,Format,Args),sayLine(Channel,String).
*/

is_file_descriptor(user_error).
is_file_descriptor(user_output).
is_file_descriptor(user_input).
is_file_descriptor(X):-is_stream(X),!.
is_file_descriptor('$stream'(_)).
is_file_descriptor('$socket'(_)).

sayLine(Channel,[]):-!,sayLine(Channel,'\n'),!.
sayLine(Channel,String):-is_string(String),!,string_to_atom(String,Atom),sayLine(Channel,Atom),!.
sayLine(Channel,Format):-is_string(Channel),!,string_to_atom(Channel,Atom),sayLine(Atom,Format),!.
sayLine(Channel,[T|O]):-!,toCycApiExpression_l([T|O],[],Format),!,sayLine(Channel,Format),!.
sayLine(Channel,Format):-is_file_descriptor(Channel),!,write(Channel,Format).
sayLine(Channel,Format):-is_stream(Channel),!,write(Channel,Format).
sayLine(Channel,Format):-!,%toMarkup(chat,Format,_,Out),
      eggdrop_say_to(Channel,Format),!.
sayLine(Channel,Format):-
	 catch((
	 tcp_socket(Socket),
         tcp_connect(Socket,'localhost':4447),
         tcp_open_socket(Socket,InStream,OutStream),
	 writeSTDERR('~q~n',[sayLine(Channel,Format)]),
	   sleep(0.2),
	   fmt(OutStream,'~w:~w~n~n',[Channel,Format]),
	   flush_output(OutStream),
	   sleep(0.2),
	   tcp_close_socket(Socket)
	   ),
	 E,writeSTDERR(E)),!.

:-dynamic(default_channel_or/1).
:-thread_local(default_channel_or/1).
default_channel(X):-default_channel_or(X),!.
default_channel(X):-current_output(X).
:-dynamic(default_user/1).
set_default_channel(Channel):-retractall(default_channel_or(_)),assert(default_channel_or(Channel)).
set_default_user(Agent):-retractall(default_user(_)),assert(default_user(Agent)).

:-dynamic(saved_note/4).
% sendNote(Agent,Channel,Subj,Data):-!.%fmt(sendNote(Agent,Channel,Subj,Data)).
% ================================================================
%   Transform Signals to Objects
% ================================================================

% ===================================================================
% writeObject(-Prolog)
%
% Replaces writeq in some cases
% ===================================================================
writeObject(Term):-!,writeObject(Term,Vars).
writeObject(Term,Vars):-toMarkup(html,Term,Vars,Chars),write(Chars).

% ================================================================
%   Serialize Objects to XML
% ================================================================
writeObject(quiet,Term,Vars):-!.

writeObject(Verbose,Term,Vars):-writeObject(Term,Vars).

		
writeObject(OBJ,Vars):- isMooOption(client=html),!,
		((toMarkup(html,OBJ,Vars,Chars),fmt(Chars))),!.
		
writeObject(OBJ,Vars):- isMooOption(client=atomata),!,
		((toMarkup(cycl,OBJ,Vars,Chars),fmt(Chars))),!.

writeObject(OBJ,Vars):- isMooOption(client=console),!,
		((toMarkup(cycl,OBJ,Vars,Chars),fmt(Chars))),!.

writeObject(OBJ,Vars):- !,
		((toMarkup(cycl,OBJ,Vars,Chars),fmt(Chars))),!.


writeObject_conj(A,Vars):-isSlot(A),!,
	writeObject(A,Vars).

writeObject_conj(and(A,true),Vars):-!,
	writeObject_conj(A,Vars).

writeObject_conj(and(true,A),Vars):-!,
	writeObject_conj(A,Vars).

writeObject_conj(and(A,B),Vars):-!,
	writeObject_conj(A,Vars),
	writeObject_conj('\n\n Also \n\n ',Vars),
	writeObject_conj(B,Vars).

writeObject_conj(Output,Vars):-
	%write(Output),nl.
	writeObject(Output,Vars).


