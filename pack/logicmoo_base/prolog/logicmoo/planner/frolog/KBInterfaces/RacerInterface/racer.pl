
/*************************************************************************

    File: racer.pl
    Copyright (C) 2007 

    Programmer: Luciana Benotti

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(racer,[load_scenario/1,    %(+Scenario)
		 load_scenario/5,   %(+Scenario,-Link,-TBoxName,-GameAboxName,-PlayerAboxName) 
	     worldKB/2,         %(-Link,-GameAboxName)
		 playerKB/2,         %(-Link,-PlayerAboxName)
         individual_direct_types/4,   %(+Link,+ABox,+Individual,-Concepts)   
         individual_types/4,   %(+Link,+ABox,+Individual,-Concepts)
		 concept_instances/4,   %(+Link,+ABox,+Concept,-Individuals)
		 individual_fillers/5,   %(+Link,+ABox,+Individual,+Role,-Individuals) 
		 individual_fillers_inv/5,   %(+Link,+ABox,+Individual,+Role,-Individuals) 
		 all_individuals/3,   %(+Link,+ABox,-Individuals) 
		 all_role_assertions_for_individual_in_domain/4,   %(+Link,+ABox,+Individual,-Assertions) 
		 all_role_assertions_for_individual_in_range/4,   %(+Link,+ABox,+Individual,-Assertions)
		 all_role_assertions/3,   %(+Link,+ABox,-Assertions)
		 individual/3, 			 %(+Link,+ABox,+Individual)
		 individual_instance/4,   %(+Link,+ABox,+Individual,+Concept)
		 individuals_related/5,   %(+Link,+ABox,+Individual1,+Individual2,+Role)
		 concept_subsumes/3,   %(+Link,+Concept1,+Concept2)
		 clone_abox/3,   %(+Link,+ABox,-ABoxClone)
		 delete_abox/2,    %(+Link,+ABox)
		 add_concept_assertion/4,    %(+Link,+ABox,+Individual,+Concept)
		 forget_concept_assertion/4,    %(+Link,+ABox,+Individual,+Concept) 
		 add_role_assertion/5,     %(+Link,+ABox,+Individual1,+Individual2,+Role) 
		 forget_role_assertion/5,    %(+Link,+ABox,+Individual1,+Individual2,+Role) 
		 concept_children/3, 	%(+Link,+Concept,-Children)
		 assert_playerKB/1,		%(+Assertion)
		 retract_playerKB/1,		%(+Assertion)
		 log/1,
		 individual_fillers_playerKB/3,	%(Individual,Role,Individuals)
		 all_individuals_playerKB/1,		%(Individuals)
		 concept_instances_playerKB/2,		%(Concept,Individuals) 
		 individual_types_playerKB/2,			%(Individual,Concepts)
		 individual_types_worldKB/2,			%(Individual,Concepts)
		 all_individuals_worldKB/1,		%(Individuals)
		 concept_instances_worldKB/2,		%(Concept,Individuals) 
		 individual_fillers_worldKB/3	%(Individual,Role,Individuals)
		]).  

:- dynamic
        worldKB/2,
		playerKB/2, 
		log/1.
		

/*************************************************************************

    Function: This module implements Frolog's interface with a
    Description Logics (DL) theorem prover.

    Technical: The theorem prover used is RacerPro, version 1.9.0. The
    module was implemented and tested in a Linux platform, OS Ubuntu
    Feisty Fawn, version 7.04. The conection with the prover uses the
    protocol TCP.

    Wishlist: The DL community is currently working in an standard for
    communication between applications and DL theorem prover. The
    standard is called DIG interface and works over the HTTP
    protocol. The current version of the DIG interface, version 1.1,
    does not support all the reasoning services needed by Frolog (such
    as retracts from the KBs) and then this interface is implemented
    using TCP. When DIG standarizes the reasoning services needed by
    Frolog this module will evolve to be independent of a particular
    reasoner.
    
    May be it would be wiser to stop using this prolog interface and 
    just use JRacer (Racer's API for Java)


*************************************************************************/


/*========================================================================
   Predicates to manage the socket connection with Racer
   The connection is identified by the structure link(InStream, OutStream) 
   of type Link

   The library(socket) library provides TCP and UDP inet-domain sockets
   from SWI-Prolog, both client and server-side communication.

   open_link(+Host, +Port, -Link) 
     Connects to RacerPro by a socket TCP, running in the host +Host and 
     listening in the port +Port. open_link(-Link) by default connects to 
     RacerPro running on the same host as the client and listening in the 
     default port for TCP: 8088
     tcp_open_socket(+SocketId, -InStream, -OutStream) Opens two SWI-Prolog 
     I/O-streams, one to deal with input from the socket and one with output 
     to the socket.
 
   query_link(+Link, +Format, +Arguments, -Answer)
     Writes in the OutStream the text generated combining Format and the 
     Arguments. Flushes the Outstream to send the text to Racer and reads
     Racer's answer from the Instream (From). The readLine predicate returns 
     a list of words from which the empty words are deleted. The answer is 
     analysed to check if it suceeded. The clean answer is returned in Answer.  

   close_link(+Link)
     After closing both InStream and OutSream, the socket itself is discarded.  
========================================================================*/
 
:- use_module(library(socket)). 
:- use_module(readLine,[readLine/2]).
:- use_module('Tools/handleExceptions',[handle/3]).  

open_link(Host, Port, link(From,To), Socket) :-
    sleep(1),
	format('. ~n'),  
    catch(tcp_connect(Socket, Host:Port), _, open_link(Host, Port, link(From,To),Socket)), 
    % If tcp_connect/2 tries to connect to Racer and Racer is not up yet then the error 
    % PrologException: error(socket_error('Connection refused'), _0) is displayed
    % this predicate sleeps for one second and then tries again
	tcp_open_socket(Socket, From, To). 

open_link(Link) :- 
    tcp_socket(Socket),
    format('Connecting to Racer at ~w:~w ~n', [localhost, 8088]),
    open_link(localhost, 8088, Link, Socket).
    
open_log :-
	open('GameScenarios/FairyTaleCastle/racerlog', write, Log),
	assert(log(Log)).

query_link(link(From,To), Query, Arguments, Answer) :- 
	log(Log),
	format(Log, Query, Arguments),
	format(To, Query, Arguments),
	flush_output(To),
	once(readLine(From, WordList)), % once: backtracking is not allowed for readLine 
	                                % we don't want to ask racer the same question twice  
	subtract(WordList, ['',nil], WordListClean), 
	good_answer(WordListClean,Answer),
	format(Log,'Answer: ~w ~n',[Answer]). 

good_answer(['answer',_|List],List):- !.
good_answer(['ok',_|List],List):- !.
good_answer(_Answer,_) :- fail.

close_link(link(From, To)) :-
	close(From),
	close(To).

/*========================================================================
   Predicates to load a game scenario. 
  
   load_scenario(+Scenario,-Link,-TBoxName,-GameAboxName,-PlayerAboxName)
     Opens the link to Racer and loads all the files of the specified 
     Scenario. That is the TBox and the player and game initial ABoxes using
     the predicates: 

   load_kbs(+Path,-Link,-TBoxName,-GameAboxName,-PlayerAboxName)

========================================================================*/

load_scenario(Scenario) :- 
	retractall(worldKB(_,_)), 
	retractall(playerKB(_,_)),
	load_scenario(Scenario,_,TBoxName,GameAboxName,PlayerAboxName),
	format('The Name of the TBox is: ~w ~n',TBoxName), 
	format('The Name of the Game ABox is: ~w ~n',GameAboxName), 
	format('The Name of the Player ABox is: ~w ~n',PlayerAboxName).
	
load_scenario(Scenario, Link, TBox, GameABox, PlayerABox) :- 
	open_log,
	default_path(Scenario, Path),
	load_kbs(Path, Link, TBox, GameABox, PlayerABox), 
	assert(worldKB(Link,GameABox)),
	assert(playerKB(Link,PlayerABox)).

default_path(f, 'GameScenarios/FairyTaleCastle'). 
default_path(s, 'GameScenarios/SpaceStation').

load_kbs(Path, Link, TBox, GameABox, PlayerABox) :- 
	open_link(Link), 
	load_kb(Path, 'game-tbox.lisp', Link, TBox),
	load_kb(Path, 'game-init-abox.lisp', Link, GameABox),
    load_kb(Path, 'player-init-abox.lisp', Link, PlayerABox). 


/*========================================================================
   Functions for Knowledge Base Management

========================================================================*/

load_kb(Path, File, Link, KBName) :-
	working_directory(CWD,CWD),
	query_link(Link, '(racer-read-file "~w~w/~w")~n', [CWD,Path,File], [_,KBName|_]). 
	

/*========================================================================
   Predicates for TBox retrieval

========================================================================*/

concept_children(Link,Concept,Children) :-
	query_link(Link,'(concept-children ~w)~n',[Concept], Children).


/*========================================================================
   Predicates for ABox retrieval

========================================================================*/

individual_direct_types(Link,ABox,Individual,Concepts) :-
	query_link(Link,'(individual-direct-types ~w ~w)~n',[Individual,ABox], Concepts). 

individual_types_playerKB(Individual,Concepts) :-
	playerKB(Link,PlayerABox),
	query_link(Link,'(individual-types ~w ~w)~n',[Individual,PlayerABox], Concepts). 
	
individual_types_worldKB(Individual,Concepts) :-
	worldKB(Link,WorldABox),
	query_link(Link,'(individual-types ~w ~w)~n',[Individual,WorldABox], Concepts). 	

individual_types(Link,ABox,Individual,Concepts) :-
	query_link(Link,'(individual-types ~w ~w)~n',[Individual,ABox], Concepts). 

concept_instances_playerKB(Concept,Individuals) :-
	playerKB(Link,PlayerABox),
	query_link(Link,'(concept-instances ~w ~w)~n',[Concept,PlayerABox], Individuals). 
	
concept_instances_worldKB(Concept,Individuals) :-
	worldKB(Link,WorldABox),
	query_link(Link,'(concept-instances ~w ~w)~n',[Concept,WorldABox], Individuals).

concept_instances(Link,ABox,Concept,Individuals) :-
	query_link(Link,'(concept-instances ~w ~w)~n',[Concept,ABox], Individuals). 

individual_fillers_playerKB(Individual,Role,Individuals) :-
	playerKB(Link,PlayerABox),
	query_link(Link,'(individual-fillers ~w ~w ~w)~n',[Individual,Role,PlayerABox], Individuals). 
	
individual_fillers_worldKB(Individual,Role,Individuals) :-
	worldKB(Link,WorldABox),
	query_link(Link,'(individual-fillers ~w ~w ~w)~n',[Individual,Role,WorldABox], Individuals). 

individual_fillers(Link,ABox,Individual,Role,Individuals) :-
	query_link(Link,'(individual-fillers ~w ~w ~w)~n',[Individual,Role,ABox], Individuals). 

individual_fillers_inv(Link,ABox,Individual,Role,Individuals) :-
	query_link(Link,'(individual-fillers ~w (inv ~w) ~w)~n',[Individual,Role,ABox], Individuals). 

all_individuals_playerKB(Individuals) :-
	playerKB(Link,PlayerABox),
	query_link(Link,'(all-individuals ~w)~n',[PlayerABox], Individuals).
	
all_individuals_worldKB(Individuals) :-
	worldKB(Link,WorldABox),
	query_link(Link,'(all-individuals ~w)~n',[WorldABox], Individuals).

all_individuals(Link,ABox,Individuals) :-
	query_link(Link,'(all-individuals ~w)~n',[ABox], Individuals).

all_role_assertions_for_individual_in_domain(Link,ABox,Individual,Assertions) :-
	query_link(Link,'(all-role-assertions-for-individual-in-domain ~w ~w)~n',[Individual,ABox], WordList),
	format_role_assertions(WordList, Assertions).

all_role_assertions_for_individual_in_range(Link,ABox,Individual,Assertions) :-
	query_link(Link,'(all-role-assertions-for-individual-in-range ~w ~w)~n',[Individual,ABox], WordList),
	format_role_assertions(WordList, Assertions).

all_role_assertions(Link,ABox,Assertions) :-
	query_link(Link,'(all-role-assertions ~w)~n',[ABox], WordList), 
	format_role_assertions(WordList, Assertions).


%Predicates for formatting the roles
format_role_assertions([],[]).
format_role_assertions([I1,I2,R|Rest1],[Assertion|Rest2]) :-
	Assertion =.. [R,I1,I2],
	format_role_assertions(Rest1,Rest2).


/*========================================================================
   Functions for KB evaluation

========================================================================*/

individual(Link,ABox,Individual) :-
	query_link(Link,'(individual? ~w ~w)~n',[Individual,ABox], Answer), 
	Answer = [t],!.

individual_instance(Link,ABox,Individual,Concept) :-
	query_link(Link,'(individual-instance? ~w ~w ~w)~n',[Individual,Concept,ABox], Answer), 
	Answer = [t],!.

individuals_related(Link,ABox,Individual1,Individual2,Role) :-
	query_link(Link,'(individuals-related? ~w ~w ~w ~w)~n',[Individual1,Individual2,Role,ABox], Answer), 
	Answer = [t],!.

concept_subsumes(Link,Concept1,Concept2) :-
	query_link(Link,'(concept-subsumes? ~w ~w)~n',[Concept1,Concept2], Answer), 
	Answer = [t],!.


/*========================================================================
   Predicates for ABox management

========================================================================*/

clone_abox(Link,ABox,ABoxClone) :-
	query_link(Link, '(clone-abox ~w)~n', [ABox],[ABoxClone]).

delete_abox(Link,ABox) :-
	query_link(Link, '(delete-abox ~w)~n', [ABox],_).

add_concept_assertion(Link,ABox,Individual,Concept) :-
	query_link(Link, '(add-concept-assertion ~w ~w ~w)~n', [ABox,Individual,Concept],_).

forget_concept_assertion(Link,ABox,Individual,Concept) :-
	query_link(Link, '(forget-concept-assertion ~w ~w ~w)~n', [ABox,Individual,Concept],_).

add_role_assertion(Link,ABox,Individual1,Individual2,Role) :-
	query_link(Link, '(add-role-assertion ~w ~w ~w ~w)~n', [ABox,Individual1,Individual2,Role],_).

forget_role_assertion(Link,ABox,Individual1,Individual2,Role) :-
	query_link(Link, '(forget-role-assertion ~w ~w ~w ~w)~n', [ABox,Individual1,Individual2,Role],_).
 
	
/*========================================================================
   Predicates for playerKB management

========================================================================*/

assert_playerKB(Assertion) :-
	Assertion =.. [Concept,Individual],
	playerKB(Link,PlayerABox),
	add_concept_assertion(Link,PlayerABox,Individual,Concept),
	individual_instance(Link,PlayerABox,Individual,Concept).
	
assert_playerKB(Assertion) :-
	Assertion =.. [Role,Individual1,Individual2],
	playerKB(Link,PlayerABox),
	add_role_assertion(Link,PlayerABox,Individual1,Individual2,Role),
	individuals_related(Link,PlayerABox,Individual1,Individual2,Role).
	
retract_playerKB(Assertion) :-
	Assertion =.. [Concept,Individual],
	playerKB(Link,PlayerABox),
	forget_concept_assertion(Link,PlayerABox,Individual,Concept),
	not(individual_instance(Link,PlayerABox,Individual,Concept)).
	
retract_playerKB(Assertion) :-
	Assertion =.. [Role,Individual1,Individual2],
	playerKB(Link,PlayerABox),
	forget_role_assertion(Link,PlayerABox,Individual1,Individual2,Role),
	not(individuals_related(Link,PlayerABox,Individual1,Individual2,Role)).
	
	

