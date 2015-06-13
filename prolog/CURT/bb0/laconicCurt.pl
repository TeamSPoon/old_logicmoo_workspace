/*************************************************************************

         name: laconicCurt.pl (Volume 1, Chapter 6)
      version: April 17, 2001
  description: Curt (including equivalent reading checking)
      authors: Patrick Blackburn & Johan Bos
 
*************************************************************************/

:- module(curt,[curt/0]).

:- ensure_loaded(comsemOperators).

:- use_module(callInference,[callTheoremProver/2,
			     callModelBuilder/3]).

:- use_module(readLine,[readLine/1]).

:- use_module(comsemPredicates,[memberList/2,
				selectFromList/3,
				printRepresentations/1]).

:- use_module(kellerStorage,[kellerStorage/2]).


/*========================================================================
   Start Curt
========================================================================*/

curt:- 
   curtTalk([],[],run).


/*========================================================================
   Control
========================================================================*/

curtTalk(_,_,quit).

curtTalk(OldReadings,OldModels,run):-
   readLine(Input),
   curtUpdate(Input,OldReadings-NewReadings,OldModels-NewModels,Moves,State), !,
   curtOutput(Moves),
   curtTalk(NewReadings,NewModels,State).

	
/*========================================================================
   Curt Help
========================================================================*/

curtHelp:-
	nl, write('bye: no more talking'),
	nl, write('readings: prints current readings'),
	nl, write('models: prints current models'),
	nl, write('new: starts a new discourse'),
	nl.


/*========================================================================
   Curt's output
========================================================================*/

curtOutput([]).
curtOutput([Move|Moves]):-
	realizeMove(Move,Output),
	format('~n~nCurt: ~p~n',[Output]),
	curtOutput(Moves).


/*========================================================================
   Curt's Moves
========================================================================*/

realizeMove(clarify,'Want to tell me something?').

realizeMove(bye,'Bye bye!').

realizeMove(accept,'OK.').

realizeMove(contradiction,'No! I do not believe that!').

realizeMove(obvious,'Well, that is obvious!').

realizeMove(noparse,'What?').


/*========================================================================
   Update Curt's Information State
========================================================================*/

curtUpdate([],R-R,M-M,[clarify],run):- !.

curtUpdate([bye],R-R,M-M,[bye],quit):- !.

curtUpdate([new],_-[],_-[],[],run):- !.

curtUpdate([help],R-R,M-M,[],run):- !,
	curtHelp.

curtUpdate([readings],Readings-Readings,M-M,[],run):- !,
	printRepresentations(Readings).

curtUpdate([models],R-R,Models-Models,[],run):- !,
	printRepresentations(Models).

curtUpdate(Input,OldReadings-NewReadings,_-Models,Moves,run):-
	kellerStorage(Input,Readings), !,
	\+ Readings = [],
	findall(
		Formula,
		(
		    memberList(New,Readings),
 		    (
		     memberList(Old,OldReadings),
		     Formula=(New&Old)
		    ;
		     OldReadings=[],
		     Formula=New
		    )
		),
	        UpdatedReadings
	       ),
	eliminateEquivalentReadings(UpdatedReadings,UniqueReadings),
        consistentReadings(UniqueReadings-NewReadings,Models,TempMoves),
        informativeReadings(NewReadings,TempMoves,Moves).

curtUpdate(_,R-R,M-M,[noparse],run).


/*========================================================================
   Select Consistent Readings
========================================================================*/

consistentReadings(UpdatedReadings-ConsistentReadings,Models,[Move]):-
	findall((Reading,Model),
		(
		    memberList(Reading,UpdatedReadings),
		    consistent(Reading,Model)
		),
		Readings),
	(
	    Readings=[],
	    Move=contradiction
	;
	    \+ Readings=[],
	    Move=accept
	),
	findall(R,memberList((R,_),Readings),ConsistentReadings),
	findall(M,memberList((_,M),Readings),Models).


/*========================================================================
   Consistency Checking calling Theorem Prover and Model Builder
========================================================================*/

consistent(Formula,Model):-
	callTheoremProver(~Formula,Proof),
	(
	    Proof=proof, !,
	    fail
	;
	    DomainSize=20,
	    callModelBuilder(Formula,DomainSize,Model),
            Model = model(_,_)
	).


/*========================================================================
   Select Informative Readings
========================================================================*/

informativeReadings(Readings,OldMoves,NewMoves):-
	\+ memberList((_&_),Readings), !,
	NewMoves=OldMoves.
	
informativeReadings(Readings,OldMoves,NewMoves):-
	findall((New&Old),
		(
		    memberList((New&Old),Readings),
		    informative(Old,New)
		),
		InformativeReadings),
	(
	    InformativeReadings=[],
	    NewMoves=[obvious]
	;
	    \+ InformativeReadings=[],
	    NewMoves=OldMoves
	).


/*========================================================================
   Informativity Checking calling Theorem Prover and Model Builder
========================================================================*/

informative(Old,New):-
	callTheoremProver(~(Old & ~New),Proof),
	(
	    Proof=proof, !,
	    fail
	;
	    DomainSize=20,
	    callModelBuilder(Old & ~New,DomainSize,Model),
            Model=model(_,_)
	).


/*========================================================================
   Eliminate Equivalent Readings
========================================================================*/

eliminateEquivalentReadings(Readings,UniqueReadings):-
	numberReadings(Readings,1,N,NumberedReadings),
        format('~nThere are ~p readings:~n',[N]),
        printRepresentations(Readings),
	eliminateEquivalentReadings(NumberedReadings,[],UniqueReadings).


/*========================================================================
   Number the readings
========================================================================*/

numberReadings([],N1,N2,[]):- 
        N2 is N1 - 1.

numberReadings([X|L1],N1,N3,[n(N1,X)|L2]):-
	N2 is N1 + 1,
	numberReadings(L1,N2,N3,L2).


/*========================================================================
   Check equivalence by calling a theorem prover
========================================================================*/

eliminateEquivalentReadings(NumberedReadings,Different,UniqueReadings):-
	selectFromList(n(N1,R1),NumberedReadings,Readings),
	memberList(n(N2,R2),Readings),
	\+ memberList(different(N1,N2),Different), !,
	callTheoremProver(((R1 > R2) & (R2 > R1)),Proof),
	(
	    Proof=proof, !,
	    format('~nReadings ~p and ~p are equivalent.~n',[N1,N2]),
	    eliminateEquivalentReadings(Readings,Different,UniqueReadings)
	;
	    eliminateEquivalentReadings([n(N1,R1)|Readings],[different(N1,N2),different(N2,N1)|Different],UniqueReadings)
	).

eliminateEquivalentReadings(NumberedReadings,_,UniqueReadings):-
	findall(Reading,memberList(n(_,Reading),NumberedReadings),UniqueReadings).
