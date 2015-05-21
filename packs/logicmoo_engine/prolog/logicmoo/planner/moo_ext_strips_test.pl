

:- style_check(-discontiguous).
:- style_check(-singleton).
/****************************************************************************/
/****************************************************************************/
/****************************************************************************/
% Database blocks
% Database
% Database blocks

/* stack(X,Y) */
preclist(stack(X,Y), [mudPossess(Agnt,X),clear(Y)]).
dellist(stack(X,Y), [mudPossess(Agnt,X),clear(Y)]).
addlist(stack(X,Y), [handempty(Agnt),localityOfObject(X,Y),clear(X)]).

/* actTake(X) */
preclist(actTake(X),  [localityOfObject(X,iTable7), clear(X), handempty(Agnt)]).
dellist(actTake(X),  [localityOfObject(X,iTable7),clear(X),handempty(Agnt)]).
addlist(actTake(X),  [mudPossess(Agnt,X)]).

/* unstack(X,Y) */
preclist(unstack(X,Y), [localityOfObject(X,Y), clear(X), handempty(Agnt)]).
dellist(unstack(X,Y), [handempty(Agnt),clear(Y),localityOfObject(X,Y)]).
addlist(unstack(X,Y), [mudPossess(Agnt,X),clear(Y)]).

/* putdown(X) */
preclist(putdown(X), [mudPossess(Agnt,X)]).
dellist(putdown(X), [mudPossess(Agnt,X)]).
addlist(putdown(X), [localityOfObject(X,iTable7),handempty(Agnt),clear(X)]).

prec( Action, Cond ) :- preclist( Action, Plist ), member( Cond, Plist ).
adds( Action, Cond ) :- addlist( Action, Alist), member( Cond, Alist ). 
dels( Action, Cond ) :- dellist( Action, Dlist), member( Cond, Dlist ). 


inconsistent(mudPossess(Agnt,X), localityOfObject(X,_)).
inconsistent(mudPossess(Agnt,X), localityOfObject(_,X)).
inconsistent(mudPossess(Agnt,X), localityOfObject(X,iTable7)).
inconsistent(mudPossess(Agnt,_), handempty(Agnt)).
inconsistent(mudPossess(Agnt,X), mudPossess(Agnt,Y)) :- not(X=Y).
inconsistent(localityOfObject(X,Y), localityOfObject(X,Z)) :- not(Z=Y).
inconsistent(localityOfObject(Y,X), localityOfObject(Z,X)) :- not(Z=Y).
inconsistent(clear(X), localityOfObject(_,X)).
inconsistent(clear(X), mudPossess(Agnt,X)).
inconsistent(localityOfObject(X,iTable7), localityOfObject(X,_)).




% Initilize state
init_state1([clear(iA7),clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal1([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7)]).

init_state2([clear(iA7),clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal2([localityOfObject(iB7,iC7),localityOfObject(iA7,iB7) ]).

init_state3([clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iA7),handempty(Agnt)]).
goal3([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7)]).

% Imposible goal
init_state4([clear(iA7),clear(iB7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),handempty(Agnt)]).
goal4([localityOfObject(iA7,iB7),localityOfObject(iB7,iA7)]).

init_state5([clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iA7),localityOfObject(iC7,iB7),handempty(Agnt)]).
goal5([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7)]).

init_state6([clear(iA7),clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal6([mudPossess(Agnt,iA7)]).


test1 :-
	init_state1(InitState),
	goal1(Goal),
	do_it(Goal, InitState).
test2 :-
	init_state2(InitState),
	goal2(Goal),
	do_it(Goal, InitState).
test3 :-
	init_state3(InitState),
	goal3(Goal),
	do_it(Goal, InitState).

% TODO - does not recognize impossibility
test4:-
	init_state4(InitState),
	goal4(Goal),
	do_it(Goal, InitState).
test5 :-
	init_state5(InitState),
	goal5(Goal),
	do_it(Goal, InitState).

test6 :-
	init_state6(InitState),
	goal6(Goal),
	do_it(Goal, InitState).

do_it(Goal, InitState) :-
	strips3(Goal,InitState,Plan),
	write('from: '),write(InitState),nl,
	write('to: '),write(Goal),nl,
	write('do  -->   '),nl,
      	reverse(Plan,RPlan),
        	writeplan(RPlan),nl.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Database
% Database blocks

/* stack(X,Y) */
prec(stack(X,Y), [mudPossess(Agnt,X),clear(Y)]).
dels(stack(X,Y), [mudPossess(Agnt,X),clear(Y)]).
adds(stack(X,Y), [handempty(Agnt),localityOfObject(X,Y),clear(X)]).

/* actTake(X) */
prec(actTake(X),  [localityOfObject(X,iTable7), clear(X), handempty(Agnt)]).
dels(actTake(X),  [localityOfObject(X,iTable7),clear(X),handempty(Agnt)]).
adds(actTake(X),  [mudPossess(Agnt,X)]).

/* unstack(X,Y) */
prec(unstack(X,Y), [localityOfObject(X,Y), clear(X), handempty(Agnt)]).
dels(unstack(X,Y), [handempty(Agnt),clear(Y),localityOfObject(X,Y)]).
adds(unstack(X,Y), [mudPossess(Agnt,X),clear(Y)]).

/* putdown(X) */
prec(putdown(X), [mudPossess(Agnt,X)]).
dels(putdown(X), [mudPossess(Agnt,X)]).
adds(putdown(X), [localityOfObject(X,iTable7),handempty(Agnt),clear(X)]).


inconsistent(mudPossess(Agnt,X), localityOfObject(X,_)).
inconsistent(mudPossess(Agnt,X), localityOfObject(_,X)).
inconsistent(mudPossess(Agnt,X), localityOfObject(X,iTable7)).
inconsistent(mudPossess(Agnt,_), handempty(Agnt)).
inconsistent(mudPossess(Agnt,X), mudPossess(Agnt,Y)) :- not(X=Y).
inconsistent(localityOfObject(X,Y), localityOfObject(X,Z)) :- not(Z=Y).
inconsistent(localityOfObject(Y,X), localityOfObject(Z,X)) :- not(Z=Y).
inconsistent(clear(X), localityOfObject(_,X)).
inconsistent(clear(X), mudPossess(Agnt,X)).
inconsistent(localityOfObject(X,iTable7), localityOfObject(X,_)).




/*--------------------------------------
                        |
                        X
                       / \

    +-----+
    |  iC7  |
    +-----+      +-----+
    |  iA7  |      |  iB7  |
----+-----+------+-----+----------------
*/


/* -------------------------------------
                        |
                        X
                       / \



    +-----+      +-----+     +-----+
    |  iA7  |      |  iB7  |     |  iC7  |
----+-----+------+-----+-----+-----+------

tMisserbal(X) <= mudHappy(X),X<50.


  mudHappy(X),X<50

(action ( doHappyThing ?X)
 (hint 
   ( mudHappy(X),X<50 ))
 (postConds 
  (not( tMisserbal ?X)))
 )

*/
% Initilize state
init_state1a([clear(iA7),clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal1a([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7) ]).

init_state2a([clear(iA7),clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal2a([localityOfObject(iB7,iC7),localityOfObject(iA7,iB7) ]).

init_state3a([clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iA7),handempty(Agnt)]).
goal3a([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7)]).

init_state4a([clear(iA7),localityOfObject(iA7,iB7),localityOfObject(iB7,iC7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal4a([localityOfObject(iA7,iB7),localityOfObject(iB7,iA7)]).

init_state5a([clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iA7),localityOfObject(iC7,iB7),handempty(Agnt)]).
goal5a([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7)]).

init_state6a([clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iC7,iA7),localityOfObject(iB7,iTable7), clear(iB7),handempty(Agnt)]).
goal6a([localityOfObject(iA7,iB7),localityOfObject(iB7,iC7)]).

init_state7a([clear(iA7),clear(iB7),clear(iC7),localityOfObject(iA7,iTable7),localityOfObject(iB7,iTable7),localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal7a([localityOfObject(iB7,iC7),localityOfObject(iC7,iA7),localityOfObject(iA7,iB7) ]).

init_state8a([clear(iB7),localityOfObject(iB7,iA7), localityOfObject(iA7,iD7), localityOfObject(iD7,iC7), localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal8a([localityOfObject(iA7,iB7),localityOfObject(iB7,iTable7), localityOfObject(iC7,iD7),localityOfObject(iD7,iTable7)]).

init_state9a([clear(iA7),localityOfObject(iA7, iB7), localityOfObject(iB7,iD7), localityOfObject(iD7,iC7), localityOfObject(iC7,iTable7),handempty(Agnt)]).
goal9a([localityOfObject(iA7,iB7),localityOfObject(iB7,iTable7), localityOfObject(iC7,iD7),localityOfObject(iD7,iTable7)]).

test1a :-
	init_state1a(InitState),
	goal1a(Goal),
	do_it(Goal, InitState).
test2a :-
	init_state2a(InitState),
	goal2a(Goal),
	do_it(Goal, InitState).
test3a :-
	init_state3a(InitState),
	goal3a(Goal),
	do_it(Goal, InitState).
test4a:-
	init_state4a(InitState),
	goal4a(Goal),
	do_it(Goal, InitState).
test5a :-
	init_state5a(InitState),
	goal5a(Goal),
	do_it(Goal, InitState).

test6a :-
	init_state6a(InitState),
	goal6a(Goal),
	do_it(Goal, InitState).

test7a :-
	init_state7a(InitState),
	goal7a(Goal),
	do_it(Goal, InitState).
test8a :-
	init_state8a(InitState),
	goal8a(Goal),
	do_it(Goal, InitState).
test9a :-
	init_state9a(InitState),
	goal9a(Goal),
	do_it(Goal, InitState).

do_it(Goal, InitState) :-
	rstrips(InitState, Goal, Plan),
	write('   from: '),write(InitState),nl,
	write('   goal: '),write(Goal),nl,
	write('     -->   '),nl,
        	writeplan(Plan),nl.

