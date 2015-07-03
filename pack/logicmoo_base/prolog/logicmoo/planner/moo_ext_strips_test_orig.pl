
:- style_check(-discontiguous).
/****************************************************************************/
/****************************************************************************/
/****************************************************************************/
% Database blocks
% Database
% Database blocks

/* stack(X,Y) */
preclist(stack(X,Y), [holding(X),clear(Y)]).
dellist(stack(X,Y), [holding(X),clear(Y)]).
addlist(stack(X,Y), [handempty,on(X,Y),clear(X)]).

/* pickup(X) */
preclist(pickup(X),  [ontable(X), clear(X), handempty]).
dellist(pickup(X),  [ontable(X),clear(X),handempty]).
addlist(pickup(X),  [holding(X)]).

/* unstack(X,Y) */
preclist(unstack(X,Y), [on(X,Y), clear(X), handempty]).
dellist(unstack(X,Y), [handempty,clear(Y),on(X,Y)]).
addlist(unstack(X,Y), [holding(X),clear(Y)]).

/* putdown(X) */
preclist(putdown(X), [holding(X)]).
dellist(putdown(X), [holding(X)]).
addlist(putdown(X), [ontable(X),handempty,clear(X)]).

prec( Action, Cond ) :- preclist( Action, Plist ), member( Cond, Plist ).
adds( Action, Cond ) :- addlist( Action, Alist), member( Cond, Alist ). 
dels( Action, Cond ) :- dellist( Action, Dlist), member( Cond, Dlist ). 


inconsistent(holding(X), on(X,_)).
inconsistent(holding(X), on(_,X)).
inconsistent(holding(X), ontable(X)).
inconsistent(holding(_), handempty).
inconsistent(holding(X), holding(Y)) :- not(X=Y).
inconsistent(on(X,Y), on(X,Z)) :- not(Z=Y).
inconsistent(on(Y,X), on(Z,X)) :- not(Z=Y).
inconsistent(clear(X), on(_,X)).
inconsistent(clear(X), holding(X)).
inconsistent(ontable(X), on(X,_)).




% Initilize state
init_state1([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal1([on(a,b),on(b,c)]).

init_state2([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal2([on(b,c),on(a,b) ]).

init_state3([clear(b),clear(c),ontable(a),ontable(b),on(c,a),handempty]).
goal3([on(a,b),on(b,c)]).

% Imposible goal
init_state4([clear(a),clear(b),ontable(a),ontable(b),handempty]).
goal4([on(a,b),on(b,a)]).

init_state5([clear(c),ontable(a),on(b,a),on(c,b),handempty]).
goal5([on(a,b),on(b,c)]).

init_state6([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal6([holding(a)]).


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
prec(stack(X,Y), [holding(X),clear(Y)]).
dels(stack(X,Y), [holding(X),clear(Y)]).
adds(stack(X,Y), [handempty,on(X,Y),clear(X)]).

/* pickup(X) */
prec(pickup(X),  [ontable(X), clear(X), handempty]).
dels(pickup(X),  [ontable(X),clear(X),handempty]).
adds(pickup(X),  [holding(X)]).

/* unstack(X,Y) */
prec(unstack(X,Y), [on(X,Y), clear(X), handempty]).
dels(unstack(X,Y), [handempty,clear(Y),on(X,Y)]).
adds(unstack(X,Y), [holding(X),clear(Y)]).

/* putdown(X) */
prec(putdown(X), [holding(X)]).
dels(putdown(X), [holding(X)]).
adds(putdown(X), [ontable(X),handempty,clear(X)]).


inconsistent(holding(X), on(X,_)).
inconsistent(holding(X), on(_,X)).
inconsistent(holding(X), ontable(X)).
inconsistent(holding(_), handempty).
inconsistent(holding(X), holding(Y)) :- not(X=Y).
inconsistent(on(X,Y), on(X,Z)) :- not(Z=Y).
inconsistent(on(Y,X), on(Z,X)) :- not(Z=Y).
inconsistent(clear(X), on(_,X)).
inconsistent(clear(X), holding(X)).
inconsistent(ontable(X), on(X,_)).




/*--------------------------------------
                        |
                        X
                       / \

    +-----+
    |  c  |
    +-----+      +-----+
    |  a  |      |  b  |
----+-----+------+-----+----------------
*/


/* -------------------------------------
                        |
                        X
                       / \



    +-----+      +-----+     +-----+
    |  a  |      |  b  |     |  c  |
----+-----+------+-----+-----+-----+------
*/
% Initilize state
init_state1a([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal1a([on(a,b),on(b,c) ]).

init_state2a([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal2a([on(b,c),on(a,b) ]).

init_state3a([clear(b),clear(c),ontable(a),ontable(b),on(c,a),handempty]).
goal3a([on(a,b),on(b,c)]).

init_state4a([clear(a),on(a,b),on(b,c),ontable(c),handempty]).
goal4a([on(a,b),on(b,a)]).

init_state5a([clear(c),ontable(a),on(b,a),on(c,b),handempty]).
goal5a([on(a,b),on(b,c)]).

init_state6a([clear(c),ontable(a),on(c,a),ontable(b), clear(b),handempty]).
goal6a([on(a,b),on(b,c)]).

init_state7a([clear(a),clear(b),clear(c),ontable(a),ontable(b),ontable(c),handempty]).
goal7a([on(b,c),on(c,a),on(a,b) ]).

init_state8a([clear(b),on(b,a), on(a,d), on(d,c), ontable(c),handempty]).
goal8a([on(a,b),ontable(b), on(c,d),ontable(d)]).

init_state9a([clear(a),on(a, b), on(b,d), on(d,c), ontable(c),handempty]).
goal9a([on(a,b),ontable(b), on(c,d),ontable(d)]).

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

