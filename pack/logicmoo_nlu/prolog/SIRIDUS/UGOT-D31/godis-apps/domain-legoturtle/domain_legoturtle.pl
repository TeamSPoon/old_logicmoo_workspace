 
/*************************************************************************

         name: domain_legoturtle.pl 
  description: LEGOTURTLE domain file
 
*************************************************************************/

 :- module( domain_legoturtle, [ plan/2,
 			 issue/2,
 			 sort_restr/1,
 			 isa/2,
 			 postcond/2,
 			 depends/2
 			  ] ).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- ensure_loaded( semsort_legoturtle ).


/*----------------------------------------------------------------------
     Dialogue plans
     I dialogplanerna ligger de fr�gor systemet beh�ver ha svar p� f�r
     kunna hantera den fr�ga man st�llt.

     plan( ?ActionOrIssue, ?Plan )
----------------------------------------------------------------------*/


default_question(dummy).

% Dialogplanerna. I findout ligger fr�gorna systemet beh�ver st�lla.
% I dev_do ligger det kommando som LEGOTURTLE ger till Tcl/Tk.


plan( top,
      [ forget_all,
	raise( X^action(X) ),
	findout( set([%action(pen_up),
		      %action(pen_down),
		      action(move),
		      action(turn)
		      ] ) ) ] ).


postcond( top, none ).

plan( up, [] ). 

plan( pen_up,
      [ dev_do( turtle, pu ) ] ).
postcond( pen_up, done(pu) ).

plan( pen_down,
     [dev_do( turtle, pd ) ] ).
postcond( pen_down, done(pd) ).

plan( move,
       [findout( set( [ action(forward),
 		       action(backward) ] ) ) ] ).
postcond(move, done(forward)).
postcond(move, done(backward)).

plan( turn,
       [findout( set( [ action(right),
 		       action(left) ] ) ) ] ).
postcond(turn, done(right)).
postcond(turn, done(left)).


plan( background,
      [ findout( X^color(X) ),
	dev_do( turtle, setbg ) ] ).
postcond( background, done(setbg) ).

plan( pencolor,
      [ findout( X^color(X) ),
	dev_do( turtle, setpc ) ] ).
postcond( pencolor, done(setpc) ).

plan( clear,
      [ dev_do( turtle, cs ) ] ).
postcond( clear, done(cs) ).

plan( circle,
      [ dev_do( turtle, [repeat, 360, '{', fd, 50, bk, 50, lt, 1, '}' ] ) ] ).
postcond( circle, done( [repeat, 360, '{', fd, 50, bk, 50, lt, 1, '}' ] ) ).

plan( tree,
      [ dev_do( turtle, [ pu, bk, 100, pd, rtree, '[', expr, '[', random, 50, ']', '+', 25, ']' ] ) ] ).
postcond( tree, done( [ pu, bk, 100, pd, rtree, '[', expr, '[', random, 50, ']', '+', 25, ']' ] ) ).

plan( backward,
      [ findout( X^steps(X) ),
      dev_do( turtle, bk ) ] ).
postcond( backward, done(bk) ).

plan( forward,
      [ findout( X^steps(X) ),
      dev_do( turtle, fd ) ] ).
postcond( forward, done(fd) ).


plan( left,
      [ findout( X^degrees(X) ),
	dev_do( turtle, lt ) ] ).
postcond( left, done(lt) ).

plan( right,
      [ findout( X^degrees(X) ),
	dev_do( turtle, rt ) ] ).
postcond( right, done(rt) ).


/*--------------------------------------------------------------
     Conceptual knowledge
     V�ljer vilket spr�k man ska anv�nda, samt vilken
     dom�n man vill k�ra p�.
----------------------------------------------------------------------*/
sort_restr( language( X ) ) :-
	sem_sort( X, language ).

sort_restr( domain( X ) ) :-
	sem_sort( X, domain ).


% sortal restrictions; determine relevance and resolvement
% G�r att man kan ge alla info p� en g�ng om man vet att
% systemet kommer att st�lla en f�ljdfr�ga. Man kan direkt
% ange antal steg, antal grader eller vilken f�rg man vill
% byta till.

sort_restr( steps( X ) ) :- sem_sort( X, number ).
sort_restr( degrees( X ) ) :- sem_sort( X, number ).
sort_restr( color( X ) ) :- sem_sort( X, colors ).


% Negation
sort_restr( \+ P ) :- sort_restr( P ).

% action
sort_restr( action( X ) ) :- sem_sort( X, action ).
sort_restr( action( respond(Q) ) ) :- sort_restr( issue(Q) ).

% issue
sort_restr( issue(Q) ):-
	plan( Q, _ ),
	\+ sort_restr( action( Q ) ).

sort_restr( issue(Q) ):-
	plan( _, Plan ),
	member( findout(Q), Plan ),
	\+ sort_restr( action( Q ) ).

% metaissue
sort_restr( und(_DP*P) ):- sort_restr(P).


% parameter validity; determines acceptance

valid_parameter( steps(N) ):- sem_sort( N, steps ).

valid_parameter( degrees(N) ):- sem_sort( N, degrees ).

valid_parameter( color(C) ):- sem_sort( C, color ).


	
depends(bla,bla).
