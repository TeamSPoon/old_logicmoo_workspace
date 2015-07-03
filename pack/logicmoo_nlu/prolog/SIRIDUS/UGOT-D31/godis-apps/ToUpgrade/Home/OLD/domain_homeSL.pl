 
:- module( domain_home, [ relevant_to_task/3,
			  relevant_to_tasks/3,
			  relevant_answer/2,
			  all_answers/2,
			  plan/2,
			  reduce/3,
			  revisable/1,
			  sysaction/1,
			  dominates/2,
			  abstract/3,
			  location/1,
			  device_type/1,
			  task/1,
			  voice/1
			] ).

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).

:- use_module( library(telia_house) ).


/*----------------------------------------------------------------------
     Dialogue plans

     plan( ?Name, ?Plan )
----------------------------------------------------------------------*/

default(dummy).

plan( top,
      [ raise(T^(task(T))),
	findout([task(perform_specific_action), task(make_query), task(wake_up), task(leaving), task(main_menu) ]),
	case([ ( task(perform_specific_action), exec(perform_specific_action) ),
	       ( task(make_query), exec(make_query) ),
	       ( task(wake_up), exec(wake_up) ),
	       ( task(leaving), exec(leaving) ),
	       ( task(main_menu), exec(main_menu) ),
	       [] ])
      ] ).

plan( main_menu,
      [ findout(main_menu),
	if_then_else(main_menu,
		     [ forget,
		       change_domain(menu) ],
		     [ forget(task(_)),
		       forget(not main_menu),
		       exec(top) ]
		    ) ] ).



plan( perform_specific_action,
      [ findout(A^action(A)),
	findout(L^location(L)),
	case([ ( action(dim_light), assume(device_type(dimmer)) ),
	       ( action(switch_on), assume(device_type(lamp)) ),
	       ( action(switch_off), assume(device_type(lamp)) ),
	       [] ]),
	findout(T^device_type(T)),
%	consultDB(D^device(D)),
	exec(A) ] ).

plan( switch_on,
      [ findout(L^location(L)),
	findout(T^device_type(T)),
	if_then_else( device_type(lamp), 
		      dev_set(home/L/D, onoff, on),
		      inform(bad_action(lamp))] ).

plan( switch_off,
      [ findout(L^location(L)),
	findout(T^device_type(T)),
	dev_set(home/L/D, onoff, off) ] ).

	if_then(device_type(D) and location(L),
		case([ ( device_type(lamp),
			   case([ ( action(switch_on), dev_set(home/L/D, onoff, on) ),
				  ( action(switch_off), dev_set(home/L/D/onoff, off ) ),
				  [ forget, inform(bad_action(lamp)) ] ]) ),
		       ( device_type(dimmer),
			   case([ ( action(dim_light), exec(dim_light) ),
				% more dimming options?
				  [ forget, inform(bad_action(dimmer)) ] ]) ),
		       forget ]),
		) ] ).


plan( perform_specific_action,
      [ findout(A^action(A)),
	findout(L^location(L)),
	case([ ( action(dim_light), assume(device_type(dimmer)) ),
	       ( action(switch_on), assume(device_type(lamp)) ),
	       ( action(switch_off), assume(device_type(lamp)) ),
	       [] ]),
	findout(T^device_type(T)),
%	consultDB(D^device(D)),
	if_then_else(device_type(D),
		     case([ ( device_type(lamp),
				case([ ( action(switch_on), exec(switch_on_lamp) ),
				       ( action(switch_off), exec(switch_off_lamp) ),
				       [ forget, inform(bad_action(lamp)) ] ]) ),
			    ( device_type(dimmer),
				case([ ( action(dim_light), exec(dim_light) ),
				% more dimming options?
				       [ forget, inform(bad_action(dimmer)) ] ]) ),
			    forget ]),
		     [ if_then(action(A), forget(action(A))),
		       consultDB(D^device(D)),
		       if_then_else(device(D),
				    inform(bad_action),
				    case([ ( device_type(lamp),
					       [ forget, inform(no_lamp_in(L)) ] ),
					   ( device_type(dimmer),
					       [ forget, inform(no_dimmer_in(L)) ] ),
					   forget ])),
		       exec(top) ])
      ] ).

plan( switch_on_lamp,
      [ if_then(device(D) and location(L),
		[ consult_dev(home/L/D,onoff),
		  dev_get(home/L/D, onoff),
		  if_then_else(dev_val(home/L/D,onoff,on),
			       [ forget,
				 inform(device_already_switched_on(D,L)) ],
			       [ dev_set(home/L/D,onoff,on),
				 forget,
				 inform(switched_on_lamp(L))
			       ])
		]),
	exec(top)
      ] ).

plan( switch_off_lamp,
      [ if_then(device(D),
	if_then(location(L),
		[ consult_dev(D,onoff),
		  if_then_else(dev_val(D,onoff,off),
			       [ forget,
				 inform(device_already_switched_off(D,L)) ],
			       [ update_dev(D,onoff,off),
				 if_then(location(L),
					 [ forget,
					   inform(switched_off_lamp(L)) ])
			       ]),
		  exec(top)
		])) ] ).

plan( dim_light,
      [ if_then(device(D),
	if_then(location(L),
		[ consult_dev(D,dimmer),
		  if_then_else(dev_val(D,dimmer,0.5),
			       [ forget,
				 inform(light_already_dimmed(D,L)) ],
			       [ update_dev(D,dimmer,0.5),
				 forget,
				 inform(dimmed_light(L)) ]),
		  exec(top)
		])) ] ).


plan( wake_up,
      [	consult_dev(kitchen/lamp,onoff),
	if_then(dev_val(kitchen/lamp,onoff,off),
		[ findout(light(kitchen)),
		  if_then(light(kitchen),update_dev(kitchen/lamp,onoff,on)) ]),
	update_dev(tv_room/lamp,onoff,on),
	update_dev(study/lamp,onoff,on),
	update_dev(hall/lamp,onoff,on),
	update_dev(living_room/dimmer,dimmer,0.5),
	consult_dev(kitchen/lamp,onoff),
	if_then_else(dev_val(kitchen/lamp,onoff,on),
		     [ forget, inform(woken_up_lit_kitchen) ],
		     [ forget, inform(woken_up_no_lit_kitchen) ]),
	exec(top) ] ).

plan( leaving,
      [ findout(leave_on_light),
	%if_then(location(_), assume(leave_on_light)),
	if_then_else(leave_on_light,
		     [ findout(X^(location(X))),
		       consultDB(D^(device(D))),
		       if_then_else(device(D),
				    exec(close_down),
				    if_then(location(L),
					    [ forget,
					      inform(no_lamp_in(L)),
					      exec(top) ])) ],
		     exec(close_down)) ] ).

plan( close_down,
      [ if_then_else(location(kitchen),
		     update_dev(kitchen/lamp,onoff,on),
		     update_dev(kitchen/lamp,onoff,off)),
	if_then_else(location(tv_room),
		     update_dev(tv_room/lamp,onoff,on),
		     update_dev(tv_room/lamp,onoff,off)),
	if_then_else(location(study),
		     update_dev(study/lamp,onoff,on),
		     update_dev(study/lamp,onoff,off)),
	if_then_else(location(hall),
		     update_dev(hall/lamp,onoff,on),
		     update_dev(hall/lamp,onoff,off)),
	update_dev(living_room^dimmer,dimmer,0.0),
	if_then_else(location(L),
		     [ forget, inform(closed_down_except(L)) ],
		     [ forget, inform(closed_down_all) ]),
	exec(top) ] ).

plan( make_query,
      [ if_then(device_type(T),
	if_then(location(L),
		[ consultDB(D^device(D)),
		  if_then_else(device(D), [], exec(no_device_found)) ])),
	findout(A^device_state(A)),
	findout(L^location(L)),
	case([ ( device_state(dimmed), assume(device_type(dimmer)) ),
	       ( device_state(switched_on), assume(device_type(lamp)) ),
	       ( device_state(switched_off), assume(device_type(lamp)) ),
	       [] ]),
	findout(T^device_type(T)),
	consultDB(D^device(D)),
	if_then_else(device(D),
		     % Device found
		     case([ ( device_type(lamp), exec(query_lamp) ),
			    ( device_type(dimmer), exec(query_dimmer) ),
			    forget ]),
		     % Device not found
		     exec(no_device_found))
      ] ).

plan( no_device_found,
      [ if_then(location(L),
		[ case([ ( device_type(lamp),
			     [ forget, inform(no_lamp_in(L)) ] ),
			 ( device_type(dimmer),
			     [ forget, inform(no_dimmer_in(L)) ] ),
			 forget ]),
		  exec(top) ] ) ] ).

plan( query_lamp,
      [ if_then(device(D),
        if_then(location(L),
	if_then(device_state(S),
		[ consult_dev(D,onoff),
		  case([ ( dev_val(D,onoff,on),
			     [ forget, inform(is_switched_onoff(on,S,L)) ] ),
			 ( dev_val(D,onoff,off),
			     [ forget, inform(is_switched_onoff(off,S,L)) ] ),
			 forget ]),
		  exec(top)
		] ))) ]).

plan( query_dimmer,
        [ if_then(device(D),
	  if_then(location(L),
		  [ consult_dev(D,dimmer),
		    case([ ( dev_val(D,dimmer,0.5),
			       [ forget, inform(is_dimmed(D,L)) ] ),
			   % other dimming options?
			   [ forget, inform(is_not_dimmed(D,L)) ] ]),
		    exec(top)
		  ] )) ]).
    
/*----------------------------------------------------------------------
     Conceptual knowledge
----------------------------------------------------------------------*/
     
task( perform_specific_action ).
task( perform_lamp_action ).
task( switch_on_lamp ).
task( switch_off_lamp ).
task( dim_light ).
task( make_query ).
task( wake_up ).
task( leaving ).
task( main_menu ).

action( switch_on ).
action( switch_off ).
action( dim_light ).

device_type( lamp ).
device_type( dimmer ).
device_type( temp_sensor ).
device_type( light_sensor ).

device_state( switched_on ).
device_state( switched_off ).
device_state( dimmed ).

location( L ) :-
	house( House ),
	member( L=_, House ).

% Voice

voice( 8 ).

/*----------------------------------------------------------------------
     relevant_answer( -Question, +Answer )
     -- Returns (if it succeeds) a Question to which
        the Answer is relevant 
----------------------------------------------------------------------*/

relevant_answer( X^(task(X)), task(A)) :-
	task( A ).
relevant_answer( X^(task(X)), A) :-
	task( A ).

relevant_answer( X^(action(X)), action(A)) :-
	action( A ).
relevant_answer( X^(action(X)), A) :-
	action( A ).

relevant_answer( X^(device_type(X)), device_type(A) ) :-
	device_type( A ).
relevant_answer( X^(device_type(X)), A ) :-
	device_type( A ).

relevant_answer( X^(location(X)), location(A) ) :-
	location( A ).
relevant_answer( X^(location(X)), A ) :-
	location( A ).

relevant_answer( X^(device_state(X)), device_state(A) ) :-
	device_state( A ).
relevant_answer( X^(device_state(X)), A ) :-
	device_state( A ).

%relevant_answer( X^(device_type(X)), task(leaving) ).

/***** GENERAL GODIS STUFF ******/


%%% Yes/no question P?

% yes
relevant_answer( Q, yes) :-
	ynq(Q).
% no
relevant_answer( Q, no) :-
	ynq(Q).
% P
relevant_answer( Q, P ) :-
	ynq(Q),
	P = Q.
% P
relevant_answer( Q, not(P) ) :-
	ynq(Q),
	P = Q.

% Alt-questions, full answer
relevant_answer( AltList, Alt ):-
	member( Alt, AltList ). 

%%% Definition of yes/no questions

ynq( YNQ ):-
	\+ YNQ = _^_,		% whq
	\+ YNQ = [_|_].		% altq


/*----------------------------------------------------------------------
     relevant_to_task( +Move, -Task, -Plan )
     -- Returns (if it succeeds) a Plan to which Move is relevant
----------------------------------------------------------------------*/


relevant_to_task( Move, Task, Plan ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ),
	%member( findout( Qs ), Plan ), % Alex
	%implied_question( Qs, Q ), % Alex
	relevant_answer( Q, A ).

% gives all elliptical answers
all_answers( Q, As ):-
	setof( A, relevant_elliptical_answer( Q, A ), As ).

relevant_elliptical_answer( Q, A ):-
	relevant_answer( Q, A ),
	atom( A ).

implied_question( FindOuts, Question ) :-
	FindOuts = [ _ | _ ],
	member( F, FindOuts ),
	implied_question( F, Question ).

implied_question( _^Q, Q ).
implied_question( Q, Q ).

/*----------------------------------------------------------------------
     relevant_to_tasks( +Move, -Tasks )
     -- Returns (if it succeeds) a list of tasks Tasks to which Move is
     relevant; all elements in Task have the form task(T)
----------------------------------------------------------------------*/

relevant_to_task( Move, Task ):-
	plan( Task, Plan ),
	Move = answer( A ),
	member( findout( Q ), Plan ),
	relevant_answer( Q, A ).

relevant_to_tasks( Move, Tasks ):-
	setof( task(Task), relevant_to_task( Move, Task ), Tasks ).


/*----------------------------------------------------------------------
   dominates( T1, T2 )
   -- Task T1 dominates T2 in the menu hierarchy
----------------------------------------------------------------------*/

dominates0( T1, T2 ):-
	plan( T1, [ findout( Ts ) | _ ] ),
	member( task( T2 ), Ts ).

dominates( T1, T2 ):-
	dominates0( T1, T2 ).

dominates( T1, T3 ):-
	dominates0( T1, T2 ),
	dominates( T2, T3 ).
	



/*----------------------------------------------------------------------
   reduce( +Q, +A, -P )

   -- reduces a quesition and an answer to a proposition

   ** this should perhaps be in the definition of the datatypes "question"
   and "answer", as an operation which takes a question and an answer
   and yields a question
----------------------------------------------------------------------*/


/*
** Y/N-questions
*/

reduce( Q, yes, P ):-
	ynq( Q ),
	P = Q. 

reduce( Q, no, P ):-
	ynq( Q ),
	P = not( Q ).

reduce( Q, P, P ):-
	ynq( Q ),
	P = Q.

reduce( Q, not(P), not(P) ):-
	ynq( Q ),
	P = Q.

/*
** Alt-questions
*/

% findout([ A1, A2, ..., An ]) is answered by A1 or A2 or .... or An

reduce(AltList, A, P):-
	member(A, AltList),!,
	P = A.

% findout([Pred(_),...]) is answered by Pred(_)

reduce(AltList, A, P):-
	AltList = [ A0 | _ ],
	A0 =.. [ Pred | _ ],
	A =.. [ Pred | _ ],!,
	P = A.



/*
** WH-questions
*/

% X^Q is answered by an atom (ellipsis)

reduce(X^P, X, P):-
	atom(X).

% X^Q is answered by full proposition

% question of form X^P(Y,X)

reduce( X^Q, A, P ):-
	Q =.. [Y,_,X],
	A =.. [Y,_,X],
	P = Q,
	!.

% question of form X^P(X)

reduce( X^Q, A, P ):-
	Q =.. [Y,X],
	A =.. [Y,X],
	P = Q,
	!.

% abstract(+A,+P,-Q)
%
% Q is a question s.t. reduce(Q,A,P) holds

% abstract( A, P, _^P ).
abstract( A, P, X^Q ):-
	A =.. [Y,_],
	P =.. [Y,_],
	Q =.. [Y,X],
%	P = Q,
	!.

sysaction( dummy ).