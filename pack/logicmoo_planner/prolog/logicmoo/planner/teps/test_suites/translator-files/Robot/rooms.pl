fluent(at(_,_)).
fluent(connects(_,_,_)).
fluent(opened(_)).
fluent(closed(_)).
fluent(door(_)).
fluent(holding(_)).
fluent(object(_)).
fluent(handempty).

action(open(_)).
action(close(_)).
action(grasp(_)).
action(release(_)).
action(move(_,_)).

poss(open(D),and(door(D),
		 and(at(robot,X),
		     and(connects(D,X,Y),
			 closed(D))))).

poss(close(D),and(door(D),
		  and(at(robot,X),
		      and(connects(D,X,Y),
			  opened(D))))).

poss(grasp(O),and(object(O),
		  and(at(robot,X),
		      and(at(O,X),
			  handempty)))).

poss(release(O),holding(O)).
poss(move(X,Y),and(at(robot,X),and(connects(D,X,Y),opened(D)))).

causes_true(opened(D),open(D),true).
causes_true(closed(D),close(D),true).
causes_true(holding(O),grasp(O),true).
causes_true(handempty,release(O),true).
causes_true(at(O,Y),move(X,Y),or(equal(O,robot),holding(O))).

causes_false(closed(D),open(D),true).
causes_false(opened(D),close(D),true).
causes_false(handempty,grasp(O),true).
causes_false(holding(O),release(O),true).
causes_false(at(O,X),move(X,Y),or(equal(O,robot),holding(O))).
