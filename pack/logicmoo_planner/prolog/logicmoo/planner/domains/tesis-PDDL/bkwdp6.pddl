(define(domain attbw)
  (:requirements :strips :equality :disjuntive-preconditions)
  (:predicates (istable ?t)
	       (block ?b)
	       (clear ?x)
	       (on ?x ?y))

  (:constants table)
  
  (:action move
   :parameters (?obj ?source ?dest)
        :precondition (or (and (istable ?dest) (clear ?source)) (clear ?dest))
        :effect (and (clear ?source)
		     (on ?obj ?dest)
		     (not (on ?obj ?source))
                     (not (clear ?dest)))
    )
)
