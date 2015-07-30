(define (domain blocksworld)
  (:requirements :strips :universal-preconditions)
  (:predicates (holding ?x)
	       (clear ?y)
               (armempty))

  (:constants table)

  (:action pickup2
	     :parameters (?a)
	     :precondition (and (armempty) (forall (?a) (clear ?a) (istable ?a)))
	     :effect (and (holding ?a) (not (armempty)))
  )
)
