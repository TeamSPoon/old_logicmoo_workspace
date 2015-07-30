(define (domain blocksworld)
  (:requirements :strips :universal-preconditions)
  (:predicates (holding ?x)
	       (clear ?y)
               (armempty))

  (:constants table)

  (:action pickup2
	     :parameters (?a)
	     :precondition (forall (?a) (clear ?a))
	     :effect (and (holding ?a) (not (armempty)))
  )
)
