(define (domain blocksworld)
  (:requirements :strips :universal-preconditions)
  (:predicates (on ?x ?y)
	       (clear ?y)

  (:action ptodo
	     :parameters (?a ?b)
	     :precondition (forall (?a) (on ?a ?b))
	     :effect (and (clear ?a))
  )
)
