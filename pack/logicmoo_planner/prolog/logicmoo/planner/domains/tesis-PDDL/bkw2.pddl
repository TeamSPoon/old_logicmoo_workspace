(define (domain blocksworld)
(:requirements :strips)
(:predicates (clear ?x)
             (ontable ?x)
	     (armempty)
             (holding ?x)
             (on ?x ?y))

(:action pickup
  :parameters (?ob)
  :precondition (clear ?ob)
  :effect (and (holding ?ob) (not (clear ?ob)) (not (armempty))))

(:action stack
  :parameters  (?ob ?underob)
  :precondition (and  (clear ?underob) (holding ?ob))
  :effect (and (clear ?ob) (on ?ob ?underob) (armempty)
               (not (clear ?underob)) (not (holding ?ob))))
)
