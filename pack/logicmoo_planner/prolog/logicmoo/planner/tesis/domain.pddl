(define (domain blocksworld)
(:requirements :strips :equality)
(:predicates 
	     (clear ?x)
	     (armempty)
             (ontable ?x)
             (holding ?x)
             (on ?x ?y))

(:action pickup
  :parameters (?ob)
  :precondition (and (clear ?ob) (ontable ?ob) (armempty))
  :effect (and (holding ?ob) (not (clear ?ob)) (not (ontable ?ob))))

(:action stack
  :parameters  (?ob ?underob)
  :precondition (and  (clear ?underob) (holding ?ob))
  :effect (and (clear ?ob) (on ?ob ?underob) (armempty)
               (not (clear ?underob)) (not (holding ?ob))))
)



