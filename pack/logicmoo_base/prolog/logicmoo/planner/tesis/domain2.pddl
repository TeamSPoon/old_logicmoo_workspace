(define (domain blocksworld)
(:requirements :strips :equality)
(:predicates (clear ?x)
             (ontable ?x)
             (armempty)
             (holding ?x)
             (on ?x ?y))

(:action pickup
  :parameters (?ob)
  :precondition (and (clear ?ob) (ontable ?ob) (armempty) (= ?ob ?ob))
  :effect (and (holding ?ob) (not (clear ?ob)) (not (ontable ?ob)) 
               (not (armempty))))

(:action stack
  :parameters  (?ob ?underob)
  :precondition (and  (clear ?underob) (holding ?ob))
  :effect (and (armempty) (clear ?ob) (on ?ob ?underob)
               (not (clear ?underob)) (not (holding ?ob))))
)
