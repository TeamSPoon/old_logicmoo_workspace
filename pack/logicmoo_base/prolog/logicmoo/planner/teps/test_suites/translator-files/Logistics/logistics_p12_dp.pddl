(define (domain logistics)
  (:predicates
    (obj ?x0)
    (truck ?x0)
    (location ?x0)
    (airplane ?x0)
    (city ?x0)
    (airport ?x0)
    (at ?x0 ?x1)
    (in ?x0 ?x1)
    (in_city ?x0 ?x1)
    (autstate_1_2)
    (autstate_1_1)
    (prev_autstate_1_2)
    (prev_autstate_1_1)
    (autstate_2_2)
    (autstate_2_1)
    (prev_autstate_2_2)
    (prev_autstate_2_1)
    (autstate_3_2)
    (autstate_3_1)
    (prev_autstate_3_2)
    (prev_autstate_3_1)
    (aut_in_final_1)
    (aut_in_final_2)
    (aut_in_final_3)
  )
  (:action load
    :parameters (?x0 ?x1 ?x2)
    :precondition 
      (and
        (obj ?x0)
        (and
          (or
            (airplane ?x1)
            (truck ?x1))
          (and
            (at ?x0 ?x2)
            (at ?x1 ?x2))))
    :effect
      (and
        (in ?x0 ?x1)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (when
          (autstate_3_2)
          (prev_autstate_3_2))
        (when
          (autstate_3_1)
          (prev_autstate_3_1))
        (not 
          (at ?x0 ?x2))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
        (when
          (not 
            (autstate_3_2))
          (not 
            (prev_autstate_3_2)))
        (when
          (not 
            (autstate_3_1))
          (not 
            (prev_autstate_3_1)))
      )
    )
  (:action unload
    :parameters (?x0 ?x1 ?x2)
    :precondition 
      (and
        (obj ?x0)
        (and
          (or
            (airplane ?x1)
            (truck ?x1))
          (and
            (location ?x2)
            (and
              (in ?x0 ?x1)
              (at ?x1 ?x2)))))
    :effect
      (and
        (at ?x0 ?x2)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (when
          (autstate_3_2)
          (prev_autstate_3_2))
        (when
          (autstate_3_1)
          (prev_autstate_3_1))
        (not 
          (in ?x0 ?x1))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
        (when
          (not 
            (autstate_3_2))
          (not 
            (prev_autstate_3_2)))
        (when
          (not 
            (autstate_3_1))
          (not 
            (prev_autstate_3_1)))
      )
    )
  (:action drive_truck
    :parameters (?x0 ?x1 ?x2 ?x3)
    :precondition 
      (and
        (truck ?x0)
        (and
          (location ?x1)
          (and
            (location ?x2)
            (and
              (city ?x3)
              (and
                (at ?x0 ?x1)
                (and
                  (in_city ?x1 ?x3)
                  (in_city ?x2 ?x3)))))))
    :effect
      (and
        (at ?x0 ?x2)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (when
          (autstate_3_2)
          (prev_autstate_3_2))
        (when
          (autstate_3_1)
          (prev_autstate_3_1))
        (not 
          (at ?x0 ?x1))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
        (when
          (not 
            (autstate_3_2))
          (not 
            (prev_autstate_3_2)))
        (when
          (not 
            (autstate_3_1))
          (not 
            (prev_autstate_3_1)))
      )
    )
  (:action fly_airplane
    :parameters (?x0 ?x1 ?x2)
    :precondition 
      (and
        (airplane ?x0)
        (and
          (airport ?x1)
          (and
            (airport ?x2)
            (at ?x0 ?x1))))
    :effect
      (and
        (at ?x0 ?x2)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (when
          (autstate_3_2)
          (prev_autstate_3_2))
        (when
          (autstate_3_1)
          (prev_autstate_3_1))
        (not 
          (at ?x0 ?x1))
        (when
          (not 
            (autstate_1_2))
          (not 
            (prev_autstate_1_2)))
        (when
          (not 
            (autstate_1_1))
          (not 
            (prev_autstate_1_1)))
        (when
          (not 
            (autstate_2_2))
          (not 
            (prev_autstate_2_2)))
        (when
          (not 
            (autstate_2_1))
          (not 
            (prev_autstate_2_1)))
        (when
          (not 
            (autstate_3_2))
          (not 
            (prev_autstate_3_2)))
        (when
          (not 
            (autstate_3_1))
          (not 
            (prev_autstate_3_1)))
      )
    )
(:derived 
    (autstate_1_2)
    (prev_autstate_1_2)
)

(:derived 
    (autstate_1_1)
    (or
      (and
        (prev_autstate_1_2)
        (at package6 city2_1))
      (prev_autstate_1_1))
)

(:derived 
    (autstate_2_2)
    (prev_autstate_2_2)
)

(:derived 
    (autstate_2_1)
    (or
      (and
        (prev_autstate_2_2)
        (at package6 city1_1))
      (prev_autstate_2_1))
)

(:derived 
    (autstate_3_2)
    (prev_autstate_3_2)
)

(:derived 
    (autstate_3_1)
    (or
      (and
        (prev_autstate_3_2)
        (at package6 city3_1))
      (prev_autstate_3_1))
)

(:derived 
    (aut_in_final_1)
    (autstate_1_1)
)

(:derived 
    (aut_in_final_2)
    (autstate_2_1)
)

(:derived 
    (aut_in_final_3)
    (autstate_3_1)
)

)