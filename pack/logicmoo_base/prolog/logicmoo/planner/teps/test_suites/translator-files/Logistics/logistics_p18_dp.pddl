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
    (autstate_1_2 ?x0)
    (autstate_1_1 ?x0)
    (autstate_1_3 ?x0)
    (prev_autstate_1_2 ?x0)
    (prev_autstate_1_1 ?x0)
    (prev_autstate_1_3 ?x0)
    (autstate_2_2)
    (autstate_2_1)
    (prev_autstate_2_2)
    (prev_autstate_2_1)
    (aut_in_final_1 ?x0)
    (aut_in_final_2)
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
        (forall (?x3)
          (when
            (autstate_1_2 ?x3)
            (prev_autstate_1_2 ?x3)))

        (forall (?x3)
          (when
            (autstate_1_1 ?x3)
            (prev_autstate_1_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_1_3 ?x3)
            (prev_autstate_1_3 ?x3)))

        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (at ?x0 ?x2))
        (forall (?x3)
          (when
            (not 
              (autstate_1_2 ?x3))
            (not 
              (prev_autstate_1_2 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_1_1 ?x3))
            (not 
              (prev_autstate_1_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_1_3 ?x3))
            (not 
              (prev_autstate_1_3 ?x3))))

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
        (forall (?x3)
          (when
            (autstate_1_2 ?x3)
            (prev_autstate_1_2 ?x3)))

        (forall (?x3)
          (when
            (autstate_1_1 ?x3)
            (prev_autstate_1_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_1_3 ?x3)
            (prev_autstate_1_3 ?x3)))

        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (in ?x0 ?x1))
        (forall (?x3)
          (when
            (not 
              (autstate_1_2 ?x3))
            (not 
              (prev_autstate_1_2 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_1_1 ?x3))
            (not 
              (prev_autstate_1_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_1_3 ?x3))
            (not 
              (prev_autstate_1_3 ?x3))))

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
        (forall (?x4)
          (when
            (autstate_1_2 ?x4)
            (prev_autstate_1_2 ?x4)))

        (forall (?x4)
          (when
            (autstate_1_1 ?x4)
            (prev_autstate_1_1 ?x4)))

        (forall (?x4)
          (when
            (autstate_1_3 ?x4)
            (prev_autstate_1_3 ?x4)))

        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (at ?x0 ?x1))
        (forall (?x4)
          (when
            (not 
              (autstate_1_2 ?x4))
            (not 
              (prev_autstate_1_2 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_1_1 ?x4))
            (not 
              (prev_autstate_1_1 ?x4))))

        (forall (?x4)
          (when
            (not 
              (autstate_1_3 ?x4))
            (not 
              (prev_autstate_1_3 ?x4))))

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
        (forall (?x3)
          (when
            (autstate_1_2 ?x3)
            (prev_autstate_1_2 ?x3)))

        (forall (?x3)
          (when
            (autstate_1_1 ?x3)
            (prev_autstate_1_1 ?x3)))

        (forall (?x3)
          (when
            (autstate_1_3 ?x3)
            (prev_autstate_1_3 ?x3)))

        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (at ?x0 ?x1))
        (forall (?x3)
          (when
            (not 
              (autstate_1_2 ?x3))
            (not 
              (prev_autstate_1_2 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_1_1 ?x3))
            (not 
              (prev_autstate_1_1 ?x3))))

        (forall (?x3)
          (when
            (not 
              (autstate_1_3 ?x3))
            (not 
              (prev_autstate_1_3 ?x3))))

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
      )
    )
(:derived 
    (autstate_1_1 ?x0)
    (or
      (and
        (prev_autstate_1_2 ?x0)
        (not 
          (airport ?x0)))
      (or
        (and
          (prev_autstate_1_2 ?x0)
          (exists (?x1)
            (and
              (airplane ?x1)
              (at ?x1 ?x0)))
)
        (or
          (prev_autstate_1_1 ?x0)
          (and
            (prev_autstate_1_3 ?x0)
            (exists (?x2)
              (and
                (airplane ?x2)
                (at ?x2 ?x0)))
))))
)

(:derived 
    (autstate_1_3 ?x0)
    (or
      (prev_autstate_1_2 ?x0)
      (prev_autstate_1_3 ?x0))
)

(:derived 
    (autstate_2_2)
    (prev_autstate_2_2)
)

(:derived 
    (autstate_2_1)
    (and
      (prev_autstate_2_2)
      (at package6 city3_1))
)

(:derived 
    (aut_in_final_1 ?x0)
    (autstate_1_1 ?x0)
)

(:derived 
    (aut_in_final_2)
    (autstate_2_1)
)

)