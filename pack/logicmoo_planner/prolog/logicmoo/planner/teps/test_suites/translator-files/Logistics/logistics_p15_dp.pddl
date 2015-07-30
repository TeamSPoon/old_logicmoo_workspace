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
    (aut_in_final_1 ?x0)
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

      )
    )
(:derived 
    (autstate_1_1 ?x0)
    (or
      (and
        (prev_autstate_1_2 ?x0)
        (not 
          (in_city ?x0 city4)))
      (or
        (and
          (prev_autstate_1_2 ?x0)
          (at package6 ?x0))
        (or
          (prev_autstate_1_1 ?x0)
          (and
            (prev_autstate_1_3 ?x0)
            (at package6 ?x0)))))
)

(:derived 
    (autstate_1_3 ?x0)
    (or
      (prev_autstate_1_2 ?x0)
      (prev_autstate_1_3 ?x0))
)

(:derived 
    (aut_in_final_1 ?x0)
    (autstate_1_1 ?x0)
)

)