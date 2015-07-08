(define (domain rooms)
  (:predicates
    (at ?x0 ?x1)
    (connects ?x0 ?x1 ?x2)
    (opened ?x0)
    (closed ?x0)
    (door ?x0)
    (holding ?x0)
    (object ?x0)
    (handempty)
    (autstate_1_2)
    (autstate_1_1)
    (autstate_1_3)
    (prev_autstate_1_2)
    (prev_autstate_1_1)
    (prev_autstate_1_3)
    (autstate_2_2)
    (autstate_2_1)
    (prev_autstate_2_2)
    (prev_autstate_2_1)
  )
  (:action open
    :parameters (?x0)
    :precondition 
      (exists (?x1 ?x2)
        (and
          (door ?x0)
          (and
            (at robot ?x1)
            (and
              (connects ?x0 ?x1 ?x2)
              (closed ?x0)))))

    :effect
      (and
        (opened ?x0)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (closed ?x0))
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
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
  (:action close
    :parameters (?x0)
    :precondition 
      (exists (?x1 ?x2)
        (and
          (door ?x0)
          (and
            (at robot ?x1)
            (and
              (connects ?x0 ?x1 ?x2)
              (opened ?x0)))))

    :effect
      (and
        (closed ?x0)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (opened ?x0))
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
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
  (:action grasp
    :parameters (?x0)
    :precondition 
      (exists (?x1)
        (and
          (object ?x0)
          (and
            (at robot ?x1)
            (and
              (at ?x0 ?x1)
              (handempty)))))

    :effect
      (and
        (holding ?x0)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (handempty))
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
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
  (:action release
    :parameters (?x0)
    :precondition 
      (holding ?x0)
    :effect
      (and
        (handempty)
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (not 
          (holding ?x0))
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
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
  (:action move
    :parameters (?x0 ?x1)
    :precondition 
      (exists (?x2)
        (and
          (at robot ?x0)
          (and
            (connects ?x2 ?x0 ?x1)
            (opened ?x2))))

    :effect
      (and
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (at ?x2 ?x1)))

        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_2_2)
          (prev_autstate_2_2))
        (when
          (autstate_2_1)
          (prev_autstate_2_1))
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (not 
              (at ?x2 ?x0))))

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
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
    (autstate_1_2)
    (prev_autstate_1_2)
)

(:derived 
    (autstate_1_1)
    (or
      (prev_autstate_1_1)
      (and
        (prev_autstate_1_3)
        (and
          (closed d1)
          (and
            (closed d12)
            (and
              (closed d23)
              (and
                (closed d34)
                (closed d4)))))))
)

(:derived 
    (autstate_1_3)
    (or
      (and
        (prev_autstate_1_2)
        (and
          (at obj3 r4)
          (at obj4 r4)))
      (and
        (prev_autstate_1_2)
        (and
          (at obj2 r3)
          (and
            (at obj3 r3)
            (at obj4 r3)))))
)

(:derived 
    (autstate_2_2)
    (prev_autstate_2_2)
)

(:derived 
    (autstate_2_1)
    (and
      (prev_autstate_2_2)
      (at obj1 c1))
)

)