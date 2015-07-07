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
    (autstate_1_4)
    (autstate_1_5)
    (autstate_1_3)
    (autstate_1_2)
    (autstate_1_1)
    (prev_autstate_1_4)
    (prev_autstate_1_5)
    (prev_autstate_1_3)
    (prev_autstate_1_2)
    (prev_autstate_1_1)
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
          (autstate_1_4)
          (prev_autstate_1_4))
        (when
          (autstate_1_5)
          (prev_autstate_1_5))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (not 
          (closed ?x0))
        (when
          (not 
            (autstate_1_4))
          (not 
            (prev_autstate_1_4)))
        (when
          (not 
            (autstate_1_5))
          (not 
            (prev_autstate_1_5)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
          (autstate_1_4)
          (prev_autstate_1_4))
        (when
          (autstate_1_5)
          (prev_autstate_1_5))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (not 
          (opened ?x0))
        (when
          (not 
            (autstate_1_4))
          (not 
            (prev_autstate_1_4)))
        (when
          (not 
            (autstate_1_5))
          (not 
            (prev_autstate_1_5)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
          (autstate_1_4)
          (prev_autstate_1_4))
        (when
          (autstate_1_5)
          (prev_autstate_1_5))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (not 
          (handempty))
        (when
          (not 
            (autstate_1_4))
          (not 
            (prev_autstate_1_4)))
        (when
          (not 
            (autstate_1_5))
          (not 
            (prev_autstate_1_5)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
          (autstate_1_4)
          (prev_autstate_1_4))
        (when
          (autstate_1_5)
          (prev_autstate_1_5))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (not 
          (holding ?x0))
        (when
          (not 
            (autstate_1_4))
          (not 
            (prev_autstate_1_4)))
        (when
          (not 
            (autstate_1_5))
          (not 
            (prev_autstate_1_5)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
          (autstate_1_4)
          (prev_autstate_1_4))
        (when
          (autstate_1_5)
          (prev_autstate_1_5))
        (when
          (autstate_1_3)
          (prev_autstate_1_3))
        (when
          (autstate_1_2)
          (prev_autstate_1_2))
        (when
          (autstate_1_1)
          (prev_autstate_1_1))
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (not 
              (at ?x2 ?x0))))

        (when
          (not 
            (autstate_1_4))
          (not 
            (prev_autstate_1_4)))
        (when
          (not 
            (autstate_1_5))
          (not 
            (prev_autstate_1_5)))
        (when
          (not 
            (autstate_1_3))
          (not 
            (prev_autstate_1_3)))
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
      )
    )
(:derived 
    (autstate_1_4)
    (prev_autstate_1_4)
)

(:derived 
    (autstate_1_5)
    (or
      (and
        (prev_autstate_1_4)
        (at o1 r2))
      (prev_autstate_1_5))
)

(:derived 
    (autstate_1_3)
    (or
      (and
        (prev_autstate_1_5)
        (and
          (at o4 r4)
          (and
            (at o3 r4)
            (and
              (at o1 r4)
              (at o2 r4)))))
      (prev_autstate_1_3))
)

(:derived 
    (autstate_1_2)
    (or
      (and
        (prev_autstate_1_3)
        (and
          (at o4 c1)
          (and
            (at o3 c1)
            (and
              (at o1 c1)
              (at o2 c1)))))
      (prev_autstate_1_2))
)

(:derived 
    (autstate_1_1)
    (or
      (and
        (prev_autstate_1_2)
        (and
          (at o4 r1)
          (and
            (at o3 r1)
            (and
              (at o1 r1)
              (at o2 r1)))))
      (prev_autstate_1_1))
)

)