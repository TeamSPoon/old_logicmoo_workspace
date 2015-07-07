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
    (autstate_2_2)
    (autstate_2_1)
    (autstate_3_2)
    (autstate_3_1)
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
          (and
            (autstate_1_2)
            (at obj1 r4))
          (autstate_1_1))
        (when
          (and
            (autstate_2_2)
            (at obj1 r2))
          (autstate_2_1))
        (when
          (and
            (autstate_3_2)
            (and
              (at robot c1)
              (at obj2 r4)))
          (autstate_3_1))
        (not 
          (closed ?x0))
        (when
          (not 
            (and
              (autstate_3_2)
              (and
                (at robot c1)
                (at obj2 r4))))
          (not 
            (autstate_3_1)))
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
          (and
            (autstate_1_2)
            (at obj1 r4))
          (autstate_1_1))
        (when
          (and
            (autstate_2_2)
            (at obj1 r2))
          (autstate_2_1))
        (when
          (and
            (autstate_3_2)
            (and
              (at robot c1)
              (at obj2 r4)))
          (autstate_3_1))
        (not 
          (opened ?x0))
        (when
          (not 
            (and
              (autstate_3_2)
              (and
                (at robot c1)
                (at obj2 r4))))
          (not 
            (autstate_3_1)))
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
          (and
            (autstate_1_2)
            (at obj1 r4))
          (autstate_1_1))
        (when
          (and
            (autstate_2_2)
            (at obj1 r2))
          (autstate_2_1))
        (when
          (and
            (autstate_3_2)
            (and
              (at robot c1)
              (at obj2 r4)))
          (autstate_3_1))
        (not 
          (handempty))
        (when
          (not 
            (and
              (autstate_3_2)
              (and
                (at robot c1)
                (at obj2 r4))))
          (not 
            (autstate_3_1)))
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
          (and
            (autstate_1_2)
            (at obj1 r4))
          (autstate_1_1))
        (when
          (and
            (autstate_2_2)
            (at obj1 r2))
          (autstate_2_1))
        (when
          (and
            (autstate_3_2)
            (and
              (at robot c1)
              (at obj2 r4)))
          (autstate_3_1))
        (not 
          (holding ?x0))
        (when
          (not 
            (and
              (autstate_3_2)
              (and
                (at robot c1)
                (at obj2 r4))))
          (not 
            (autstate_3_1)))
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
          (and
            (autstate_1_2)
            (or
              (and
                (= ?x1 r4)
                (holding obj1))
              (and
                (at obj1 r4)
                (not 
                  (and
                    (= ?x0 r4)
                    (holding obj1))))))
          (autstate_1_1))
        (when
          (and
            (autstate_2_2)
            (or
              (and
                (= ?x1 r2)
                (holding obj1))
              (and
                (at obj1 r2)
                (not 
                  (and
                    (= ?x0 r2)
                    (holding obj1))))))
          (autstate_2_1))
        (when
          (and
            (autstate_3_2)
            (or
              (or
                (and
                  (= ?x1 c1)
                  (and
                    (= ?x1 r4)
                    (holding obj2)))
                (or
                  (and
                    (at robot c1)
                    (and
                      (not 
                        (= ?x0 c1))
                      (and
                        (= ?x1 r4)
                        (holding obj2))))
                  (and
                    (at obj2 r4)
                    (and
                      (not 
                        (and
                          (= ?x0 r4)
                          (holding obj2)))
                      (= ?x1 c1)))))
              (and
                (and
                  (at robot c1)
                  (at obj2 r4))
                (not 
                  (or
                    (= ?x0 c1)
                    (and
                      (= ?x0 r4)
                      (holding obj2)))))))
          (autstate_3_1))
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (not 
              (at ?x2 ?x0))))

        (when
          (not 
            (and
              (autstate_3_2)
              (or
                (or
                  (and
                    (= ?x1 c1)
                    (and
                      (= ?x1 r4)
                      (holding obj2)))
                  (or
                    (and
                      (at robot c1)
                      (and
                        (not 
                          (= ?x0 c1))
                        (and
                          (= ?x1 r4)
                          (holding obj2))))
                    (and
                      (at obj2 r4)
                      (and
                        (not 
                          (and
                            (= ?x0 r4)
                            (holding obj2)))
                        (= ?x1 c1)))))
                (and
                  (and
                    (at robot c1)
                    (at obj2 r4))
                  (not 
                    (or
                      (= ?x0 c1)
                      (and
                        (= ?x0 r4)
                        (holding obj2))))))))
          (not 
            (autstate_3_1)))
      )
    )
)