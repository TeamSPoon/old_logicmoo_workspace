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
    (autstate_2_3)
    (autstate_2_1)
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
            (and
              (at obj1 c4)
              (and
                (at obj2 c4)
                (and
                  (at obj3 c4)
                  (at obj4 c4)))))
          (autstate_1_1))
        (when
          (autstate_2_2)
          (autstate_2_3))
        (when
          (and
            (autstate_2_3)
            (and
              (at obj1 c1)
              (and
                (at obj2 c1)
                (and
                  (at obj3 c1)
                  (at obj4 c1)))))
          (autstate_2_1))
        (not 
          (closed ?x0))
        (not 
          (autstate_2_2))
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
            (and
              (at obj1 c4)
              (and
                (at obj2 c4)
                (and
                  (at obj3 c4)
                  (at obj4 c4)))))
          (autstate_1_1))
        (when
          (autstate_2_2)
          (autstate_2_3))
        (when
          (and
            (autstate_2_3)
            (and
              (at obj1 c1)
              (and
                (at obj2 c1)
                (and
                  (at obj3 c1)
                  (at obj4 c1)))))
          (autstate_2_1))
        (not 
          (opened ?x0))
        (not 
          (autstate_2_2))
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
            (and
              (at obj1 c4)
              (and
                (at obj2 c4)
                (and
                  (at obj3 c4)
                  (at obj4 c4)))))
          (autstate_1_1))
        (when
          (autstate_2_2)
          (autstate_2_3))
        (when
          (and
            (autstate_2_3)
            (and
              (at obj1 c1)
              (and
                (at obj2 c1)
                (and
                  (at obj3 c1)
                  (at obj4 c1)))))
          (autstate_2_1))
        (not 
          (handempty))
        (not 
          (autstate_2_2))
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
            (and
              (at obj1 c4)
              (and
                (at obj2 c4)
                (and
                  (at obj3 c4)
                  (at obj4 c4)))))
          (autstate_1_1))
        (when
          (autstate_2_2)
          (autstate_2_3))
        (when
          (and
            (autstate_2_3)
            (and
              (at obj1 c1)
              (and
                (at obj2 c1)
                (and
                  (at obj3 c1)
                  (at obj4 c1)))))
          (autstate_2_1))
        (not 
          (holding ?x0))
        (not 
          (autstate_2_2))
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
              (or
                (and
                  (and
                    (= ?x1 c4)
                    (holding obj1))
                  (or
                    (and
                      (and
                        (= ?x1 c4)
                        (holding obj2))
                      (or
                        (and
                          (and
                            (= ?x1 c4)
                            (holding obj3))
                          (and
                            (= ?x1 c4)
                            (holding obj4)))
                        (or
                          (and
                            (at obj3 c4)
                            (and
                              (not 
                                (and
                                  (= ?x0 c4)
                                  (holding obj3)))
                              (and
                                (= ?x1 c4)
                                (holding obj4))))
                          (and
                            (at obj4 c4)
                            (and
                              (not 
                                (and
                                  (= ?x0 c4)
                                  (holding obj4)))
                              (and
                                (= ?x1 c4)
                                (holding obj3)))))))
                    (or
                      (and
                        (at obj2 c4)
                        (and
                          (not 
                            (and
                              (= ?x0 c4)
                              (holding obj2)))
                          (or
                            (and
                              (and
                                (= ?x1 c4)
                                (holding obj3))
                              (and
                                (= ?x1 c4)
                                (holding obj4)))
                            (or
                              (and
                                (at obj3 c4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c4)
                                      (holding obj3)))
                                  (and
                                    (= ?x1 c4)
                                    (holding obj4))))
                              (and
                                (at obj4 c4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c4)
                                      (holding obj4)))
                                  (and
                                    (= ?x1 c4)
                                    (holding obj3))))))))
                      (and
                        (and
                          (at obj3 c4)
                          (at obj4 c4))
                        (and
                          (not 
                            (or
                              (and
                                (= ?x0 c4)
                                (holding obj3))
                              (and
                                (= ?x0 c4)
                                (holding obj4))))
                          (and
                            (= ?x1 c4)
                            (holding obj2)))))))
                (or
                  (and
                    (at obj1 c4)
                    (and
                      (not 
                        (and
                          (= ?x0 c4)
                          (holding obj1)))
                      (or
                        (and
                          (and
                            (= ?x1 c4)
                            (holding obj2))
                          (or
                            (and
                              (and
                                (= ?x1 c4)
                                (holding obj3))
                              (and
                                (= ?x1 c4)
                                (holding obj4)))
                            (or
                              (and
                                (at obj3 c4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c4)
                                      (holding obj3)))
                                  (and
                                    (= ?x1 c4)
                                    (holding obj4))))
                              (and
                                (at obj4 c4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c4)
                                      (holding obj4)))
                                  (and
                                    (= ?x1 c4)
                                    (holding obj3)))))))
                        (or
                          (and
                            (at obj2 c4)
                            (and
                              (not 
                                (and
                                  (= ?x0 c4)
                                  (holding obj2)))
                              (or
                                (and
                                  (and
                                    (= ?x1 c4)
                                    (holding obj3))
                                  (and
                                    (= ?x1 c4)
                                    (holding obj4)))
                                (or
                                  (and
                                    (at obj3 c4)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 c4)
                                          (holding obj3)))
                                      (and
                                        (= ?x1 c4)
                                        (holding obj4))))
                                  (and
                                    (at obj4 c4)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 c4)
                                          (holding obj4)))
                                      (and
                                        (= ?x1 c4)
                                        (holding obj3))))))))
                          (and
                            (and
                              (at obj3 c4)
                              (at obj4 c4))
                            (and
                              (not 
                                (or
                                  (and
                                    (= ?x0 c4)
                                    (holding obj3))
                                  (and
                                    (= ?x0 c4)
                                    (holding obj4))))
                              (and
                                (= ?x1 c4)
                                (holding obj2))))))))
                  (and
                    (and
                      (at obj2 c4)
                      (and
                        (at obj3 c4)
                        (at obj4 c4)))
                    (and
                      (not 
                        (or
                          (and
                            (= ?x0 c4)
                            (holding obj2))
                          (or
                            (and
                              (= ?x0 c4)
                              (holding obj3))
                            (and
                              (= ?x0 c4)
                              (holding obj4)))))
                      (and
                        (= ?x1 c4)
                        (holding obj1))))))
              (and
                (and
                  (at obj1 c4)
                  (and
                    (at obj2 c4)
                    (and
                      (at obj3 c4)
                      (at obj4 c4))))
                (not 
                  (or
                    (and
                      (= ?x0 c4)
                      (holding obj1))
                    (or
                      (and
                        (= ?x0 c4)
                        (holding obj2))
                      (or
                        (and
                          (= ?x0 c4)
                          (holding obj3))
                        (and
                          (= ?x0 c4)
                          (holding obj4)))))))))
          (autstate_1_1))
        (when
          (autstate_2_2)
          (autstate_2_3))
        (when
          (and
            (autstate_2_3)
            (or
              (or
                (and
                  (and
                    (= ?x1 c1)
                    (holding obj1))
                  (or
                    (and
                      (and
                        (= ?x1 c1)
                        (holding obj2))
                      (or
                        (and
                          (and
                            (= ?x1 c1)
                            (holding obj3))
                          (and
                            (= ?x1 c1)
                            (holding obj4)))
                        (or
                          (and
                            (at obj3 c1)
                            (and
                              (not 
                                (and
                                  (= ?x0 c1)
                                  (holding obj3)))
                              (and
                                (= ?x1 c1)
                                (holding obj4))))
                          (and
                            (at obj4 c1)
                            (and
                              (not 
                                (and
                                  (= ?x0 c1)
                                  (holding obj4)))
                              (and
                                (= ?x1 c1)
                                (holding obj3)))))))
                    (or
                      (and
                        (at obj2 c1)
                        (and
                          (not 
                            (and
                              (= ?x0 c1)
                              (holding obj2)))
                          (or
                            (and
                              (and
                                (= ?x1 c1)
                                (holding obj3))
                              (and
                                (= ?x1 c1)
                                (holding obj4)))
                            (or
                              (and
                                (at obj3 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding obj3)))
                                  (and
                                    (= ?x1 c1)
                                    (holding obj4))))
                              (and
                                (at obj4 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding obj4)))
                                  (and
                                    (= ?x1 c1)
                                    (holding obj3))))))))
                      (and
                        (and
                          (at obj3 c1)
                          (at obj4 c1))
                        (and
                          (not 
                            (or
                              (and
                                (= ?x0 c1)
                                (holding obj3))
                              (and
                                (= ?x0 c1)
                                (holding obj4))))
                          (and
                            (= ?x1 c1)
                            (holding obj2)))))))
                (or
                  (and
                    (at obj1 c1)
                    (and
                      (not 
                        (and
                          (= ?x0 c1)
                          (holding obj1)))
                      (or
                        (and
                          (and
                            (= ?x1 c1)
                            (holding obj2))
                          (or
                            (and
                              (and
                                (= ?x1 c1)
                                (holding obj3))
                              (and
                                (= ?x1 c1)
                                (holding obj4)))
                            (or
                              (and
                                (at obj3 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding obj3)))
                                  (and
                                    (= ?x1 c1)
                                    (holding obj4))))
                              (and
                                (at obj4 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding obj4)))
                                  (and
                                    (= ?x1 c1)
                                    (holding obj3)))))))
                        (or
                          (and
                            (at obj2 c1)
                            (and
                              (not 
                                (and
                                  (= ?x0 c1)
                                  (holding obj2)))
                              (or
                                (and
                                  (and
                                    (= ?x1 c1)
                                    (holding obj3))
                                  (and
                                    (= ?x1 c1)
                                    (holding obj4)))
                                (or
                                  (and
                                    (at obj3 c1)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 c1)
                                          (holding obj3)))
                                      (and
                                        (= ?x1 c1)
                                        (holding obj4))))
                                  (and
                                    (at obj4 c1)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 c1)
                                          (holding obj4)))
                                      (and
                                        (= ?x1 c1)
                                        (holding obj3))))))))
                          (and
                            (and
                              (at obj3 c1)
                              (at obj4 c1))
                            (and
                              (not 
                                (or
                                  (and
                                    (= ?x0 c1)
                                    (holding obj3))
                                  (and
                                    (= ?x0 c1)
                                    (holding obj4))))
                              (and
                                (= ?x1 c1)
                                (holding obj2))))))))
                  (and
                    (and
                      (at obj2 c1)
                      (and
                        (at obj3 c1)
                        (at obj4 c1)))
                    (and
                      (not 
                        (or
                          (and
                            (= ?x0 c1)
                            (holding obj2))
                          (or
                            (and
                              (= ?x0 c1)
                              (holding obj3))
                            (and
                              (= ?x0 c1)
                              (holding obj4)))))
                      (and
                        (= ?x1 c1)
                        (holding obj1))))))
              (and
                (and
                  (at obj1 c1)
                  (and
                    (at obj2 c1)
                    (and
                      (at obj3 c1)
                      (at obj4 c1))))
                (not 
                  (or
                    (and
                      (= ?x0 c1)
                      (holding obj1))
                    (or
                      (and
                        (= ?x0 c1)
                        (holding obj2))
                      (or
                        (and
                          (= ?x0 c1)
                          (holding obj3))
                        (and
                          (= ?x0 c1)
                          (holding obj4)))))))))
          (autstate_2_1))
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (not 
              (at ?x2 ?x0))))

        (not 
          (autstate_2_2))
      )
    )
)