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
    (autstate_1_3)
    (autstate_1_4)
    (autstate_1_2)
    (autstate_1_1)
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
            (autstate_1_3)
            (at o1 r2))
          (autstate_1_4))
        (when
          (and
            (autstate_1_4)
            (and
              (at o4 r4)
              (and
                (at o3 r4)
                (and
                  (at o1 r4)
                  (at o2 r4)))))
          (autstate_1_2))
        (when
          (and
            (autstate_1_2)
            (and
              (at o4 c1)
              (and
                (at o3 c1)
                (and
                  (at o1 c1)
                  (at o2 c1)))))
          (autstate_1_1))
        (not 
          (closed ?x0))
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
            (autstate_1_3)
            (at o1 r2))
          (autstate_1_4))
        (when
          (and
            (autstate_1_4)
            (and
              (at o4 r4)
              (and
                (at o3 r4)
                (and
                  (at o1 r4)
                  (at o2 r4)))))
          (autstate_1_2))
        (when
          (and
            (autstate_1_2)
            (and
              (at o4 c1)
              (and
                (at o3 c1)
                (and
                  (at o1 c1)
                  (at o2 c1)))))
          (autstate_1_1))
        (not 
          (opened ?x0))
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
            (autstate_1_3)
            (at o1 r2))
          (autstate_1_4))
        (when
          (and
            (autstate_1_4)
            (and
              (at o4 r4)
              (and
                (at o3 r4)
                (and
                  (at o1 r4)
                  (at o2 r4)))))
          (autstate_1_2))
        (when
          (and
            (autstate_1_2)
            (and
              (at o4 c1)
              (and
                (at o3 c1)
                (and
                  (at o1 c1)
                  (at o2 c1)))))
          (autstate_1_1))
        (not 
          (handempty))
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
            (autstate_1_3)
            (at o1 r2))
          (autstate_1_4))
        (when
          (and
            (autstate_1_4)
            (and
              (at o4 r4)
              (and
                (at o3 r4)
                (and
                  (at o1 r4)
                  (at o2 r4)))))
          (autstate_1_2))
        (when
          (and
            (autstate_1_2)
            (and
              (at o4 c1)
              (and
                (at o3 c1)
                (and
                  (at o1 c1)
                  (at o2 c1)))))
          (autstate_1_1))
        (not 
          (holding ?x0))
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
            (autstate_1_3)
            (or
              (and
                (= ?x1 r2)
                (holding o1))
              (and
                (at o1 r2)
                (not 
                  (and
                    (= ?x0 r2)
                    (holding o1))))))
          (autstate_1_4))
        (when
          (and
            (autstate_1_4)
            (or
              (or
                (and
                  (and
                    (= ?x1 r4)
                    (holding o4))
                  (or
                    (and
                      (and
                        (= ?x1 r4)
                        (holding o3))
                      (or
                        (and
                          (and
                            (= ?x1 r4)
                            (holding o1))
                          (and
                            (= ?x1 r4)
                            (holding o2)))
                        (or
                          (and
                            (at o1 r4)
                            (and
                              (not 
                                (and
                                  (= ?x0 r4)
                                  (holding o1)))
                              (and
                                (= ?x1 r4)
                                (holding o2))))
                          (and
                            (at o2 r4)
                            (and
                              (not 
                                (and
                                  (= ?x0 r4)
                                  (holding o2)))
                              (and
                                (= ?x1 r4)
                                (holding o1)))))))
                    (or
                      (and
                        (at o3 r4)
                        (and
                          (not 
                            (and
                              (= ?x0 r4)
                              (holding o3)))
                          (or
                            (and
                              (and
                                (= ?x1 r4)
                                (holding o1))
                              (and
                                (= ?x1 r4)
                                (holding o2)))
                            (or
                              (and
                                (at o1 r4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 r4)
                                      (holding o1)))
                                  (and
                                    (= ?x1 r4)
                                    (holding o2))))
                              (and
                                (at o2 r4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 r4)
                                      (holding o2)))
                                  (and
                                    (= ?x1 r4)
                                    (holding o1))))))))
                      (and
                        (and
                          (at o1 r4)
                          (at o2 r4))
                        (and
                          (not 
                            (or
                              (and
                                (= ?x0 r4)
                                (holding o1))
                              (and
                                (= ?x0 r4)
                                (holding o2))))
                          (and
                            (= ?x1 r4)
                            (holding o3)))))))
                (or
                  (and
                    (at o4 r4)
                    (and
                      (not 
                        (and
                          (= ?x0 r4)
                          (holding o4)))
                      (or
                        (and
                          (and
                            (= ?x1 r4)
                            (holding o3))
                          (or
                            (and
                              (and
                                (= ?x1 r4)
                                (holding o1))
                              (and
                                (= ?x1 r4)
                                (holding o2)))
                            (or
                              (and
                                (at o1 r4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 r4)
                                      (holding o1)))
                                  (and
                                    (= ?x1 r4)
                                    (holding o2))))
                              (and
                                (at o2 r4)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 r4)
                                      (holding o2)))
                                  (and
                                    (= ?x1 r4)
                                    (holding o1)))))))
                        (or
                          (and
                            (at o3 r4)
                            (and
                              (not 
                                (and
                                  (= ?x0 r4)
                                  (holding o3)))
                              (or
                                (and
                                  (and
                                    (= ?x1 r4)
                                    (holding o1))
                                  (and
                                    (= ?x1 r4)
                                    (holding o2)))
                                (or
                                  (and
                                    (at o1 r4)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 r4)
                                          (holding o1)))
                                      (and
                                        (= ?x1 r4)
                                        (holding o2))))
                                  (and
                                    (at o2 r4)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 r4)
                                          (holding o2)))
                                      (and
                                        (= ?x1 r4)
                                        (holding o1))))))))
                          (and
                            (and
                              (at o1 r4)
                              (at o2 r4))
                            (and
                              (not 
                                (or
                                  (and
                                    (= ?x0 r4)
                                    (holding o1))
                                  (and
                                    (= ?x0 r4)
                                    (holding o2))))
                              (and
                                (= ?x1 r4)
                                (holding o3))))))))
                  (and
                    (and
                      (at o3 r4)
                      (and
                        (at o1 r4)
                        (at o2 r4)))
                    (and
                      (not 
                        (or
                          (and
                            (= ?x0 r4)
                            (holding o3))
                          (or
                            (and
                              (= ?x0 r4)
                              (holding o1))
                            (and
                              (= ?x0 r4)
                              (holding o2)))))
                      (and
                        (= ?x1 r4)
                        (holding o4))))))
              (and
                (and
                  (at o4 r4)
                  (and
                    (at o3 r4)
                    (and
                      (at o1 r4)
                      (at o2 r4))))
                (not 
                  (or
                    (and
                      (= ?x0 r4)
                      (holding o4))
                    (or
                      (and
                        (= ?x0 r4)
                        (holding o3))
                      (or
                        (and
                          (= ?x0 r4)
                          (holding o1))
                        (and
                          (= ?x0 r4)
                          (holding o2)))))))))
          (autstate_1_2))
        (when
          (and
            (autstate_1_2)
            (or
              (or
                (and
                  (and
                    (= ?x1 c1)
                    (holding o4))
                  (or
                    (and
                      (and
                        (= ?x1 c1)
                        (holding o3))
                      (or
                        (and
                          (and
                            (= ?x1 c1)
                            (holding o1))
                          (and
                            (= ?x1 c1)
                            (holding o2)))
                        (or
                          (and
                            (at o1 c1)
                            (and
                              (not 
                                (and
                                  (= ?x0 c1)
                                  (holding o1)))
                              (and
                                (= ?x1 c1)
                                (holding o2))))
                          (and
                            (at o2 c1)
                            (and
                              (not 
                                (and
                                  (= ?x0 c1)
                                  (holding o2)))
                              (and
                                (= ?x1 c1)
                                (holding o1)))))))
                    (or
                      (and
                        (at o3 c1)
                        (and
                          (not 
                            (and
                              (= ?x0 c1)
                              (holding o3)))
                          (or
                            (and
                              (and
                                (= ?x1 c1)
                                (holding o1))
                              (and
                                (= ?x1 c1)
                                (holding o2)))
                            (or
                              (and
                                (at o1 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding o1)))
                                  (and
                                    (= ?x1 c1)
                                    (holding o2))))
                              (and
                                (at o2 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding o2)))
                                  (and
                                    (= ?x1 c1)
                                    (holding o1))))))))
                      (and
                        (and
                          (at o1 c1)
                          (at o2 c1))
                        (and
                          (not 
                            (or
                              (and
                                (= ?x0 c1)
                                (holding o1))
                              (and
                                (= ?x0 c1)
                                (holding o2))))
                          (and
                            (= ?x1 c1)
                            (holding o3)))))))
                (or
                  (and
                    (at o4 c1)
                    (and
                      (not 
                        (and
                          (= ?x0 c1)
                          (holding o4)))
                      (or
                        (and
                          (and
                            (= ?x1 c1)
                            (holding o3))
                          (or
                            (and
                              (and
                                (= ?x1 c1)
                                (holding o1))
                              (and
                                (= ?x1 c1)
                                (holding o2)))
                            (or
                              (and
                                (at o1 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding o1)))
                                  (and
                                    (= ?x1 c1)
                                    (holding o2))))
                              (and
                                (at o2 c1)
                                (and
                                  (not 
                                    (and
                                      (= ?x0 c1)
                                      (holding o2)))
                                  (and
                                    (= ?x1 c1)
                                    (holding o1)))))))
                        (or
                          (and
                            (at o3 c1)
                            (and
                              (not 
                                (and
                                  (= ?x0 c1)
                                  (holding o3)))
                              (or
                                (and
                                  (and
                                    (= ?x1 c1)
                                    (holding o1))
                                  (and
                                    (= ?x1 c1)
                                    (holding o2)))
                                (or
                                  (and
                                    (at o1 c1)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 c1)
                                          (holding o1)))
                                      (and
                                        (= ?x1 c1)
                                        (holding o2))))
                                  (and
                                    (at o2 c1)
                                    (and
                                      (not 
                                        (and
                                          (= ?x0 c1)
                                          (holding o2)))
                                      (and
                                        (= ?x1 c1)
                                        (holding o1))))))))
                          (and
                            (and
                              (at o1 c1)
                              (at o2 c1))
                            (and
                              (not 
                                (or
                                  (and
                                    (= ?x0 c1)
                                    (holding o1))
                                  (and
                                    (= ?x0 c1)
                                    (holding o2))))
                              (and
                                (= ?x1 c1)
                                (holding o3))))))))
                  (and
                    (and
                      (at o3 c1)
                      (and
                        (at o1 c1)
                        (at o2 c1)))
                    (and
                      (not 
                        (or
                          (and
                            (= ?x0 c1)
                            (holding o3))
                          (or
                            (and
                              (= ?x0 c1)
                              (holding o1))
                            (and
                              (= ?x0 c1)
                              (holding o2)))))
                      (and
                        (= ?x1 c1)
                        (holding o4))))))
              (and
                (and
                  (at o4 c1)
                  (and
                    (at o3 c1)
                    (and
                      (at o1 c1)
                      (at o2 c1))))
                (not 
                  (or
                    (and
                      (= ?x0 c1)
                      (holding o4))
                    (or
                      (and
                        (= ?x0 c1)
                        (holding o3))
                      (or
                        (and
                          (= ?x0 c1)
                          (holding o1))
                        (and
                          (= ?x0 c1)
                          (holding o2)))))))))
          (autstate_1_1))
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (not 
              (at ?x2 ?x0))))

      )
    )
)