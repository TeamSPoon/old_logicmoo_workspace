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
    (autstate_1_6)
    (autstate_1_5)
    (autstate_1_8)
    (autstate_1_9)
    (autstate_1_1)
    (autstate_1_7)
    (autstate_2_2)
    (autstate_2_6)
    (autstate_2_5)
    (autstate_2_8)
    (autstate_2_9)
    (autstate_2_1)
    (autstate_2_7)
    (autstate_3_2)
    (autstate_3_6)
    (autstate_3_5)
    (autstate_3_8)
    (autstate_3_9)
    (autstate_3_1)
    (autstate_3_7)
    (autstate_4_2)
    (autstate_4_6)
    (autstate_4_5)
    (autstate_4_8)
    (autstate_4_9)
    (autstate_4_1)
    (autstate_4_7)
    (autstate_5_2)
    (autstate_5_6)
    (autstate_5_5)
    (autstate_5_8)
    (autstate_5_9)
    (autstate_5_1)
    (autstate_5_7)
    (autstate_6_2)
    (autstate_6_1)
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
          (or
            (autstate_1_7)
            (or
              (and
                (autstate_1_5)
                (and
                  (closed d1)
                  (not 
                    (= ?x0 d1))))
              (autstate_1_2)))
          (autstate_1_6))
        (when
          (or
            (autstate_1_9)
            (autstate_1_8))
          (autstate_1_5))
        (when
          (autstate_1_9)
          (autstate_1_8))
        (when
          (or
            (and
              (autstate_1_6)
              (and
                (closed d1)
                (not 
                  (= ?x0 d1))))
            (autstate_1_2))
          (autstate_1_9))
        (when
          (or
            (and
              (autstate_1_6)
              (and
                (closed d1)
                (not 
                  (= ?x0 d1))))
            (autstate_1_2))
          (autstate_1_1))
        (when
          (and
            (autstate_1_9)
            (or
              (= ?x0 d1)
              (not 
                (closed d1))))
          (autstate_1_7))
        (when
          (or
            (autstate_2_7)
            (or
              (and
                (autstate_2_5)
                (and
                  (closed d12)
                  (not 
                    (= ?x0 d12))))
              (autstate_2_2)))
          (autstate_2_6))
        (when
          (or
            (autstate_2_9)
            (autstate_2_8))
          (autstate_2_5))
        (when
          (autstate_2_9)
          (autstate_2_8))
        (when
          (or
            (and
              (autstate_2_6)
              (and
                (closed d12)
                (not 
                  (= ?x0 d12))))
            (autstate_2_2))
          (autstate_2_9))
        (when
          (or
            (and
              (autstate_2_6)
              (and
                (closed d12)
                (not 
                  (= ?x0 d12))))
            (autstate_2_2))
          (autstate_2_1))
        (when
          (and
            (autstate_2_9)
            (or
              (= ?x0 d12)
              (not 
                (closed d12))))
          (autstate_2_7))
        (when
          (or
            (autstate_3_7)
            (or
              (and
                (autstate_3_5)
                (and
                  (closed d23)
                  (not 
                    (= ?x0 d23))))
              (autstate_3_2)))
          (autstate_3_6))
        (when
          (or
            (autstate_3_9)
            (autstate_3_8))
          (autstate_3_5))
        (when
          (autstate_3_9)
          (autstate_3_8))
        (when
          (or
            (and
              (autstate_3_6)
              (and
                (closed d23)
                (not 
                  (= ?x0 d23))))
            (autstate_3_2))
          (autstate_3_9))
        (when
          (or
            (and
              (autstate_3_6)
              (and
                (closed d23)
                (not 
                  (= ?x0 d23))))
            (autstate_3_2))
          (autstate_3_1))
        (when
          (and
            (autstate_3_9)
            (or
              (= ?x0 d23)
              (not 
                (closed d23))))
          (autstate_3_7))
        (when
          (or
            (autstate_4_7)
            (or
              (and
                (autstate_4_5)
                (and
                  (closed d34)
                  (not 
                    (= ?x0 d34))))
              (autstate_4_2)))
          (autstate_4_6))
        (when
          (or
            (autstate_4_9)
            (autstate_4_8))
          (autstate_4_5))
        (when
          (autstate_4_9)
          (autstate_4_8))
        (when
          (or
            (and
              (autstate_4_6)
              (and
                (closed d34)
                (not 
                  (= ?x0 d34))))
            (autstate_4_2))
          (autstate_4_9))
        (when
          (or
            (and
              (autstate_4_6)
              (and
                (closed d34)
                (not 
                  (= ?x0 d34))))
            (autstate_4_2))
          (autstate_4_1))
        (when
          (and
            (autstate_4_9)
            (or
              (= ?x0 d34)
              (not 
                (closed d34))))
          (autstate_4_7))
        (when
          (or
            (autstate_5_7)
            (or
              (and
                (autstate_5_5)
                (and
                  (closed d4)
                  (not 
                    (= ?x0 d4))))
              (autstate_5_2)))
          (autstate_5_6))
        (when
          (or
            (autstate_5_9)
            (autstate_5_8))
          (autstate_5_5))
        (when
          (autstate_5_9)
          (autstate_5_8))
        (when
          (or
            (and
              (autstate_5_6)
              (and
                (closed d4)
                (not 
                  (= ?x0 d4))))
            (autstate_5_2))
          (autstate_5_9))
        (when
          (or
            (and
              (autstate_5_6)
              (and
                (closed d4)
                (not 
                  (= ?x0 d4))))
            (autstate_5_2))
          (autstate_5_1))
        (when
          (and
            (autstate_5_9)
            (or
              (= ?x0 d4)
              (not 
                (closed d4))))
          (autstate_5_7))
        (when
          (and
            (autstate_6_2)
            (and
              (at robot c1)
              (at obj1 r4)))
          (autstate_6_1))
        (not 
          (closed ?x0))
        (when
          (and
            (not 
              (= ?x0 d1))
            (closed d1))
          (not 
            (autstate_1_2)))
        (when
          (and
            (not 
              (or
                (autstate_1_7)
                (or
                  (and
                    (autstate_1_5)
                    (and
                      (closed d1)
                      (not 
                        (= ?x0 d1))))
                  (autstate_1_2))))
            (or
              (not 
                (closed d1))
              (= ?x0 d1)))
          (not 
            (autstate_1_6)))
        (when
          (not 
            (or
              (autstate_1_9)
              (autstate_1_8)))
          (not 
            (autstate_1_5)))
        (when
          (not 
            (autstate_1_9))
          (not 
            (autstate_1_8)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (and
                  (closed d1)
                  (not 
                    (= ?x0 d1))))
              (autstate_1_2)))
          (not 
            (autstate_1_9)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (and
                  (closed d1)
                  (not 
                    (= ?x0 d1))))
              (autstate_1_2)))
          (not 
            (autstate_1_1)))
        (when
          (not 
            (and
              (autstate_1_9)
              (or
                (= ?x0 d1)
                (not 
                  (closed d1)))))
          (not 
            (autstate_1_7)))
        (when
          (and
            (not 
              (= ?x0 d12))
            (closed d12))
          (not 
            (autstate_2_2)))
        (when
          (and
            (not 
              (or
                (autstate_2_7)
                (or
                  (and
                    (autstate_2_5)
                    (and
                      (closed d12)
                      (not 
                        (= ?x0 d12))))
                  (autstate_2_2))))
            (or
              (not 
                (closed d12))
              (= ?x0 d12)))
          (not 
            (autstate_2_6)))
        (when
          (not 
            (or
              (autstate_2_9)
              (autstate_2_8)))
          (not 
            (autstate_2_5)))
        (when
          (not 
            (autstate_2_9))
          (not 
            (autstate_2_8)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (and
                  (closed d12)
                  (not 
                    (= ?x0 d12))))
              (autstate_2_2)))
          (not 
            (autstate_2_9)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (and
                  (closed d12)
                  (not 
                    (= ?x0 d12))))
              (autstate_2_2)))
          (not 
            (autstate_2_1)))
        (when
          (not 
            (and
              (autstate_2_9)
              (or
                (= ?x0 d12)
                (not 
                  (closed d12)))))
          (not 
            (autstate_2_7)))
        (when
          (and
            (not 
              (= ?x0 d23))
            (closed d23))
          (not 
            (autstate_3_2)))
        (when
          (and
            (not 
              (or
                (autstate_3_7)
                (or
                  (and
                    (autstate_3_5)
                    (and
                      (closed d23)
                      (not 
                        (= ?x0 d23))))
                  (autstate_3_2))))
            (or
              (not 
                (closed d23))
              (= ?x0 d23)))
          (not 
            (autstate_3_6)))
        (when
          (not 
            (or
              (autstate_3_9)
              (autstate_3_8)))
          (not 
            (autstate_3_5)))
        (when
          (not 
            (autstate_3_9))
          (not 
            (autstate_3_8)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (and
                  (closed d23)
                  (not 
                    (= ?x0 d23))))
              (autstate_3_2)))
          (not 
            (autstate_3_9)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (and
                  (closed d23)
                  (not 
                    (= ?x0 d23))))
              (autstate_3_2)))
          (not 
            (autstate_3_1)))
        (when
          (not 
            (and
              (autstate_3_9)
              (or
                (= ?x0 d23)
                (not 
                  (closed d23)))))
          (not 
            (autstate_3_7)))
        (when
          (and
            (not 
              (= ?x0 d34))
            (closed d34))
          (not 
            (autstate_4_2)))
        (when
          (and
            (not 
              (or
                (autstate_4_7)
                (or
                  (and
                    (autstate_4_5)
                    (and
                      (closed d34)
                      (not 
                        (= ?x0 d34))))
                  (autstate_4_2))))
            (or
              (not 
                (closed d34))
              (= ?x0 d34)))
          (not 
            (autstate_4_6)))
        (when
          (not 
            (or
              (autstate_4_9)
              (autstate_4_8)))
          (not 
            (autstate_4_5)))
        (when
          (not 
            (autstate_4_9))
          (not 
            (autstate_4_8)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (and
                  (closed d34)
                  (not 
                    (= ?x0 d34))))
              (autstate_4_2)))
          (not 
            (autstate_4_9)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (and
                  (closed d34)
                  (not 
                    (= ?x0 d34))))
              (autstate_4_2)))
          (not 
            (autstate_4_1)))
        (when
          (not 
            (and
              (autstate_4_9)
              (or
                (= ?x0 d34)
                (not 
                  (closed d34)))))
          (not 
            (autstate_4_7)))
        (when
          (and
            (not 
              (= ?x0 d4))
            (closed d4))
          (not 
            (autstate_5_2)))
        (when
          (and
            (not 
              (or
                (autstate_5_7)
                (or
                  (and
                    (autstate_5_5)
                    (and
                      (closed d4)
                      (not 
                        (= ?x0 d4))))
                  (autstate_5_2))))
            (or
              (not 
                (closed d4))
              (= ?x0 d4)))
          (not 
            (autstate_5_6)))
        (when
          (not 
            (or
              (autstate_5_9)
              (autstate_5_8)))
          (not 
            (autstate_5_5)))
        (when
          (not 
            (autstate_5_9))
          (not 
            (autstate_5_8)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (and
                  (closed d4)
                  (not 
                    (= ?x0 d4))))
              (autstate_5_2)))
          (not 
            (autstate_5_9)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (and
                  (closed d4)
                  (not 
                    (= ?x0 d4))))
              (autstate_5_2)))
          (not 
            (autstate_5_1)))
        (when
          (not 
            (and
              (autstate_5_9)
              (or
                (= ?x0 d4)
                (not 
                  (closed d4)))))
          (not 
            (autstate_5_7)))
        (when
          (not 
            (and
              (autstate_6_2)
              (and
                (at robot c1)
                (at obj1 r4))))
          (not 
            (autstate_6_1)))
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
          (or
            (autstate_1_7)
            (or
              (and
                (autstate_1_5)
                (or
                  (= ?x0 d1)
                  (closed d1)))
              (autstate_1_2)))
          (autstate_1_6))
        (when
          (or
            (autstate_1_9)
            (autstate_1_8))
          (autstate_1_5))
        (when
          (autstate_1_9)
          (autstate_1_8))
        (when
          (or
            (and
              (autstate_1_6)
              (or
                (= ?x0 d1)
                (closed d1)))
            (autstate_1_2))
          (autstate_1_9))
        (when
          (or
            (and
              (autstate_1_6)
              (or
                (= ?x0 d1)
                (closed d1)))
            (autstate_1_2))
          (autstate_1_1))
        (when
          (and
            (autstate_1_9)
            (and
              (not 
                (closed d1))
              (not 
                (= ?x0 d1))))
          (autstate_1_7))
        (when
          (or
            (autstate_2_7)
            (or
              (and
                (autstate_2_5)
                (or
                  (= ?x0 d12)
                  (closed d12)))
              (autstate_2_2)))
          (autstate_2_6))
        (when
          (or
            (autstate_2_9)
            (autstate_2_8))
          (autstate_2_5))
        (when
          (autstate_2_9)
          (autstate_2_8))
        (when
          (or
            (and
              (autstate_2_6)
              (or
                (= ?x0 d12)
                (closed d12)))
            (autstate_2_2))
          (autstate_2_9))
        (when
          (or
            (and
              (autstate_2_6)
              (or
                (= ?x0 d12)
                (closed d12)))
            (autstate_2_2))
          (autstate_2_1))
        (when
          (and
            (autstate_2_9)
            (and
              (not 
                (closed d12))
              (not 
                (= ?x0 d12))))
          (autstate_2_7))
        (when
          (or
            (autstate_3_7)
            (or
              (and
                (autstate_3_5)
                (or
                  (= ?x0 d23)
                  (closed d23)))
              (autstate_3_2)))
          (autstate_3_6))
        (when
          (or
            (autstate_3_9)
            (autstate_3_8))
          (autstate_3_5))
        (when
          (autstate_3_9)
          (autstate_3_8))
        (when
          (or
            (and
              (autstate_3_6)
              (or
                (= ?x0 d23)
                (closed d23)))
            (autstate_3_2))
          (autstate_3_9))
        (when
          (or
            (and
              (autstate_3_6)
              (or
                (= ?x0 d23)
                (closed d23)))
            (autstate_3_2))
          (autstate_3_1))
        (when
          (and
            (autstate_3_9)
            (and
              (not 
                (closed d23))
              (not 
                (= ?x0 d23))))
          (autstate_3_7))
        (when
          (or
            (autstate_4_7)
            (or
              (and
                (autstate_4_5)
                (or
                  (= ?x0 d34)
                  (closed d34)))
              (autstate_4_2)))
          (autstate_4_6))
        (when
          (or
            (autstate_4_9)
            (autstate_4_8))
          (autstate_4_5))
        (when
          (autstate_4_9)
          (autstate_4_8))
        (when
          (or
            (and
              (autstate_4_6)
              (or
                (= ?x0 d34)
                (closed d34)))
            (autstate_4_2))
          (autstate_4_9))
        (when
          (or
            (and
              (autstate_4_6)
              (or
                (= ?x0 d34)
                (closed d34)))
            (autstate_4_2))
          (autstate_4_1))
        (when
          (and
            (autstate_4_9)
            (and
              (not 
                (closed d34))
              (not 
                (= ?x0 d34))))
          (autstate_4_7))
        (when
          (or
            (autstate_5_7)
            (or
              (and
                (autstate_5_5)
                (or
                  (= ?x0 d4)
                  (closed d4)))
              (autstate_5_2)))
          (autstate_5_6))
        (when
          (or
            (autstate_5_9)
            (autstate_5_8))
          (autstate_5_5))
        (when
          (autstate_5_9)
          (autstate_5_8))
        (when
          (or
            (and
              (autstate_5_6)
              (or
                (= ?x0 d4)
                (closed d4)))
            (autstate_5_2))
          (autstate_5_9))
        (when
          (or
            (and
              (autstate_5_6)
              (or
                (= ?x0 d4)
                (closed d4)))
            (autstate_5_2))
          (autstate_5_1))
        (when
          (and
            (autstate_5_9)
            (and
              (not 
                (closed d4))
              (not 
                (= ?x0 d4))))
          (autstate_5_7))
        (when
          (and
            (autstate_6_2)
            (and
              (at robot c1)
              (at obj1 r4)))
          (autstate_6_1))
        (not 
          (opened ?x0))
        (when
          (or
            (closed d1)
            (= ?x0 d1))
          (not 
            (autstate_1_2)))
        (when
          (and
            (not 
              (or
                (autstate_1_7)
                (or
                  (and
                    (autstate_1_5)
                    (or
                      (= ?x0 d1)
                      (closed d1)))
                  (autstate_1_2))))
            (and
              (not 
                (= ?x0 d1))
              (not 
                (closed d1))))
          (not 
            (autstate_1_6)))
        (when
          (not 
            (or
              (autstate_1_9)
              (autstate_1_8)))
          (not 
            (autstate_1_5)))
        (when
          (not 
            (autstate_1_9))
          (not 
            (autstate_1_8)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (or
                  (= ?x0 d1)
                  (closed d1)))
              (autstate_1_2)))
          (not 
            (autstate_1_9)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (or
                  (= ?x0 d1)
                  (closed d1)))
              (autstate_1_2)))
          (not 
            (autstate_1_1)))
        (when
          (not 
            (and
              (autstate_1_9)
              (and
                (not 
                  (closed d1))
                (not 
                  (= ?x0 d1)))))
          (not 
            (autstate_1_7)))
        (when
          (or
            (closed d12)
            (= ?x0 d12))
          (not 
            (autstate_2_2)))
        (when
          (and
            (not 
              (or
                (autstate_2_7)
                (or
                  (and
                    (autstate_2_5)
                    (or
                      (= ?x0 d12)
                      (closed d12)))
                  (autstate_2_2))))
            (and
              (not 
                (= ?x0 d12))
              (not 
                (closed d12))))
          (not 
            (autstate_2_6)))
        (when
          (not 
            (or
              (autstate_2_9)
              (autstate_2_8)))
          (not 
            (autstate_2_5)))
        (when
          (not 
            (autstate_2_9))
          (not 
            (autstate_2_8)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (or
                  (= ?x0 d12)
                  (closed d12)))
              (autstate_2_2)))
          (not 
            (autstate_2_9)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (or
                  (= ?x0 d12)
                  (closed d12)))
              (autstate_2_2)))
          (not 
            (autstate_2_1)))
        (when
          (not 
            (and
              (autstate_2_9)
              (and
                (not 
                  (closed d12))
                (not 
                  (= ?x0 d12)))))
          (not 
            (autstate_2_7)))
        (when
          (or
            (closed d23)
            (= ?x0 d23))
          (not 
            (autstate_3_2)))
        (when
          (and
            (not 
              (or
                (autstate_3_7)
                (or
                  (and
                    (autstate_3_5)
                    (or
                      (= ?x0 d23)
                      (closed d23)))
                  (autstate_3_2))))
            (and
              (not 
                (= ?x0 d23))
              (not 
                (closed d23))))
          (not 
            (autstate_3_6)))
        (when
          (not 
            (or
              (autstate_3_9)
              (autstate_3_8)))
          (not 
            (autstate_3_5)))
        (when
          (not 
            (autstate_3_9))
          (not 
            (autstate_3_8)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (or
                  (= ?x0 d23)
                  (closed d23)))
              (autstate_3_2)))
          (not 
            (autstate_3_9)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (or
                  (= ?x0 d23)
                  (closed d23)))
              (autstate_3_2)))
          (not 
            (autstate_3_1)))
        (when
          (not 
            (and
              (autstate_3_9)
              (and
                (not 
                  (closed d23))
                (not 
                  (= ?x0 d23)))))
          (not 
            (autstate_3_7)))
        (when
          (or
            (closed d34)
            (= ?x0 d34))
          (not 
            (autstate_4_2)))
        (when
          (and
            (not 
              (or
                (autstate_4_7)
                (or
                  (and
                    (autstate_4_5)
                    (or
                      (= ?x0 d34)
                      (closed d34)))
                  (autstate_4_2))))
            (and
              (not 
                (= ?x0 d34))
              (not 
                (closed d34))))
          (not 
            (autstate_4_6)))
        (when
          (not 
            (or
              (autstate_4_9)
              (autstate_4_8)))
          (not 
            (autstate_4_5)))
        (when
          (not 
            (autstate_4_9))
          (not 
            (autstate_4_8)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (or
                  (= ?x0 d34)
                  (closed d34)))
              (autstate_4_2)))
          (not 
            (autstate_4_9)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (or
                  (= ?x0 d34)
                  (closed d34)))
              (autstate_4_2)))
          (not 
            (autstate_4_1)))
        (when
          (not 
            (and
              (autstate_4_9)
              (and
                (not 
                  (closed d34))
                (not 
                  (= ?x0 d34)))))
          (not 
            (autstate_4_7)))
        (when
          (or
            (closed d4)
            (= ?x0 d4))
          (not 
            (autstate_5_2)))
        (when
          (and
            (not 
              (or
                (autstate_5_7)
                (or
                  (and
                    (autstate_5_5)
                    (or
                      (= ?x0 d4)
                      (closed d4)))
                  (autstate_5_2))))
            (and
              (not 
                (= ?x0 d4))
              (not 
                (closed d4))))
          (not 
            (autstate_5_6)))
        (when
          (not 
            (or
              (autstate_5_9)
              (autstate_5_8)))
          (not 
            (autstate_5_5)))
        (when
          (not 
            (autstate_5_9))
          (not 
            (autstate_5_8)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (or
                  (= ?x0 d4)
                  (closed d4)))
              (autstate_5_2)))
          (not 
            (autstate_5_9)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (or
                  (= ?x0 d4)
                  (closed d4)))
              (autstate_5_2)))
          (not 
            (autstate_5_1)))
        (when
          (not 
            (and
              (autstate_5_9)
              (and
                (not 
                  (closed d4))
                (not 
                  (= ?x0 d4)))))
          (not 
            (autstate_5_7)))
        (when
          (not 
            (and
              (autstate_6_2)
              (and
                (at robot c1)
                (at obj1 r4))))
          (not 
            (autstate_6_1)))
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
          (or
            (autstate_1_7)
            (or
              (and
                (autstate_1_5)
                (closed d1))
              (autstate_1_2)))
          (autstate_1_6))
        (when
          (or
            (autstate_1_9)
            (autstate_1_8))
          (autstate_1_5))
        (when
          (autstate_1_9)
          (autstate_1_8))
        (when
          (or
            (and
              (autstate_1_6)
              (closed d1))
            (autstate_1_2))
          (autstate_1_9))
        (when
          (or
            (and
              (autstate_1_6)
              (closed d1))
            (autstate_1_2))
          (autstate_1_1))
        (when
          (and
            (autstate_1_9)
            (not 
              (closed d1)))
          (autstate_1_7))
        (when
          (or
            (autstate_2_7)
            (or
              (and
                (autstate_2_5)
                (closed d12))
              (autstate_2_2)))
          (autstate_2_6))
        (when
          (or
            (autstate_2_9)
            (autstate_2_8))
          (autstate_2_5))
        (when
          (autstate_2_9)
          (autstate_2_8))
        (when
          (or
            (and
              (autstate_2_6)
              (closed d12))
            (autstate_2_2))
          (autstate_2_9))
        (when
          (or
            (and
              (autstate_2_6)
              (closed d12))
            (autstate_2_2))
          (autstate_2_1))
        (when
          (and
            (autstate_2_9)
            (not 
              (closed d12)))
          (autstate_2_7))
        (when
          (or
            (autstate_3_7)
            (or
              (and
                (autstate_3_5)
                (closed d23))
              (autstate_3_2)))
          (autstate_3_6))
        (when
          (or
            (autstate_3_9)
            (autstate_3_8))
          (autstate_3_5))
        (when
          (autstate_3_9)
          (autstate_3_8))
        (when
          (or
            (and
              (autstate_3_6)
              (closed d23))
            (autstate_3_2))
          (autstate_3_9))
        (when
          (or
            (and
              (autstate_3_6)
              (closed d23))
            (autstate_3_2))
          (autstate_3_1))
        (when
          (and
            (autstate_3_9)
            (not 
              (closed d23)))
          (autstate_3_7))
        (when
          (or
            (autstate_4_7)
            (or
              (and
                (autstate_4_5)
                (closed d34))
              (autstate_4_2)))
          (autstate_4_6))
        (when
          (or
            (autstate_4_9)
            (autstate_4_8))
          (autstate_4_5))
        (when
          (autstate_4_9)
          (autstate_4_8))
        (when
          (or
            (and
              (autstate_4_6)
              (closed d34))
            (autstate_4_2))
          (autstate_4_9))
        (when
          (or
            (and
              (autstate_4_6)
              (closed d34))
            (autstate_4_2))
          (autstate_4_1))
        (when
          (and
            (autstate_4_9)
            (not 
              (closed d34)))
          (autstate_4_7))
        (when
          (or
            (autstate_5_7)
            (or
              (and
                (autstate_5_5)
                (closed d4))
              (autstate_5_2)))
          (autstate_5_6))
        (when
          (or
            (autstate_5_9)
            (autstate_5_8))
          (autstate_5_5))
        (when
          (autstate_5_9)
          (autstate_5_8))
        (when
          (or
            (and
              (autstate_5_6)
              (closed d4))
            (autstate_5_2))
          (autstate_5_9))
        (when
          (or
            (and
              (autstate_5_6)
              (closed d4))
            (autstate_5_2))
          (autstate_5_1))
        (when
          (and
            (autstate_5_9)
            (not 
              (closed d4)))
          (autstate_5_7))
        (when
          (and
            (autstate_6_2)
            (and
              (at robot c1)
              (at obj1 r4)))
          (autstate_6_1))
        (not 
          (handempty))
        (when
          (closed d1)
          (not 
            (autstate_1_2)))
        (when
          (and
            (not 
              (or
                (autstate_1_7)
                (or
                  (and
                    (autstate_1_5)
                    (closed d1))
                  (autstate_1_2))))
            (not 
              (closed d1)))
          (not 
            (autstate_1_6)))
        (when
          (not 
            (or
              (autstate_1_9)
              (autstate_1_8)))
          (not 
            (autstate_1_5)))
        (when
          (not 
            (autstate_1_9))
          (not 
            (autstate_1_8)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (closed d1))
              (autstate_1_2)))
          (not 
            (autstate_1_9)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (closed d1))
              (autstate_1_2)))
          (not 
            (autstate_1_1)))
        (when
          (not 
            (and
              (autstate_1_9)
              (not 
                (closed d1))))
          (not 
            (autstate_1_7)))
        (when
          (closed d12)
          (not 
            (autstate_2_2)))
        (when
          (and
            (not 
              (or
                (autstate_2_7)
                (or
                  (and
                    (autstate_2_5)
                    (closed d12))
                  (autstate_2_2))))
            (not 
              (closed d12)))
          (not 
            (autstate_2_6)))
        (when
          (not 
            (or
              (autstate_2_9)
              (autstate_2_8)))
          (not 
            (autstate_2_5)))
        (when
          (not 
            (autstate_2_9))
          (not 
            (autstate_2_8)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (closed d12))
              (autstate_2_2)))
          (not 
            (autstate_2_9)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (closed d12))
              (autstate_2_2)))
          (not 
            (autstate_2_1)))
        (when
          (not 
            (and
              (autstate_2_9)
              (not 
                (closed d12))))
          (not 
            (autstate_2_7)))
        (when
          (closed d23)
          (not 
            (autstate_3_2)))
        (when
          (and
            (not 
              (or
                (autstate_3_7)
                (or
                  (and
                    (autstate_3_5)
                    (closed d23))
                  (autstate_3_2))))
            (not 
              (closed d23)))
          (not 
            (autstate_3_6)))
        (when
          (not 
            (or
              (autstate_3_9)
              (autstate_3_8)))
          (not 
            (autstate_3_5)))
        (when
          (not 
            (autstate_3_9))
          (not 
            (autstate_3_8)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (closed d23))
              (autstate_3_2)))
          (not 
            (autstate_3_9)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (closed d23))
              (autstate_3_2)))
          (not 
            (autstate_3_1)))
        (when
          (not 
            (and
              (autstate_3_9)
              (not 
                (closed d23))))
          (not 
            (autstate_3_7)))
        (when
          (closed d34)
          (not 
            (autstate_4_2)))
        (when
          (and
            (not 
              (or
                (autstate_4_7)
                (or
                  (and
                    (autstate_4_5)
                    (closed d34))
                  (autstate_4_2))))
            (not 
              (closed d34)))
          (not 
            (autstate_4_6)))
        (when
          (not 
            (or
              (autstate_4_9)
              (autstate_4_8)))
          (not 
            (autstate_4_5)))
        (when
          (not 
            (autstate_4_9))
          (not 
            (autstate_4_8)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (closed d34))
              (autstate_4_2)))
          (not 
            (autstate_4_9)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (closed d34))
              (autstate_4_2)))
          (not 
            (autstate_4_1)))
        (when
          (not 
            (and
              (autstate_4_9)
              (not 
                (closed d34))))
          (not 
            (autstate_4_7)))
        (when
          (closed d4)
          (not 
            (autstate_5_2)))
        (when
          (and
            (not 
              (or
                (autstate_5_7)
                (or
                  (and
                    (autstate_5_5)
                    (closed d4))
                  (autstate_5_2))))
            (not 
              (closed d4)))
          (not 
            (autstate_5_6)))
        (when
          (not 
            (or
              (autstate_5_9)
              (autstate_5_8)))
          (not 
            (autstate_5_5)))
        (when
          (not 
            (autstate_5_9))
          (not 
            (autstate_5_8)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (closed d4))
              (autstate_5_2)))
          (not 
            (autstate_5_9)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (closed d4))
              (autstate_5_2)))
          (not 
            (autstate_5_1)))
        (when
          (not 
            (and
              (autstate_5_9)
              (not 
                (closed d4))))
          (not 
            (autstate_5_7)))
        (when
          (not 
            (and
              (autstate_6_2)
              (and
                (at robot c1)
                (at obj1 r4))))
          (not 
            (autstate_6_1)))
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
          (or
            (autstate_1_7)
            (or
              (and
                (autstate_1_5)
                (closed d1))
              (autstate_1_2)))
          (autstate_1_6))
        (when
          (or
            (autstate_1_9)
            (autstate_1_8))
          (autstate_1_5))
        (when
          (autstate_1_9)
          (autstate_1_8))
        (when
          (or
            (and
              (autstate_1_6)
              (closed d1))
            (autstate_1_2))
          (autstate_1_9))
        (when
          (or
            (and
              (autstate_1_6)
              (closed d1))
            (autstate_1_2))
          (autstate_1_1))
        (when
          (and
            (autstate_1_9)
            (not 
              (closed d1)))
          (autstate_1_7))
        (when
          (or
            (autstate_2_7)
            (or
              (and
                (autstate_2_5)
                (closed d12))
              (autstate_2_2)))
          (autstate_2_6))
        (when
          (or
            (autstate_2_9)
            (autstate_2_8))
          (autstate_2_5))
        (when
          (autstate_2_9)
          (autstate_2_8))
        (when
          (or
            (and
              (autstate_2_6)
              (closed d12))
            (autstate_2_2))
          (autstate_2_9))
        (when
          (or
            (and
              (autstate_2_6)
              (closed d12))
            (autstate_2_2))
          (autstate_2_1))
        (when
          (and
            (autstate_2_9)
            (not 
              (closed d12)))
          (autstate_2_7))
        (when
          (or
            (autstate_3_7)
            (or
              (and
                (autstate_3_5)
                (closed d23))
              (autstate_3_2)))
          (autstate_3_6))
        (when
          (or
            (autstate_3_9)
            (autstate_3_8))
          (autstate_3_5))
        (when
          (autstate_3_9)
          (autstate_3_8))
        (when
          (or
            (and
              (autstate_3_6)
              (closed d23))
            (autstate_3_2))
          (autstate_3_9))
        (when
          (or
            (and
              (autstate_3_6)
              (closed d23))
            (autstate_3_2))
          (autstate_3_1))
        (when
          (and
            (autstate_3_9)
            (not 
              (closed d23)))
          (autstate_3_7))
        (when
          (or
            (autstate_4_7)
            (or
              (and
                (autstate_4_5)
                (closed d34))
              (autstate_4_2)))
          (autstate_4_6))
        (when
          (or
            (autstate_4_9)
            (autstate_4_8))
          (autstate_4_5))
        (when
          (autstate_4_9)
          (autstate_4_8))
        (when
          (or
            (and
              (autstate_4_6)
              (closed d34))
            (autstate_4_2))
          (autstate_4_9))
        (when
          (or
            (and
              (autstate_4_6)
              (closed d34))
            (autstate_4_2))
          (autstate_4_1))
        (when
          (and
            (autstate_4_9)
            (not 
              (closed d34)))
          (autstate_4_7))
        (when
          (or
            (autstate_5_7)
            (or
              (and
                (autstate_5_5)
                (closed d4))
              (autstate_5_2)))
          (autstate_5_6))
        (when
          (or
            (autstate_5_9)
            (autstate_5_8))
          (autstate_5_5))
        (when
          (autstate_5_9)
          (autstate_5_8))
        (when
          (or
            (and
              (autstate_5_6)
              (closed d4))
            (autstate_5_2))
          (autstate_5_9))
        (when
          (or
            (and
              (autstate_5_6)
              (closed d4))
            (autstate_5_2))
          (autstate_5_1))
        (when
          (and
            (autstate_5_9)
            (not 
              (closed d4)))
          (autstate_5_7))
        (when
          (and
            (autstate_6_2)
            (and
              (at robot c1)
              (at obj1 r4)))
          (autstate_6_1))
        (not 
          (holding ?x0))
        (when
          (closed d1)
          (not 
            (autstate_1_2)))
        (when
          (and
            (not 
              (or
                (autstate_1_7)
                (or
                  (and
                    (autstate_1_5)
                    (closed d1))
                  (autstate_1_2))))
            (not 
              (closed d1)))
          (not 
            (autstate_1_6)))
        (when
          (not 
            (or
              (autstate_1_9)
              (autstate_1_8)))
          (not 
            (autstate_1_5)))
        (when
          (not 
            (autstate_1_9))
          (not 
            (autstate_1_8)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (closed d1))
              (autstate_1_2)))
          (not 
            (autstate_1_9)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (closed d1))
              (autstate_1_2)))
          (not 
            (autstate_1_1)))
        (when
          (not 
            (and
              (autstate_1_9)
              (not 
                (closed d1))))
          (not 
            (autstate_1_7)))
        (when
          (closed d12)
          (not 
            (autstate_2_2)))
        (when
          (and
            (not 
              (or
                (autstate_2_7)
                (or
                  (and
                    (autstate_2_5)
                    (closed d12))
                  (autstate_2_2))))
            (not 
              (closed d12)))
          (not 
            (autstate_2_6)))
        (when
          (not 
            (or
              (autstate_2_9)
              (autstate_2_8)))
          (not 
            (autstate_2_5)))
        (when
          (not 
            (autstate_2_9))
          (not 
            (autstate_2_8)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (closed d12))
              (autstate_2_2)))
          (not 
            (autstate_2_9)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (closed d12))
              (autstate_2_2)))
          (not 
            (autstate_2_1)))
        (when
          (not 
            (and
              (autstate_2_9)
              (not 
                (closed d12))))
          (not 
            (autstate_2_7)))
        (when
          (closed d23)
          (not 
            (autstate_3_2)))
        (when
          (and
            (not 
              (or
                (autstate_3_7)
                (or
                  (and
                    (autstate_3_5)
                    (closed d23))
                  (autstate_3_2))))
            (not 
              (closed d23)))
          (not 
            (autstate_3_6)))
        (when
          (not 
            (or
              (autstate_3_9)
              (autstate_3_8)))
          (not 
            (autstate_3_5)))
        (when
          (not 
            (autstate_3_9))
          (not 
            (autstate_3_8)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (closed d23))
              (autstate_3_2)))
          (not 
            (autstate_3_9)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (closed d23))
              (autstate_3_2)))
          (not 
            (autstate_3_1)))
        (when
          (not 
            (and
              (autstate_3_9)
              (not 
                (closed d23))))
          (not 
            (autstate_3_7)))
        (when
          (closed d34)
          (not 
            (autstate_4_2)))
        (when
          (and
            (not 
              (or
                (autstate_4_7)
                (or
                  (and
                    (autstate_4_5)
                    (closed d34))
                  (autstate_4_2))))
            (not 
              (closed d34)))
          (not 
            (autstate_4_6)))
        (when
          (not 
            (or
              (autstate_4_9)
              (autstate_4_8)))
          (not 
            (autstate_4_5)))
        (when
          (not 
            (autstate_4_9))
          (not 
            (autstate_4_8)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (closed d34))
              (autstate_4_2)))
          (not 
            (autstate_4_9)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (closed d34))
              (autstate_4_2)))
          (not 
            (autstate_4_1)))
        (when
          (not 
            (and
              (autstate_4_9)
              (not 
                (closed d34))))
          (not 
            (autstate_4_7)))
        (when
          (closed d4)
          (not 
            (autstate_5_2)))
        (when
          (and
            (not 
              (or
                (autstate_5_7)
                (or
                  (and
                    (autstate_5_5)
                    (closed d4))
                  (autstate_5_2))))
            (not 
              (closed d4)))
          (not 
            (autstate_5_6)))
        (when
          (not 
            (or
              (autstate_5_9)
              (autstate_5_8)))
          (not 
            (autstate_5_5)))
        (when
          (not 
            (autstate_5_9))
          (not 
            (autstate_5_8)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (closed d4))
              (autstate_5_2)))
          (not 
            (autstate_5_9)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (closed d4))
              (autstate_5_2)))
          (not 
            (autstate_5_1)))
        (when
          (not 
            (and
              (autstate_5_9)
              (not 
                (closed d4))))
          (not 
            (autstate_5_7)))
        (when
          (not 
            (and
              (autstate_6_2)
              (and
                (at robot c1)
                (at obj1 r4))))
          (not 
            (autstate_6_1)))
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
          (or
            (autstate_1_7)
            (or
              (and
                (autstate_1_5)
                (closed d1))
              (autstate_1_2)))
          (autstate_1_6))
        (when
          (or
            (autstate_1_9)
            (autstate_1_8))
          (autstate_1_5))
        (when
          (autstate_1_9)
          (autstate_1_8))
        (when
          (or
            (and
              (autstate_1_6)
              (closed d1))
            (autstate_1_2))
          (autstate_1_9))
        (when
          (or
            (and
              (autstate_1_6)
              (closed d1))
            (autstate_1_2))
          (autstate_1_1))
        (when
          (and
            (autstate_1_9)
            (not 
              (closed d1)))
          (autstate_1_7))
        (when
          (or
            (autstate_2_7)
            (or
              (and
                (autstate_2_5)
                (closed d12))
              (autstate_2_2)))
          (autstate_2_6))
        (when
          (or
            (autstate_2_9)
            (autstate_2_8))
          (autstate_2_5))
        (when
          (autstate_2_9)
          (autstate_2_8))
        (when
          (or
            (and
              (autstate_2_6)
              (closed d12))
            (autstate_2_2))
          (autstate_2_9))
        (when
          (or
            (and
              (autstate_2_6)
              (closed d12))
            (autstate_2_2))
          (autstate_2_1))
        (when
          (and
            (autstate_2_9)
            (not 
              (closed d12)))
          (autstate_2_7))
        (when
          (or
            (autstate_3_7)
            (or
              (and
                (autstate_3_5)
                (closed d23))
              (autstate_3_2)))
          (autstate_3_6))
        (when
          (or
            (autstate_3_9)
            (autstate_3_8))
          (autstate_3_5))
        (when
          (autstate_3_9)
          (autstate_3_8))
        (when
          (or
            (and
              (autstate_3_6)
              (closed d23))
            (autstate_3_2))
          (autstate_3_9))
        (when
          (or
            (and
              (autstate_3_6)
              (closed d23))
            (autstate_3_2))
          (autstate_3_1))
        (when
          (and
            (autstate_3_9)
            (not 
              (closed d23)))
          (autstate_3_7))
        (when
          (or
            (autstate_4_7)
            (or
              (and
                (autstate_4_5)
                (closed d34))
              (autstate_4_2)))
          (autstate_4_6))
        (when
          (or
            (autstate_4_9)
            (autstate_4_8))
          (autstate_4_5))
        (when
          (autstate_4_9)
          (autstate_4_8))
        (when
          (or
            (and
              (autstate_4_6)
              (closed d34))
            (autstate_4_2))
          (autstate_4_9))
        (when
          (or
            (and
              (autstate_4_6)
              (closed d34))
            (autstate_4_2))
          (autstate_4_1))
        (when
          (and
            (autstate_4_9)
            (not 
              (closed d34)))
          (autstate_4_7))
        (when
          (or
            (autstate_5_7)
            (or
              (and
                (autstate_5_5)
                (closed d4))
              (autstate_5_2)))
          (autstate_5_6))
        (when
          (or
            (autstate_5_9)
            (autstate_5_8))
          (autstate_5_5))
        (when
          (autstate_5_9)
          (autstate_5_8))
        (when
          (or
            (and
              (autstate_5_6)
              (closed d4))
            (autstate_5_2))
          (autstate_5_9))
        (when
          (or
            (and
              (autstate_5_6)
              (closed d4))
            (autstate_5_2))
          (autstate_5_1))
        (when
          (and
            (autstate_5_9)
            (not 
              (closed d4)))
          (autstate_5_7))
        (when
          (and
            (autstate_6_2)
            (or
              (or
                (and
                  (= ?x1 c1)
                  (and
                    (= ?x1 r4)
                    (holding obj1)))
                (or
                  (and
                    (at robot c1)
                    (and
                      (not 
                        (= ?x0 c1))
                      (and
                        (= ?x1 r4)
                        (holding obj1))))
                  (and
                    (at obj1 r4)
                    (and
                      (not 
                        (and
                          (= ?x0 r4)
                          (holding obj1)))
                      (= ?x1 c1)))))
              (and
                (and
                  (at robot c1)
                  (at obj1 r4))
                (not 
                  (or
                    (= ?x0 c1)
                    (and
                      (= ?x0 r4)
                      (holding obj1)))))))
          (autstate_6_1))
        (forall (?x2)
          (when
            (or
              (= ?x2 robot)
              (holding ?x2))
            (not 
              (at ?x2 ?x0))))

        (when
          (closed d1)
          (not 
            (autstate_1_2)))
        (when
          (and
            (not 
              (or
                (autstate_1_7)
                (or
                  (and
                    (autstate_1_5)
                    (closed d1))
                  (autstate_1_2))))
            (not 
              (closed d1)))
          (not 
            (autstate_1_6)))
        (when
          (not 
            (or
              (autstate_1_9)
              (autstate_1_8)))
          (not 
            (autstate_1_5)))
        (when
          (not 
            (autstate_1_9))
          (not 
            (autstate_1_8)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (closed d1))
              (autstate_1_2)))
          (not 
            (autstate_1_9)))
        (when
          (not 
            (or
              (and
                (autstate_1_6)
                (closed d1))
              (autstate_1_2)))
          (not 
            (autstate_1_1)))
        (when
          (not 
            (and
              (autstate_1_9)
              (not 
                (closed d1))))
          (not 
            (autstate_1_7)))
        (when
          (closed d12)
          (not 
            (autstate_2_2)))
        (when
          (and
            (not 
              (or
                (autstate_2_7)
                (or
                  (and
                    (autstate_2_5)
                    (closed d12))
                  (autstate_2_2))))
            (not 
              (closed d12)))
          (not 
            (autstate_2_6)))
        (when
          (not 
            (or
              (autstate_2_9)
              (autstate_2_8)))
          (not 
            (autstate_2_5)))
        (when
          (not 
            (autstate_2_9))
          (not 
            (autstate_2_8)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (closed d12))
              (autstate_2_2)))
          (not 
            (autstate_2_9)))
        (when
          (not 
            (or
              (and
                (autstate_2_6)
                (closed d12))
              (autstate_2_2)))
          (not 
            (autstate_2_1)))
        (when
          (not 
            (and
              (autstate_2_9)
              (not 
                (closed d12))))
          (not 
            (autstate_2_7)))
        (when
          (closed d23)
          (not 
            (autstate_3_2)))
        (when
          (and
            (not 
              (or
                (autstate_3_7)
                (or
                  (and
                    (autstate_3_5)
                    (closed d23))
                  (autstate_3_2))))
            (not 
              (closed d23)))
          (not 
            (autstate_3_6)))
        (when
          (not 
            (or
              (autstate_3_9)
              (autstate_3_8)))
          (not 
            (autstate_3_5)))
        (when
          (not 
            (autstate_3_9))
          (not 
            (autstate_3_8)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (closed d23))
              (autstate_3_2)))
          (not 
            (autstate_3_9)))
        (when
          (not 
            (or
              (and
                (autstate_3_6)
                (closed d23))
              (autstate_3_2)))
          (not 
            (autstate_3_1)))
        (when
          (not 
            (and
              (autstate_3_9)
              (not 
                (closed d23))))
          (not 
            (autstate_3_7)))
        (when
          (closed d34)
          (not 
            (autstate_4_2)))
        (when
          (and
            (not 
              (or
                (autstate_4_7)
                (or
                  (and
                    (autstate_4_5)
                    (closed d34))
                  (autstate_4_2))))
            (not 
              (closed d34)))
          (not 
            (autstate_4_6)))
        (when
          (not 
            (or
              (autstate_4_9)
              (autstate_4_8)))
          (not 
            (autstate_4_5)))
        (when
          (not 
            (autstate_4_9))
          (not 
            (autstate_4_8)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (closed d34))
              (autstate_4_2)))
          (not 
            (autstate_4_9)))
        (when
          (not 
            (or
              (and
                (autstate_4_6)
                (closed d34))
              (autstate_4_2)))
          (not 
            (autstate_4_1)))
        (when
          (not 
            (and
              (autstate_4_9)
              (not 
                (closed d34))))
          (not 
            (autstate_4_7)))
        (when
          (closed d4)
          (not 
            (autstate_5_2)))
        (when
          (and
            (not 
              (or
                (autstate_5_7)
                (or
                  (and
                    (autstate_5_5)
                    (closed d4))
                  (autstate_5_2))))
            (not 
              (closed d4)))
          (not 
            (autstate_5_6)))
        (when
          (not 
            (or
              (autstate_5_9)
              (autstate_5_8)))
          (not 
            (autstate_5_5)))
        (when
          (not 
            (autstate_5_9))
          (not 
            (autstate_5_8)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (closed d4))
              (autstate_5_2)))
          (not 
            (autstate_5_9)))
        (when
          (not 
            (or
              (and
                (autstate_5_6)
                (closed d4))
              (autstate_5_2)))
          (not 
            (autstate_5_1)))
        (when
          (not 
            (and
              (autstate_5_9)
              (not 
                (closed d4))))
          (not 
            (autstate_5_7)))
        (when
          (not 
            (and
              (autstate_6_2)
              (or
                (or
                  (and
                    (= ?x1 c1)
                    (and
                      (= ?x1 r4)
                      (holding obj1)))
                  (or
                    (and
                      (at robot c1)
                      (and
                        (not 
                          (= ?x0 c1))
                        (and
                          (= ?x1 r4)
                          (holding obj1))))
                    (and
                      (at obj1 r4)
                      (and
                        (not 
                          (and
                            (= ?x0 r4)
                            (holding obj1)))
                        (= ?x1 c1)))))
                (and
                  (and
                    (at robot c1)
                    (at obj1 r4))
                  (not 
                    (or
                      (= ?x0 c1)
                      (and
                        (= ?x0 r4)
                        (holding obj1))))))))
          (not 
            (autstate_6_1)))
      )
    )
)