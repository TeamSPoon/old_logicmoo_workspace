(define (problem Run-Jack)
    (:domain roads)
  (:objects Jack Microsoft Rockwell KI)
  (:length (:serial 2) (:parallel 2))  
  (:goal (AND (at Jack Microsoft)))
  (:init (at jack Rockwell)
	 (road Rockwell KI) (road KI Microsoft)))

