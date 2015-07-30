(load-file "zeonworld.tlp")
(load-file "zeonproblems.tlp")
(set-initial-world (init1))
(set-search-strategy breadth-first)

(set-goal (and (goal1)  (control-useplane) (control-only1) (control-fuel)))
(plan) 

(set-goal (and (goal2) (control-useplane) (control-only1) (control-fuel)))
(plan)

(set-goal (and (goal3) (control-useplane) (control-only1) (control-fuel)))
(plan) ;no

(set-goal (and (goal4) (control-useplane) (control-only1) (control-fuel)))
(plan) ;no

(set-goal (and (goal5) (control-useplane) (control-only12) (control-fuel)))
(plan) ;no

(set-goal (and (goal6) (control-useplane) (control-only12) (control-fuel)))
(plan) ;no

(set-goal (and (goal7) (control-useplane) (control-only12) (control-fuel)))
(plan) ;no

(set-goal (and (goal8) (control-only1) (control-fuel)))
(plan) ;yes

(set-goal (and (goal9) (control-only1) (control-fuel)))
(plan) ;yes

(set-goal (and (goal10) (control-only1) (control-fuel)))
(plan) ;yes

(set-goal (and (goal11) (control-only1) (control-fuel)))
(plan) ;yes

(set-goal (and (goal11a) (control-only13) (control-fuel)))
(plan) ;no


(set-goal (and (goal12) (control-only1) (control-fuel)))
(plan) ;no


(set-goal (and (goal12a) (control-only13) (control-fuel)))
(plan) ;no

(set-goal (and (goal13) (control-only1) (control-fuel)))
(plan) ;no


(set-goal (and (goal13a) (control-only13) (control-fuel)))
(plan) ;no

(set-goal (and (goal14) (control-only1) (control-fuel)))
(plan) ;no

(set-goal (and (goal14a) (control-only1) (control-fuel)))
(plan) ;no

(set-goal (and (goal14b) (control-only1) (control-fuel)))
(plan) ;no

(set-goal (and (goal14c) (control-only13) (control-fuel)))
(plan) ;no

(set-goal (and (goal15) (control-only12) (control-fuel)))
(plan) ;no


(set-goal (and (goal15a) (control-only123) (control-fuel)))
(plan) ;no


(set-goal (and (goal15b) (control-only123) (control-fuel)))
(plan) ;no


(set-goal (and (goal16) (control-only12) (control-fuel)))
(plan) ;no


(set-goal (and (goal17) (control-only1) (control-fuel)))
(plan) ;no


(set-goal (and (goal18) (control-only12) (control-fuel)))
(plan) ;no

(set-goal (and (goal18a) (control-only1) (control-fuel)))
(plan) ;no

(set-goal (and (goal19) (control-fuel)))
(plan)

