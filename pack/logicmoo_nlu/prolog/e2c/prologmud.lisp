(set-agenda-error-mode :ignore)
(load "e2c/cogbot-init.lisp")
(LOAD-KE-TEXT-FILE #$CycAdministrator "e2c/e2c-new.ke" :agenda nil)
;;(LOAD-KE-TEXT-FILE #$CycAdministrator "e2c/e2c-new0.ke" :agenda nil)
(load "e2c/const-renamer.lisp")
(load "e2c/export.lisp")
(load "e2c/remove-instances.lisp")
;;(load "e2c/cogbot-ke.lisp")
;; (cdo ((*x* 0  (+ *x* 1))) ((= *x* (constant-count))) (fi-assert `(,(foc "plainCycConstant") ,(find-constant-by-internal-id *x*)) '#$UniversalVocabularyMt))
;;(set-dispatch-macro-character #\# #\$ #'FIND-RENAMED-CONSTANT-RMF)
;;(kif-warn (WITH-ANY-MT (with-all-mts (cdolist (trm22 (ALL-INSTANCES #$Function-Denotational)) (maybe-rename trm22)))))
;;(namet)
;;(namet2)

