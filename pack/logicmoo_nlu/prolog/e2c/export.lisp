(define FORCE-PRINT (string) (print string) (force-output))



;;;; use (load "cynd/osim.lisp")

(force-print "loading export.lisp...")

(define plw-str (p)
  (princ p *file-output*))
  
(define plw-nl () (terpri *file-output*))

(define prolog-need-quote (str) 
 (ret t) 
 (punless (equal (SUBSTITUTE  #\_ #\- str) str)
   (ret t)) (clet ((ch (char str 0)))
    (pwhen (member ch '(#\: #\' #\` #\#) #'char=) (ret nil))
     (ret (cor (cnot (lower-case-p ch)) (find #\: str)))))

(define plw-list (trm0 &optional (begstr "") (endstr "") (nullstr "")) 
 (pwhen (null trm0 ) (ret (progn (plw-str begstr) (plw-str endstr))))
 (pwhen (atom trm0 ) (ret (plw-term trm0)))
 (pwhen (null (cdr trm0)) (ret (progn (plw-str begstr) (plw-term (car trm0)) (plw-str endstr))))
 (pwhen (cnot (consp (cdr trm0))) 
   (ret (progn (plw-str begstr) (plw-term (car trm0)) (plw-str " |/**/ ") (plw-term (cdr trm0)) (plw-str endstr))))
 (ret
  (progn 
   (plw-str begstr) 
   (plw-term (car trm0)) 
   (plw-cdr (cdr trm0) "," "|" nullstr)
   (plw-str endstr))))

(define plw-cdr (trm &optional (commastr ",")(barstr "|") (nullstr "")) 
 (pcond
 ((null trm) (ret (plw-str nullstr)))
 ((cnot (consp trm)) (ret (progn (plw-str barstr)(plw-term trm))))
 ((null (cdr trm)) (ret (progn (plw-str commastr) (plw-term (car trm)))))
 ((cnot (consp (cdr trm)))
    (ret (progn (plw-str commastr) (plw-term (car trm)) (plw-str barstr) (plw-term (cdr trm)))))
  (t (ret (progn 
             (plw-str commastr) 
	     (plw-term (car trm))
             (plw-cdr (cdr trm) commastr barstr nullstr))))))

(define plw-atom (trm)
 (pwhen (constant-p trm) (ret (plw-atom (constant-prolog-name trm))))
 (pwhen (stringp trm) 
  (pwhen (prolog-need-quote trm) (ret (progn (plw-str "'") (plw-str trm) (plw-str "'"))))
  (ret (progn (plw-str trm))))
 (throw trm))

(define simple-name (trm)
 (pwhen (constant-p trm) (ret (constant-name trm)))
 (pwhen (stringp trm) (ret term))
 (ret (string trm)))
 
(define plw-naut (str trm) 
 (pwhen (cnot (consp trm)) (ret (progn (princ str) (princ "0(") (plw-term trm) (princ ")"))))
  (clet ((pred (elt trm 0)) (cdrtrm (cdr trm)) (*VA* (ISA? pred #$VariableArityRelation)))
 
 (pwhen (function? pred)
  (pwhen (ISA? pred #$UnreifiableFunction)
	  (ret (progn (plw-str str) (plw-str "U(") (plw-term pred) (plw-argz *va* cdrtrm) (plw-str ")" ))))
  (pwhen (ISA? pred #$ReifiableFunction)
	  (ret (progn (plw-str str) (plw-str "R(") (plw-term pred) (plw-argz *va* cdrtrm) (plw-str ")" ))))
  (ret (progn (plw-str str) (plw-str "D(") (plw-term pred) (plw-argz *va* cdrtrm) (plw-str ")" ))))
    
 (pcond 
  ((equal (length trm) 1) (ret (progn (plw-str str) (plw-str "1(") (plw-term (first trm)) (plw-str ")"))))
  ((equal (length trm) 2) (ret (progn (plw-str str) (plw-str "2(") (plw-term (first trm)) (plw-str ",") (plw-term (second trm)) (plw-str ")"))))
  ((equal (length trm) 3) (ret (progn (plw-str str) (plw-str "3(") (plw-term (first trm)) (plw-str ",") (plw-term (second trm)) (plw-str ",") (plw-term (third trm)) (plw-str ")"))))
  ((stringp (second trm)) (ret (progn (plw-str str) (plw-str "S(") (plw-term (first trm)) (plw-argz t (cdr trm)) (plw-str ")"))))

   (t (ret (progn (plw-str str) (plw-str "N(") (plw-term (first trm)) (plw-argz t (cdr trm)) (plw-str ")")))))))
  
  
(define plw-dotl (cdrtrm)
 (ret (plw-list cdrtrm "[" "]")))

(define plw-argz (va cdrtrm) 
 (pwhen va 
  (pwhen (null cdrtrm) (ret (plw-str ",'[]'")))
  (ret (progn (plw-str ",") (plw-dotl cdrtrm))))
 (pwhen (null (null cdrtrm)) (plw-str ",") (plw-list  cdrtrm)))

(define unescape (str)
 (csetq str (SUBSTITUTE #\| #\Return (SUBSTITUTE #\| #\Linefeed str)))
 (ret (fif (< (length str) 3) str (string-trim '(#\Space) str))))

(define plw-term (trm &optional (is-pred ()))
 (pcond 
  ((nart-p trm) (ret (plw-naut "nart" (nart-hl-formula trm))))
  ((assertion-p trm) (ret (plw-term (assertion-el-ist-formula trm))))
  ((equal trm "[]") (ret (plw-str "'[]'")))
  ((null trm) (ret (plw-str "'[]'")))
  ((EL-variable-p trm) (ret (plw-str (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm)))))
  ((HL-variable-p trm) (ret (plw-term (nth (variable-id trm) (ASSERTION-EL-VARIABLES *assrtwas*)))))
  ((constant-p trm) (ret (plw-atom trm)))
  ((keywordp trm)(ret (progn (plw-str "'$VAR'(" ) (plw-atom (symbol-name trm)) (plw-str ")"))))
  ((symbolp trm) (ret (progn (plw-str "'") (plw-str (package-name (symbol-package trm))) (plw-str ":") 
                                   (plw-str (symbol-name trm)) 
                                   (plw-str "'"))))
  ((stringp trm) (ret (plw-str (write-to-string (unescape trm)))))
  ((cnot (consp trm)) (ret (plw-str (write-to-string trm))))  ;;((numberp trm) (ret (plw-str trm)))
  (t 
   (clet ((pred (car trm)) (wtrm trm) (cdrtrm (cdr trm)))
	(pwhen (equal pred (foc #$TheList) (ret (plw-dotl cdrtrm))) 
	(pwhen (equal pred ".") (ret (plw-dotl cdrtrm)))
	(pwhen (function? pred) (ret (plw-naut "u" wtrm)))
	
	(pwhen 
	  (cor (ISA? pred #$Quantifier) (ISA? pred #$LogicalConnective))
	   (ret (progn (plw-atom pred) (plw-list cdrtrm "(" ")"))))

        (punless (cor is-pred (predicate? pred)) (ret (plw-naut "u" wtrm)))
	
	(pwhen (constant-p pred)
		(ret (progn (plw-str  "t(") (plw-atom pred)
		  (pwhen cdrtrm (plw-list cdrtrm "," " "))
		  (plw-str ")"))))
	(ret (plw-list wtrm "t(" ")"))))))


(define constant-prolog-name (trm)
 (clet ((found (gethash trm *renames* )))
   (pwhen (stringp found) (ret found))
 (ret (constant-name trm))))
   

(defvar *assrtwas* ())
(defvar *assrtform* ())
(defvar *deduced* ())
(defvar *every1000* 1)
(defvar *file-output* *standard-output*)
(defvar *renames* (MAKE-HASH-TABLE 336790 ))

;; 1_2
;; 3_1_2
;; 2_1_3_4
;; 2_1_3_4_5...
(define reorder (f)
  (clet ((len (length f)) (pred (car f)))
    ;; (pwhen (equal pred #$isa) (ret (list (third f) pred (second f))))
    (pwhen (= len 3) (ret (list (third f) pred (second f))))
    (pwhen (> len 3) (ret (cons (second f) (cons pred (cddr f)))))
  (ret f)))

(define unreorder (f)
  (clet ((len (length f)) (pred (car f)))
    (pwhen (= len 3) (ret (list (second f) (third f) (first f))))
    (pwhen (> len 3) (ret (cons (second f) (cons pred (cddr f)))))))

(define showa (assrt &optional (out *standard-output*))
 (clet ((found assrt) )
  (pwhen (integerp assrt) (csetq found (FIND-ASSERTION-BY-ID assrt))(pwhen found (csetq assrt found)))
  (pwhen (consp assrt) (csetq found (FIND-ASSERTION-ANY-MT assrt))(pwhen found (csetq assrt found)))  
 (csetq *assrtform* (assertion-formula assrt))
 (pwhen *assrtform*
  (csetq *assrtwas* assrt) 
  (csetq assrt *assrtwas*) 
  (punless (equal (car *assrtform*) #$termOfUnit)
    (csetq *assrtform* (reorder *assrtform*))
    (clet ((*file-output* out)(cdrform (cdr *assrtform*)))
	  (plw-str (string-downcase (symbol-name (assertion-direction assrt))))
	  (plw-str "(")
	  (plw-term (assertion-mt assrt))
	  (plw-str ",")
	  (plw-term (car *assrtform*))
	  (pwhen cdrform (plw-list cdrform "," " "))
          (plw-str ",a7166_") 
	  (plw-str (assertion-id assrt))
	  (plw-str " /* , ")
	  (plw-str (string-downcase (symbol-name (assertion-strength assrt))))
	  (pwhen (asserted-assertion? assrt) (plw-str "_a"))
	  (pwhen (deduced-assertion? assrt) (plw-str "_d"))
	  (pwhen (asserted-by assrt) (plw-str "_b"))
	  (pwhen (asserted-when assrt) (plw-str "_w"))
	  (plw-str " */ ).") 
	  
          (plw-nl))))))

 

#|

(defvar *expected* (- (assertion-count) (nart-count)))
(defvar *live* 0)
(defvar *dead* 0)

(define missing-asserts (fn)
(csetq *live* 0)
(csetq *dead* 0)
(csetq *was* 0)
(cdo ((anum (nart-count) (1+ anum))) ((= anum (assertion-count)))
 (clet ((assrt (find-assertion-by-id anum))(countme (funcall fn assrt)))
 (pwhen (null countme) (cinc *dead*) 
   (punless (equal *was* 'dead)
     (csetq *was* 'dead)
	(print `(dead ,anum ,assrt ,(asserted-by assrt)))))
 (punless (null countme) (cinc *live*)
   (punless (equal *was* 'live)
     (csetq *was* 'live)
	(print `(live ,anum ,assrt ,(asserted-by assrt)))))))
    

(print `(*live* ,*live* *dead* ,*dead*)))

(missing-asserts #'asserted-by)

% VersionOfElleForSubstituteThenEvaluate0029-3
(missing-asserts #'assertion-formula)
(missing-asserts #'assertion-p)
(find-assertion-by-id 8000000)
#301966
[Def](cyclistNotes DrinkingVessel "If BuyingADrink were to imply a ServingFoodOrDrink in which a drink is served, then the axioms about the drink being in a drinking vessel and being transported, etc., would be inferred.") 

GAP HERE

#302622
TooHotToTouch 



#8467944
[Def](toBeReviewedBy  [Def]M(multiWordString (TheList "null" "pointer" "offset") Error-TheWord CountNoun NullPointerOffsetError) NLReviewer)



313913
995069

681157
680,000



cd /mnt/sdb1/researchcyc-4.0q/server/cyc/run
timedatectl set-ntp off
timedatectl set-time "2013-08-09 23:39:00"
./bin/run-cyc.sh
timedatectl set-ntp on

(load "export.lisp")

(dumpt)

(load "e2c/prologmud.lisp")

|#



; (w)

(define w ()
(load "e2c/prologmud.lisp")
(cdo  ;; ((anum 1190000 (1+ anum))) ((= anum 1190005))
 ((anum 990000 (1+ anum))) ((= anum (+ 990000 500)))
 (clet ((assrt (find-assertion-by-id anum))) (showa assrt *file-output*))))


(define plw-support (d)
 (plw-term (list (support-mt d) (support-sentence d) (symbol-name (support-truth d)) (symbol-name (support-strength d)) (symbol-name (support-module d)))))

(define plw-id (str int)
  (plw-str str)
  (plw-str "7166_")
  (plw-str int))

(define plw-deduction (ded)
 (csetq	 *ded* ded)
 (csetq *assrt* (deduction-assertion ded))
 (cdolist 
    (d (KB-DEDUCTION-SUPPORTS ded))

  (plw-str "deduction(")
	 (plw-id "d" (deduction-id ded))
	 (plw-str ",")
	 (pcond 
	   ((assertion-p *assrt*) (plw-id "a" (assertion-id *assrt*)))
	   (t (plw-term *assrt*)))
	 (plw-str ",")
     
      (pcond 
        ((assertion-p d)(plw-id "a" (assertion-id d)))
        ((HL-SUPPORT-P d)(plw-support d)) 
	((SUPPORT-P d)(plw-support d)) 
       (t (plw-id (type-of d) d)))
 ;;(plw-str ",")(plw-str (string-downcase (symbol-name (KB-DEDUCTION-STRENGTH ded))))
 ;;(plw-str ",")(plw-str (string-downcase (symbol-name (KB-DEDUCTION-TRUTH ded))))
  (plw-str ").")(plw-nl)))
 
(define foc (trm) (ret (find-or-create-constant trm)))

(define ds () (ds1 (foc "denotationAndString")))

(define ds1 (term) 
  (load "e2c/prologmud.lisp")
  (sL::clet ((*file-output* (SL::OPEN-TEXT (cconcatenate (simple-name term) ".pl") :output)) 
    (*standard-output* *file-output*))
   (WITH-ANY-MT (with-all-mts (map-term #'showa term)))
 (SL::close *file-output*)))

(define dt () 
(load "e2c/prologmud.lisp")
 (sL::clet ((*file-output* (SL::OPEN-TEXT "supports.pl" :output)))
  ;; 0 
  (cdo  ((anum 0 (1+ anum))) ((= anum (deduction-count)))
  (plw-deduction (find-deduction-by-id anum)))
  (SL::close *file-output*)))




(plw-str "hi0" )

(define dumpt ()
	(sL::clet ((*file-output* (SL::OPEN-TEXT "dump4.txt" :output)))
(plw-str  "
:- style_check(-discontiguous). 
:- style_check(-singleton).
:- include('dir.header').
")
  (cdo ((anum 0 (1+ anum))) ((= anum (assertion-count)))
	 (clet ((assrt (find-assertion-by-id anum)))
	  (pwhen (equal (rem (cinc  *every1000*) 10000) 1) 
	   (showa assrt *standard-output*) (force-output *standard-output*) (force-output *file-output*))

   (showa assrt *file-output*)))
 (SL::close *file-output*)))

(force-output)
(plw-str "hi")
(force-output)

;; (LOAD-KE-TEXT-FILE #$CycAdministrator "e2c/e2c-new-0.ke" :agenda t)



