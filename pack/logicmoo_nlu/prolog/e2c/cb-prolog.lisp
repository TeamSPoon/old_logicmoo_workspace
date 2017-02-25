;; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, first visit that file with C-x C-f,
;; then enter the text in that file's own buffer.
;; (load "cynd/cb-prolog.lisp")

#+Allegro
(sl::cpushnew :CYC-COMMON-LISP *features*)

#+Allegro
(use-package :CYC)

#-CLISP
 (sl::punless (member :CYC-COMMON-LISP *features*)
  (progn (load "cynd/common.lisp")
   ;;(USE-PACKAGE :SYS :CYC #'better-symbol)
   ))

#+CLISP
(load "cyc-stub.lisp")

(import 'SUBLISP:PUNLESS)
(import 'CL::DEFUN)
(import 'CYC::FIND-OR-CREATE-CONSTANT)
(import 'SUBLISP:CLET)
(import 'SUBLISP:DEFINE)
(import 'SUBLISP:RET)

(defun cyc-call-goal (goal)
    (ask-template goal goal #$InferencePSC))

(cl::defmacro reduction-eval (goal)
     `(with-error-handler 
        (cl-lambda () goal)
        (eval goal)))

(defvar *DOUBLE-QUOTE-CHAR* (char "\"" 0))


;;;;;;;;;;;;;START FILL POINTER WORKARROUNDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
#-COMMON-LISP
(punless (fboundp 'ARRAY-HAS-FILL-POINTER-P)
  (defun ARRAY-HAS-FILL-POINTER-P (a) ))

#-COMMON-LISP
(punless (fboundp 'set-fill-pointer)
 (defsetf 'fill-pointer 'set-fill-pointer)
 (define set-fill-pointer (a p)  (warn "playing with fill poitneres!") ))

#-COMMON-LISP
(punless (fboundp 'FILL-POINTER)
  (defun FILL-POINTER (a) 
    (warn "playing with fill poitneres!")
    0))


;;;;;;;;;;;;;END FILL POINTER WORKARROUNDS ;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun foc (str) (find-or-create-constant str))

;; TODO - make prolog reader use it's own copy
(defvar *lvarloc nil)
(defvar *lvarglob nil)

(defvar *prologMt* (foc "PrologDataMt"))
(defvar *theList* (foc "TheList"))
(defvar *entailedBy* (foc "prolog:entailedBy"))
(defvar *prologFn* (foc "prolog:fn"))
(defvar *prologSource* (foc "prolog:sourceCode"))
(defvar *ist* (foc "ist"))
(defvar *prologPredicate* (foc "prolog:ProgrammingPredicate"))
(defvar *prologCons* (list  *prologFn* "."))
(defvar *CycNil* (foc "TheEmptyList"))
(defvar *prologNil* (list  *prologFn* "[]"))

(defvar *xfy* ())
(defvar *fx* ())
(defvar *xfx* ())
(defvar *fy* ())

(setq *xfy* '(";" ","))
(setq *xfx* '(":-" "->" "+" "-" "/" "*" "=" "==" ":"))
(setq *fx* '("+" "-"))
(setq *fy* '(":-"))

(defun string-remove-segment ( str strtstr endstr )
 (let ((start (search-obey-escapes strtstr str)))
  (if (null start) str
   (let ((end (search-obey-escapes endstr str #'eql #'identity 0 nil start)))
    (if (not (and (numberp start)(numberp end)(> end start))) str
     (string-remove-segment (string-concat (SUBSEQ str 0 start) (SUBSEQ str (+ end (length endstr)))) strtstr endstr))))))

;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

#|
CYC(5): (<- (foo 3))
FOO
CYC(6): (<- (foo 4))
FOO
CYC(7): (?- (foo ?X))

?X = 1;

?X = 2;

?X = 3;

?X = 4;

No.NIL

CYC(8): (symbol-plist 'foo)
(CLAUSES (((FOO 1)) ((FOO 2)) ((FOO 3)) ((FOO 4))))
CYC(9): 

CYC(8): (symbol-plist 'member)
(CLAUSES (((MEMBER ?ITEM (?ITEM . ?REST))) ((MEMBER ?ITEM (?X . ?REST)) (MEMBER ?ITEM ?REST))))

CYC(6) (?- (#$prolog:fn "member" ?item (#$prolog:fn "." 1 (#$prolog:fn "." 2 (#$prolog:fn "[]")))))
No.Nil ;; fixing


    (<- (#$prolog:fn "member" ?item (#$prolog:fn "." ?item  ?rest)))
    (<- (#$prolog:fn "member" ?item (#$prolog:fn "." ??x ?rest)) (#$prolog:fn "member" ?item ?rest))


 (eval `(<- (,*prologFn* "m" 1)))
 (eval `(?- (,*prologFn* "m" 1)))

(?- (#$isa foo Col))

CYC(12):  
(?- (append ?X ?Y (1 2 3)))

?X = NIL
?Y = (1 2 3);

?X = (1)
?Y = (2 3);

?X = (1 2)
?Y = (3);

?X = (1 2 3)
?Y = NIL;

No.
|#
;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;; Code from Paradigms of AI Programming
;;; Copyright (c) 1991 Peter Norvig

;;; File auxfns.lisp: Auxiliary functions used by all other programs
;;; Load this file before running any other programs.

#|
CYC(5): (<- (foo 3))
FOO
CYC(6): (<- (foo 4))
FOO
CYC(7): (?- (foo ?X))

?X = 1;

?X = 2;

?X = 3;

?X = 4;

No.NIL

CYC(8): (symbol-plist 'foo)
(CLAUSES (((FOO 1)) ((FOO 2)) ((FOO 3)) ((FOO 4))))
CYC(9): 

CYC(8): (symbol-plist 'member)
(CLAUSES (((MEMBER ?ITEM (?ITEM . ?REST))) ((MEMBER ?ITEM (?X . ?REST)) (MEMBER ?ITEM ?REST))))

CYC(6) (?- (#$prolog:fn "member" ?item (#$prolog:fn "." 1 (#$prolog:fn "." 2 (#$prolog:fn "[]")))))
No.Nil ;; fixing


    (<- (#$prolog:fn "member" ?item (#$prolog:fn "." ?item  ?rest)))
    (<- (#$prolog:fn "member" ?item (#$prolog:fn "." ??x ?rest)) (#$prolog:fn "member" ?item ?rest))

(?- (#$isa foo Col))

CYC(12):  (?- (append ?X ?Y (1 2 3)))

?X = NIL
?Y = (1 2 3);

?X = (1)
?Y = (2 3);

?X = (1 2)
?Y = (3);

?X = (1 2 3)
?Y = NIL;

No.
|#

;;;; Implementation-Specific Details


(defvar *prologFn* (find-or-create-constant "prolog:fn"))

    
#|
(eval-when (eval compile load)
  ;; Make it ok to place a function definition on a built-in LISP symbol.
  #+(or Allegro EXCL)
  (dolist (pkg '(excl common-lisp common-lisp-user))
    (setf (excl:package-definition-lock (find-package pkg)) nil))

  ;; Don't warn if a function is defined in multiple files --
  ;; this happens often since we refine several programs.
  #+Lispworks
  (setq *PACKAGES-FOR-WARN-ON-REDEFINITION* nil)

  #+LCL 
   (compiler-options :warnings nil)
  )
|#
;;;; REQUIRES

;;; The function REQUIRES is used in subsequent files to state dependencies
;;; between files.  The current definition just loads the required files,
;;; assumming they match the pathname specified in *book-DIRECTORY*.
;;; You should change that to match where you have stored the files.
;;; A more sophisticated REQUIRES would only load it if it has not yet
;;; been loaded, and would search in different directories if needed.
#|

(defun requires (&rest files)
  "The arguments are files that are required to run an application."
  (mapc #'load-book-file files))

(defvar *book-files*
  `("auxfns" "tutor" "examples" 
    "intro" "simple" "overview" "gps1" "gps" "eliza1" "eliza" "patmatch" 
    "eliza-pm" "search" "gps-srch" "student" "macsyma" "macsymar" "book-unify" 
    "prolog1" "prolog" "prologc1" "prologc2" "prologc" "prologcp" 
    "clos" "krep1" "krep2" "krep" "cmacsyma" "mycin" "mycin-r" "waltz" 
    "othello" "othello2" "syntax1" "syntax2" "syntax3" "unifgram" 
    "grammar" "lexicon" "interp1" "interp2" "interp3" 
    "compile1" "compile2" "compile3" "compopt"))

(defparameter *book-directory*
  (make-pathname :name nil :type nil
		 :defaults (or (and (boundp '*load-truename*) *load-truename*)
			       (truename ""))) ;;??? Maybe Change this
  "The location of the source files for this book.  If things don't work,
  change it to reflect the location of the files on your computer.")

(defparameter *book-source* 
  (make-pathname :name nil :type "lisp" ;;???  Maybe Change this
		 :defaults *book-directory*)) 

(defparameter *book-binary*
  (make-pathname
   :name nil
   :type (first (list #+LCL (first *load-binary-pathname-types*)
		      #+Lispworks system::*binary-file-type*
		      #+MCL "fasl"
		      #+Allegro excl:*fasl-default-type*
		      #+(or AKCL KCL) "o"
		      #+CMU "sparcf"
		      #+CLISP "fas"
		      "bin"))  ;;???  Maybe Change this
   :directory (append (pathname-directory *book-source*) '("bin"))
   :defaults *book-directory*))

(defun book-pathname (name &optional (type :lisp))
  (make-pathname :name name 
		 :defaults (ecase type
			     ((:lisp :source) *book-source*)
			     ((:binary :bin) *book-binary*))))

(defun compile-all-book-files ()
  (mapc #'compile-book-file *book-files*))

(defun compile-book-file (name)
  (let ((path (book-pathname name :lisp)))
    (load path)
    (compile-file path :output-file (book-pathname name :binary))))

(defun load-book-file (file)
  "Load the binary file if it exists and is newer, else load the source."
  (let* ((src (book-pathname file :lisp))
	 (src-date (file-write-date src))
	 (bin (book-pathname file :binary))
	 (bin-date (file-write-date bin)))
    (load (if (and (probe-file bin) src-date bin-date (>= bin-date src-date))
	      bin
	    src))))
|#
;;;; Macros (formerly in auxmacs.lisp: that file no longer needed)


  (cl::defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym) temps))
      `(if (every #'side-effect-free? (list . ,variables))
	(progn . ,body)
	(list 'let ,`(list ,@(mapcar (cl-lambda (tmp var) `(list ',tmp ,var)) temps variables))
	 (let ,(mapcar (cl-lambda (var tmp) `(,var ',tmp))
		       variables temps)
	   .,body)))))

  (defun side-effect-free? (exp)
    "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
    (or (atom exp) (constantp exp)
	(first-eql exp 'function)
	(and (first-eql exp 'the)
	     (side-effect-free? (third exp)))))

  (cl::defmacro book-funcall-if (fn arg)
    (once-only (fn)
	       `(if ,fn (funcall ,fn ,arg) ,arg)))

  (cl::defmacro read-time-case (first-case &rest other-cases)
    "Do the first case, where normally cases are
  specified with #+ or possibly #- marks."
    (declare (ignore other-cases))
    first-case)

  (defun rest2 (x)
    "The rest of a list after the first TWO elements."
    (rest (rest x)))

  (defun book-find-anywhere (item tree)
    "Does item occur anywhere in tree?"
    (if (atom tree)
	(if (eql item tree) tree)
	(or (book-find-anywhere item (first tree))
	    (book-find-anywhere item (rest tree)))))

  (defun first-eql (list x)
    "Is x a list whose first element is x?"
    (and (consp list) (eql (first list) x)))
 

;;;; Auxiliary Functions

;;(setf (symbol-function 'find-all-if) #'remove-if-not)

(print "loading book-prolog.lisp")(terpri)

#+CLISP
(defun book-find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  (if test-not
      (apply #'remove item sequence 
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))


(defun partition-if (pred list)
  "Return 2 values: elements of list that satisfy pred,
  and elements that don't."
  (let ((yes-list nil)
        (no-list nil))
    (dolist (item list)
      (if (funcall pred item)
          (push item yes-list)
          (push item no-list)))
    (values (nreverse yes-list) (nreverse no-list))))

(defun length=1 (x) 
  "Is x a list of length 1?"
  (and (consp x) (null (cdr x))))

(defun maybe-add (op exps &optional if-nil)
  "For example, (maybe-add 'and exps t) returns
  t if exps is nil, exps if there is only one,
  and (and exp1 exp2...) if there are several exps."
  (cond ((null exps) if-nil)
        ((length=1 exps) (first exps))
        (t (cons op exps))))

;;; ==============================

(defun seq-ref (seq index)
  "Return code that indexes into a sequence, using
  the pop-lists/aref-vectors strategy."
  `(if (listp ,seq)
       (prog1 (first ,seq)
              (setq ,seq (the list (rest ,seq))))
       (aref ,seq ,index)))


(defun maybe-set-fill-pointer (array new-length)
  "If this is an array with a fill pointer, set it to
  new-length, if that is longer than the current length."
  (if (and (arrayp array)
           (array-has-fill-pointer-p array))
      (setf (fill-pointer array) 
            (max (fill-pointer array) new-length))))

;;; ==============================

;;; NOTE: In ANSI Common Lisp, the effects of adding a definition (or most
;;; anything else) to a symbol in the common-lisp package is undefined.
;;; Therefore, it would be best to rename the function SYMBOL to something 
;;; else.  This has not been done (for compatibility with the book).  

(defun book-symbol (&rest args)
  "Concatenate symbols or strings to form an interned symbol"
  (intern (force-format nil "~{~a~}" args)))

(print "loaded book-prolog.lisp!")(terpri)

(defun new-symbol (&rest args)
  "Concatenate symbols or strings to form an uninterned symbol"
  (make-symbol (force-format nil "~{~a~}" args)))

'(defun last1 (list)
  "Return the last element (not last cons cell) of list"
  (first (last list)))

;;; ==============================

(defun mappend (fn list)
  "Append the results of calling fn on each element of list.
  Like mapcon, but uses append instead of nconc."
  (apply #'append (mapcar fn list)))

(defun mklist (x) 
  "If x is a list return it, otherwise return the list of x"
  (if (listp x) x (list x)))

(defun book-flatten (exp)
  "Get rid of imbedded lists (to one level only)."
  (mappend #'mklist exp))

(defun random-elt (seq) 
  "Pick a random element out of a sequence."
  (elt seq (random (length seq))))

;;; ==============================
;; (member-equal 2 '(1 2 3))
(defun member-equal (item list)
  (cl::member item list :test #'equal))

;;; ==============================

(defun compose (&rest functions)
  (cl-lambda (x)
      (reduce #'funcall functions :from-end t :initial-value x)))

;;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id force-format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'force-format *debug-io* force-format-string args)))

(defun book-debug (&rest ids)
  "Start dbg output on the given ids."
  (setf *dbg-ids* (union ids *dbg-ids*)))

(setq *dbg-ids* '(|member| member append))

(defun undebug (&rest ids)
  "Stop dbg on the ids.  With no ids, stop dbg altogether."
  (setf *dbg-ids* (if (null ids) nil
                      (set-difference *dbg-ids* ids))))

;;; ==============================

(defun dbg-indent (id indent force-format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (force-princ "  " *debug-io*))
    (apply #'force-format *debug-io* force-format-string args)))

;;;; PATTERN MATCHING FACILITY
#|
(let ((*funcall-trace* t)) (?- (member ?X (1 2 3))))
|#
(defconstant fail nil)
(defconstant no-bindings '((t . t)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((book-variable-p pattern) (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((and (consp pattern) (consp input))
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) bindings)))
        (t fail)))

(defun match-variable (var input bindings)
  "Does VAR match input?  Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
          ((equal input (binding-val binding)) bindings)
          (t fail))))

(defun make-binding (var val) (cons var val))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (cons var val)
        ;; Once we add a "real" binding,
        ;; we can get rid of the dummy no-bindings
        (if (eq bindings no-bindings)
            nil
            bindings)))

(defun book-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

;;; ==============================

;;;; The Memoization facility:

(cl::defmacro defun-memo (fn args &body body)
  "Define a memoized function."
  `(memoize (defun ,fn ,args . ,body)))

#+CL-ARGLIST-IMPLEMENTED
(defun memo (fn &key (key #'first) (test #'eql) name)
  "Return a memo-function of fn."
  (let ((table (cl::make-hash-table :test test)))
    (setf (get name 'memo) table)
    (cl-lambda (&rest args)
        (let ((k (funcall key args)))
          (multiple-value-bind (val found-p)
              (gethash k table)
            (if found-p val
                (setf (gethash k table) (apply fn args))))))))

#+CL-ARGLIST-IMPLEMENTED
(defun memoize (fn-name &key (key #'first) (test #'eql))
  "Replace fn-name's global definition with a memoized version."
  (clear-memoize fn-name)
  (setf (symbol-function fn-name)
        (memo (symbol-function fn-name)
              :name fn-name :key key :test test)))

(defun clear-memoize (fn-name)
  "Clear the hash table from a memo function."
  (let ((table (get fn-name 'memo)))
    (when table (clrhash table))))

#+CL-ARGLIST-IMPLEMENTED
;;;; Delayed computation:
(cl::defstruct delay value (computed? nil))

(cl::defmacro delay (&rest body)
  "A computation that can be executed later by FORCE."
  `(make-delay :value (cl-lambda () . ,body)))

(defun force (delay)
  "Do a delayed computation, or fetch its previously-computed value."
  (if (delay-computed? delay)
      (delay-value delay)
      (prog1 (setf (delay-value delay) (funcall (delay-value delay)))
             (setf (delay-computed? delay) t))))

;;;; Defresource:

#+CL-ARGLIST-IMPLEMENTED
(cl::defmacro defresource (name &key constructor (initial-copies 0)
                       (size (max initial-copies 10)))
  (let ((resource (symbol '* (symbol name '-resource*)))
        (deallocate (symbol 'deallocate- name))
        (allocate (symbol 'allocate- name)))
    `(progn
       (defparameter ,resource (make-array ,size :fill-pointer 0))
       (defun ,allocate ()
         "Get an element from the resource pool, or make one."
         (if (= (fill-pointer ,resource) 0)
             ,constructor
             (vector-pop ,resource)))
       (defun ,deallocate (,name)
         "Place a no-longer-needed element back in the pool."
         (vector-push-extend ,name ,resource))
       ,(if (> initial-copies 0)
            `(mapc #',deallocate (cl::loop repeat ,initial-copies 
                                       collect (,allocate))))
       ',name)))

(cl::defmacro with-resource ((var resource &optional protect) &rest body)
  "Execute body with VAR bound to an instance of RESOURCE."
  (let ((allocate (symbol 'allocate- resource))
        (deallocate (symbol 'deallocate- resource)))
    (if protect
        `(let ((,var nil))
           (unwind-protect (progn (setf ,var (,allocate)) ,@body)
             (unless (null ,var) (,deallocate ,var))))
        `(let ((,var (,allocate)))
           ,@body
           (,deallocate var)))))

;;;; Queues:

;;; A queue is a (last . contents) pair

(defun queue-contents (q) (cdr q))
#|
(defun make-queue ()
  "Build a new queue, with no elements."
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  "Insert item at the end of the queue."
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  "Remove an item from the front of the queue."
  (pop (cdr q))
  (if (null (cdr q)) (setf (car q) q))
  q)
|#
(defun front (q) (first (queue-contents q)))

(defun empty-queue-p (q) (null (queue-contents q)))

(defun queue-nconc (q list)
  "Add the elements of LIST to the end of the queue."
  (setf (car q)
        (last (setf (rest (car q)) list))))


(defun kwetest (t &key key) (print `(kwetest ,t ,&key ,key)))
;;;; Other:

#+CL-ARGLIST-IMPLEMENTED
(defun sort* (seq pred &key key) 
  "Sort without altering the sequence"
  (sort (copy-seq seq) pred :key key))

(defun reuse-cons (x y x-y)
  "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
  (if (and (eql x (car x-y)) (eql y (cdr x-y)))
      x-y
      (cons x y)))

;;; ==============================


(defun rest3 (list)
  "The rest of a list after the first THREE elements."
  (cdddr list))

;;; ==============================

(defun unique-find-if-anywhere (predicate tree
                                &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-if-anywhere
        predicate
        (first tree)
        (unique-find-if-anywhere predicate (rest tree)
                                 found-so-far))))

(defun find-if-anywhere (predicate tree)
  "Does predicate apply to any atom in the tree?"
  (if (atom tree)
      (funcall predicate tree)
      (or (find-if-anywhere predicate (first tree))
          (find-if-anywhere predicate (rest tree)))))

;;; ==============================

(cl::defmacro define-enumerated-type (type &rest elements)
  "Represent an enumerated type with integers 0-n."
  `(progn
     (deftype ,type () '(integer 0 ,(- (length elements) 1)))
     (defun ,(symbol type '->symbol) (,type)
       (elt ',elements ,type))
     (defun ,(symbol 'symbol-> type) (symbol)
       (position symbol ',elements))
     ,@(cl::loop for element in elements
             for i from 0
             collect `(defconstant ,element ,i))))

;;; ==============================

(defun not-null (x) (not (null x)))

(defun first-or-nil (x)
  "The first element of x if it is a list; else nil."
  (if (consp x) (first x) nil))

(defun first-or-self (x)
  "The first element of x, if it is a list; else x itself."
  (if (consp x) (first x) x))

;;; ==============================

;;;; CLtL2 and ANSI CL Compatibility

(unless (fboundp 'defmethod)
  (cl::defmacro defmethod (name args &rest body)
    `(defun ',name ',args ,@body))
)

(unless (fboundp 'map-into)
#-CLISP
 (defun map-into (result-sequence function &rest sequences)
  "Destructively set elements of RESULT-SEQUENCE to the results
  of applying FUNCTION to respective elements of SEQUENCES."
  (let ((arglist (make-list (length sequences)))
        (n (if (listp result-sequence)
               most-positive-fixnum
               (array-dimension result-sequence 0))))
    ;; arglist is made into a list of args for each call
    ;; n is the length of the longest vector
    (when sequences
      (setf n (min n (cl::loop for seq in sequences
                           minimize (length seq)))))
    ;; Define some shared functions:
    (flet
      ((do-one-call (i)
         (cl::loop for seq on sequences
               for arg on arglist
               do (if (listp (first seq))
                      (setf (first arg)
                            (pop (first seq)))
                      (setf (first arg)
                            (aref (first seq) i))))
         (apply function arglist))
       (do-result (i)
         (if (and (vectorp result-sequence)
                  (array-has-fill-pointer-p result-sequence))
             (setf (fill-pointer result-sequence) 
                   (max i (fill-pointer result-sequence))))))
      (declare (inline do-one-call))
      ;; Decide if the result is a list or vector,
      ;; and cl::loop through each element
      (if (listp result-sequence)
          (cl::loop for i from 0 to (- n 1)
                for r on result-sequence
                do (setf (first r)
                         (do-one-call i))
                finally (do-result i))
          (cl::loop for i from 0 to (- n 1)
                do (setf (aref result-sequence i)
                         (do-one-call i))
                finally (do-result i))))
      result-sequence))
)

;;(unless (fboundp 'book-complement)
(defun book-complement (fn)
  "If FN returns y, then (book-complement FN) returns (not y)."
  (cl-lambda (&rest args) (not (apply fn args))))
;;)

(unless (fboundp 'with-compilation-unit)
(cl::defmacro with-compilation-unit (options &body body)
  "Do the body, but delay compiler warnings until the end."
  ;; That way, undefined function warnings that are really
  ;; just forward references will not be printed at all.
  ;; This is defined in Common Lisp the Language, 2nd ed.
  (declare (ignore options))
  `(,(read-time-case
       #+Lispm 'compiler:compiler-warnings-context-bind
       #+Lucid 'with-deferred-warnings
               'progn)
    .,body))
)

;;;; Reduce

(when nil ;; Change this to T if you need REDUCE with :key keyword.

(defun reduce* (fn seq from-end start end key init init-p)
  (funcall (if (listp seq) #'reduce-list #'reduce-vect)
           fn seq from-end (or start 0) end key init init-p))

#+CL-ARGLIST-IMPLEMENTED
(defun reduce (function sequence &key from-end start end key
               (initial-value nil initial-value-p))
  (reduce* function sequence from-end start end
           key initial-value initial-value-p))

(defun reduce-vect (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (assert (<= 0 start end (length seq)) (start end)
          "Illegal subsequence of ~a --- :start ~d :end ~d"
          seq start end)
  (case (- end start)
    (1 (if init-p
           (funcall fn init (book-funcall-if key (aref seq start)))
           (book-funcall-if key (aref seq start))))
    (0 (if init-p init (funcall fn)))
    (t (if (not from-end)
           (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (book-funcall-if key (aref seq start)))
                       (funcall
                         fn
                         (book-funcall-if key (aref seq start))
                         (book-funcall-if key (aref seq (+ start 1)))))))
             (cl::loop for i from (+ start (if init-p 1 2))
                   to (- end 1)
                   do (setf result
                            (funcall
                              fn result
                              (book-funcall-if key (aref seq i)))))
             result)
           (let ((result
                   (if init-p
                       (funcall
                         fn
                         (book-funcall-if key (aref seq (- end 1)))
                         init)
                       (funcall
                         fn
                         (book-funcall-if key (aref seq (- end 2)))
                         (book-funcall-if key (aref seq (- end 1)))))))
             (cl::loop for i from (- end (if init-p 2 3)) downto start
                   do (setf result
                            (funcall
                              fn
                              (book-funcall-if key (aref seq i))
                              result)))
             result)))))

(defun reduce-list (fn seq from-end start end key init init-p)
  (if (null end) (setf end (length seq)))
  (cond ((> start 0)
         (reduce-list fn (nthcdr start seq) from-end 0
                      (- end start) key init init-p))
        ((or (null seq) (eql start end))
         (if init-p init (funcall fn)))
        ((= (- end start) 1)
         (if init-p
             (funcall fn init (book-funcall-if key (first seq)))
             (book-funcall-if key (first seq))))
        (from-end
         (reduce-vect fn (coerce seq 'vector) t start end
                      key init init-p))
        ((null (rest seq))
         (if init-p
             (funcall fn init (book-funcall-if key (first seq)))
             (book-funcall-if key (first seq))))
        (t (let ((result
                   (if init-p
                       (funcall
                         fn init
                         (book-funcall-if key (pop seq)))
                       (funcall
                         fn
                         (book-funcall-if key (pop seq))
                         (book-funcall-if key (pop seq))))))
             (if end
                 (cl::loop repeat (- end (if init-p 1 2)) while seq
                    do (setf result
                             (funcall
                               fn result
                               (book-funcall-if key (pop seq)))))
                 (cl::loop while seq
                    do (setf result
                             (funcall
                               fn result
                               (book-funcall-if key (pop seq))))))
             result))))
);;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig

;;;; File pat-match.lisp: Pattern matcher from section 6.2

;;; Two bug fixes By Richard Fateman, rjf@cs.berkeley.edu  October 92.

;;; The basic are in auxfns.lisp; look for "PATTERN MATCHING FACILITY"

(defun book-variable-p (x)
  "Is x a variable (a symbol beginning with `?')?"
  (and (symbolp x) (equal (elt (symbol-name x) 0) #\?)))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match pattern against input in the context of the bindings"
  (cond ((eq bindings fail) fail)
        ((book-variable-p pattern)
         (match-variable pattern input bindings))
        ((eql pattern input) bindings)
        ((segment-pattern-p pattern)                
         (segment-matcher pattern input bindings))  
        ((single-pattern-p pattern)                 ; ***
         (single-matcher pattern input bindings))   ; ***
        ((and (consp pattern) (consp input)) 
         (pat-match (rest pattern) (rest input)
                    (pat-match (first pattern) (first input) 
                               bindings)))
        (t fail)))


(setf (get '?is  'single-match) 'match-is)
(setf (get '?or  'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)

(setf (get '?*  'segment-match) 'segment-match)
(setf (get '?+  'segment-match) 'segment-match+)
(setf (get '??  'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern)) 
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
           pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of single pattern."
  (funcall (single-match-fn (first pattern))
           (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x, 
  if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred,
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
         (pred (second var-and-pred))
         (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
            (not (funcall pred input)))
        fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input."
  (cond ((eq bindings fail) fail)
        ((null patterns) bindings)
        (t (match-and (rest patterns) input
                      (pat-match (first patterns) input
                                 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns) 
                                     input bindings)))
        (if (eq new-bindings fail)
            (match-or (rest patterns) input bindings)
            new-bindings))))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input.
  This will never bind any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (if (null pat)
        (match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
          (if (null pos)
              fail
              (let ((b2 (pat-match
                          pat (subseq input pos)
                          (match-variable var (subseq input 0 pos)
                                          bindings))))
                ;; If this match failed, try another longer one
                (if (eq b2 fail)
                    (segment-match pattern input bindings (+ pos 1))
                    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input,
  starting at position start.  If pat1 is non-constant, then just
  return start."
  (cond ((and (atom pat1) (not (book-variable-p pat1)))
         (position pat1 input :start start :test #'equal))
        ((<= start (length input)) start) ;*** fix, rjf 10/1/92 (was <)
        (t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-match pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
        (pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)."
  ;; *** fix, rjf 10/1/92 (used to eval binding values)
  (and (progv (mapcar #'car bindings)
              (mapcar #'cdr bindings)
          (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))  

(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev) 
    (expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
        ((atom pat) pat)
        (t (cons (expand-pat-match-abbrev (first pat))
                 (expand-pat-match-abbrev (rest pat))))))

#+CL-ARGLIST-IMPLEMENTED
(defun rule-based-translator 
       (input rules &key (matcher 'pat-match) 
        (rule-if #'first) (rule-then #'rest) (action #'sublis))
  "Find the first rule in rules that matches input,
  and apply the action to that rule."
  (some 
    (cl-lambda (rule)
        (let ((result (funcall matcher (funcall rule-if rule) 
                               input)))
          (if (not (eq result fail))
              (funcall action result (funcall rule-then rule)))))
    rules))
;;; -*- Mode: Lisp; Syntax: Common-Lisp; -*-
;;; Code from Paradigms of Artificial Intelligence Programming
;;; Copyright (c) 1991 Peter Norvig

;;;; File book-unify.lisp: Unification functions

;(requires "patmatch")

(defparameter *occurs-check* t "Should we do the occurs check?")

(defun book-unify (x y &optional (bindings no-bindings))
  "See if x and y match with given bindings."
  (cond ((eq bindings fail) fail)
        ((eql x y) bindings)
        ((book-variable-p x) (book-unify-variable x y bindings))
        ((book-variable-p y) (book-unify-variable y x bindings))
        ((and (consp x) (consp y))
         (book-unify (rest x) (rest y) 
                (book-unify (first x) (first y) bindings)))
        (t fail)))

(defun book-unify-variable (var x bindings) ;;deref-exp
  "book-unify var with x, using (and maybe extending) bindings."
  (cond ((get-binding var bindings)
         (book-unify (lookup var bindings) x bindings))
        ((and (book-variable-p x) (get-binding x bindings))
         (book-unify var (lookup x bindings) bindings))
        ((and *occurs-check* (occurs-check var x bindings))
         fail)
        (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)
  "Does var occur anywhere inside x?"
  (cond ((eq var x) t)
        ((and (book-variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))

;;; ==============================

(defun book-subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (book-variable-p x) (get-binding x bindings))
         (book-subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (book-subst-bindings bindings (car x))
                       (book-subst-bindings bindings (cdr x))
                       x))))

;;; ==============================

(defun unifier (x y)
 "Return something that unifies with both x and y (or fail)."
 (book-subst-bindings (book-unify x y) x))
;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;(defun requires (f) (load f))
;;;; File prolog.lisp: prolog from (11.3), with interactive backtracking.

;(requires "book-unify") ; does not require "prolog1"

;;;; does not include destructive unification (11.6); see prologc.lisp

;; clauses are represented as (head . body) cons cells
(defun clause-head (clause) (first clause))
(defun clause-body (clause) (rest clause))


(setq   *book-unify-RETURN-JUSTIFICATION?* t)


(defun get-predicate (relation) 
    (if (stringp relation)
      (intern relation)
      (if (consp relation)
         (if (equal (car relation) *prologFn*)
            (get-predicate (second relation))
            (get-predicate (car relation)))
           relation)))

(defun args (x) "The arguments of a relation" 
  (when (consp x)
     (if (equal (car x) *prologFn*)
        (rest (cdr x))
        (rest x))))

(defvar *db-predicates* nil
  "a list of all predicates stored in the database.")
(defvar *cycl-predicates* nil
  "a list of all predicates stored in the cyc database.")
(defvar *subl-predicates* nil
  "a list of all predicates executed in lisp.")

(defun replace-?-vars (exp)
    "Replace any ? within exp with a var of the form ?123."
    (cond ((eq exp '?) (gensym "?"))
	  ((atom exp) exp)
	  (t (reuse-cons (replace-?-vars (first exp))
			 (replace-?-vars (rest exp))
			 exp))))

(cl::defmacro with-undo-bindings (&body body)
  "Undo bindings after each expression in body except the last."
  (if (length=1 body)
      (first body)
      `(let ((old-trail (fill-pointer *trail*)))
         ,(first body)
         ,@(cl::loop for exp in (rest body)
                 collect '(undo-bindings! old-trail)
                 collect exp))))

;;           (?- (#$isa ?Who #$Cyclist))
           ;; (cyc-call-goal '(#$isa ?Who #$Cyclist))    
#|

 (cyc-call-goal '(#$isa ?Who #$Cyclist)) =>
 ((#$isa #$SKSIAdministrator #$Cyclist) (#$isa #$OntologyExporterUser #$Cyclist)
 (#$isa #$OperationsAdministrator #$Cyclist) (#$isa #$KCTAdministrator #$Cyclist) (#$isa #$AKBUser #$Cyclist)
 (#$isa #$CycAdministrator #$Cyclist) (#$isa #$Guest #$Cyclist) (#$isa #$CPOF-User #$Cyclist)
 (#$isa #$EII-User1 #$Cyclist) (#$isa #$EII-User2 #$Cyclist) (#$isa #$HPKB-User #$Cyclist)
 (#$isa #$GuestCyclist #$Cyclist) (#$isa #$LDSC-User #$Cyclist) (#$isa #$NLReviewer #$Cyclist)
 (#$isa #$LSReviewer #$Cyclist) (#$isa #$SomeCyclist #$Cyclist) (#$isa #$BUTLERAgent #$Cyclist)
 (#$isa #$CycTermLearner #$Cyclist)
 (#$isa (#$TheLogicalFieldValueFn #$CycorpBugzillaCreate-LS #$Cyclist 2) #$Cyclist)
 (#$isa (#$TheLogicalFieldValueFn #$CycorpBugzillaCreate-LS #$Cyclist 1) #$Cyclist)
 (#$isa (#$TheLogicalFieldValueFn #$CycorpBugzillaUpdate-LS #$Cyclist 1) #$Cyclist)
 (#$isa #$TheUser #$Cyclist) (#$isa #$TheBuilder #$Cyclist) (#$isa #$TheCycProcessOwner #$Cyclist)
 (#$isa #$CycAutomatedFormulaTemplateTester #$Cyclist) (#$isa #$Cyc #$Cyclist))

 |#

;; (get-goal-clauses '(FOO ?X))
;; (get-goal-clauses '(#$isa ?X #$Cyclist))
;; (?- (#$isa ?X #$Cyclist))
 (defun get-goal-clauses (goal)
     (let ((pred (get-predicate goal)))
       (if (constant-p pred)
         (mapcar #'list (cyc-call-goal goal))
         (get-db-clauses pred))))

;; clauses are stored on the predicate's plist
(defun get-db-clauses (pred)
   (if (constant-p pred)
       (clauses-for-cyc-pred pred)
       (if (consp pred) 
          (get-db-clauses (get-predicate pred))
          (if (symbolp pred) 
            (get pred 'clauses)
            ()))))

(defun clauses-for-cyc-pred (pred)
  (let (len (arity pred))
    (if len 
     `((cyc-query '(,pred ,@(make-nvars len)) ,*prologMt* ))
     `((cyc-query '(,pred . ?REST) ,*prologMt*)))))

(defun add-clause (clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (get-predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (book-variable-p pred))))
    (let ((clauses (ASSOC pred *db-predicates*)))
        (when (null clauses)
            (setq clauses (list pred))
            (push clauses *db-predicates*))
         (rplacd clauses (nconc (cdr clauses) (list clause))))
    pred))

(defun add-clause (clause)
  "add a clause to the data base, indexed by head's predicate."
  ;; the predicate must be a non-variable symbol.
  (let ((pred (get-predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (book-variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (nconc (get-db-clauses pred) (list clause)))
    pred))

(cl::defmacro <- (&rest clause)
  "add a clause to the data base."
  `(add-clause ',(replace-?-vars clause)))

(defun clear-db ()
  "remove all clauses (for all predicates) from the data base."
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "remove the clauses for a single predicate."
  (setf (get predicate 'clauses) nil))

(defun book-rename-variables (x)
  "replace all variables in x with new ones."
  (sublis (mapcar (cl-lambda (var) 
        (cons var (gensym (string var))))
                  (variables-in x)) x))

(defun unique-book-find-anywhere-if (predicate tree
                                &optional found-so-far)
  "return a list of leaves of tree satisfying predicate,
  with duplicates removed."
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-book-find-anywhere-if
        predicate
        (first tree)
        (unique-book-find-anywhere-if predicate (rest tree)
                                 found-so-far))))

(defun book-find-anywhere-if (predicate tree)
  "does predicate apply to any atom in the tree?"  
  (if (atom tree)
      (funcall predicate tree)
      (or (book-find-anywhere-if predicate (first tree))
          (book-find-anywhere-if predicate (rest tree)))))

(cl::defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

(defun prove-all (goals bindings)
  "Find a solution to the conjunction of goals."
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))


(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (force-format t "~&No.~%")
  (values))

(defun show-prolog-vars/2 (vars bindings other-goals)
 "Print each variable with its binding.
  Then ask the user if more solutions are desired."
  (print (list 'show-prolog-vars/2 vars bindings other-goals))
  (when vars
      (dolist (var vars)
        (force-format t "~&~a = ~a" var
                (book-subst-bindings bindings var))))
 (force-format t "~&Yes")
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))

(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars/2)

(defun continue-p ()
  "Ask user if we should continue looking for solutions."
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise 
      (force-format t " Type ; to see more or . to stop")
      (continue-p))))

(defun variables-in (exp)
  "Return a list of all the variables in EXP."
  (unique-book-find-anywhere-if #'non-anon-book-variable-p exp))

(defun non-anon-book-variable-p (x)
  (and (book-variable-p x) (not (eq x '?))))

;(requires "auxfns")

;;;; -*- Mode: Lisp; Syntax: Common-Lisp -*-
;;;; Code from Paradigms of AI Programming
;;;; Copyright (c) 1991 Peter Norvig
;;(defun requires (f) (load f))
;;;; File prologcp.lisp:  Primitives for the prolog compiler
;;;; needed to actually run some functions.

;;; Bug fix by Adam Farquhar, farquhar@cs.utexas.edu.
;;; Trivia: Farquhar is Norvig's cousin.

;;; Loading #P"D:/mirror/pl/packages/ecls/book/prologcp.lisp"
;;; Loading #P"D:/mirror/pl/packages/ecls/book/prolog.lisp"
;;; Loading #P"D:/mirror/pl/packages/ecls/book/book-unify.lisp"
;;; Loading #P"D:/mirror/pl/packages/ecls/book/patmatch.lisp"
;;; Loading #P"D:/mirror/pl/packages/ecls/book/auxfns.lisp"
;; #P"D:/mirror/pl/packages/ecls/book/prologcp.lisp"


;(requires "prolog")


(defun deref-equal (x y)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?"
  (or (eql (deref x) (deref y))
      (and (consp x)
           (consp y)
           (deref-equal (first x) (first y))
           (deref-equal (rest x) (rest y)))))


(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar (cl-lambda (var) (cons (deref var) (?)))
                  (unique-book-find-anywhere-if #'book-variable-p exp))
          exp))

(defun setof/3 (exp goal result cont)
  "Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal (cl-lambda ()
                     (push (deref-copy exp) answers)))
    (if (and (not (null answers))
             (book-unify result (cl::delete-duplicates
                              answers
                              :test #'deref-equal)))
        (funcall cont))))


(defun unbound-book-variable-p (exp)
  "Is EXP an unbound var?"
  (and (book-variable-p exp) (not (bound-p exp))))


 (defun prove (goal bindings other-goals)
  "Return a list of possible solutions to goal."
  (let ((clauses (get-goal-clauses goal)))
    (if (listp clauses)
        (some
          (cl-lambda (clause)
              (let ((new-clause (book-rename-variables clause)))
                (prove-all
                  (append (clause-body new-clause) other-goals)
                  (book-unify goal (clause-head new-clause) bindings))))
          clauses)
        ;; The predicate's "clauses" can be an atom:
        ;; a primitive function to call
        (funcall clauses (rest goal) bindings
                 other-goals))))

   
;;STRING-RIGHT-TRIM STRING-RIGHT-TRIM

(cl::defmacro defprolog (namespec args &rest body)
  (clet (symstring fnsym name arity plen cycname)
    (when (symbolp namespec) (setq fnsym namespec)(setq symstring (symbol-name namespec)))
    (when (constant-p namespec) 
      (setq cycname namespec) 
      (setq symstring (cconcatenate (constant-name namespec) "/" (write-to-string (ARITY namespec)))))
    (setq plen (POSITION  #\/ symstring))
    (when plen 
       (setq name (SUBSEQ symstring 0 plen))
       (setq arity (SUBSEQ symstring (+ 1 plen))))
    (when (stringp arity) (setq arity (string-to-number arity)))
    (unless arity 
        (setq arity (if (member (last args) '(cont)) (1- (length args)) (length args))))
    (print (list 'defprolog namespec name arity args))    
    (pushnew (list name arity namespec args body) *subl-predicates* :test #'EQUAL)
    (unless (symbolp fnsym) (setq fnsym (make-symbol (cconcatenate name "/" (write-to-string arity)))))
    `(progn (setf (get ',(intern name) 'clauses) ',fnsym) (defun ,fnsym ,args ,@body))))
#|
 (let ((div (FIND
 (name ())
 (let ((arity (1- (length `,args))
    (fname (concatenate 'string `,name "/" (write-to-string `,farity))))
  (let ((sym (intern `,fname)))
   (let ((fort (prolog-to-cycl `,fname `,arity)))
    (setf (get `,sym 'evaluable) t)
    (setf (get `,sym 'prolog_name) `,name)
    (setf (get `,sym 'prolog_arity) `,farity)
    (setf (get `,sym 'lisp_arity) `,arity)
    (prolog-assert-now (list  (foc "arity") `,fort `,arity ) *prologMt* ) 
    (prolog-assert-now (list  (foc "evaluationDefn") `,fort (list  (foc "SubLQuoteFn") `,sym)) *prologMt* ) 
    `(defun-evaluation-defn ,sym ,args (ret (trace-progn . ,body)))))))
|#
    (defprolog numberp/1 (x cont)
      (when (numberp (deref x))
        (funcall cont)))
    
    (defprolog atom/1 (x cont)
      (when (atom (deref x))
        (funcall cont)))

(defprolog read/1 (exp cont)
  (if (book-unify exp (read))
      (funcall cont)))

(defprolog write/1 (exp cont)
  (write (deref-exp exp) :pretty t)
  (funcall cont))

(defprolog nl/0 (cont) (terpri) (funcall cont))

(defprolog =/2 (?arg1 ?arg2 cont)
  (if (book-unify ?arg1 ?arg2)
      (funcall cont)))

(defprolog ==/2 (?arg1 ?arg2 cont)
  "Are the two arguments EQUAL with no unification,
  but with dereferencing?  If so, succeed."
  (if (deref-equal ?arg1 ?arg2)
      (funcall cont)))

(defprolog call/1 (goal cont)
  "Try to prove goal by calling it."
  (deref goal)
  (apply (make-predicate (first goal) (length (args goal)))
         (append (args goal) (list cont))))


(defprolog not/1 (relation cont)
  "Negation by failure: If you can't prove G, then (not G) true."
  ;; Either way, undo the bindings.
  (with-undo-bindings
    (call/1 relation (cl-lambda () (return-from not/1 nil)))
    (funcall cont)))

(defprolog bagof/3 (exp goal result cont)
  "Find all solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (bagof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal (cl-lambda ()
		     ;; Bug fix by mdf0%shemesh@gte.com (Mark Feblowitz)
		     ;; on 25 Jan 1996; was deref-COPY
                     (push (deref-EXP exp) answers))) 
    (if (and (not (null answers))
             (book-unify result (nreverse answers)))
        (funcall cont))))


(defprolog setof/3 (exp goal result cont)
  "Find all unique solutions to GOAL, and for each solution,
  collect the value of EXP into the list RESULT."
  ;; Ex: Assume (p 1) (p 2) (p 3).  Then:
  ;;     (setof ?x (p ?x) ?l) ==> ?l = (1 2 3)
  (let ((answers nil))
    (call/1 goal (cl-lambda ()
                     (push (deref-copy exp) answers)))
    (if (and (not (null answers))
             (book-unify result (cl::delete-duplicates
                              answers
                              :test #'deref-equal)))
        (funcall cont))))

(defprolog is/2 (var exp cont)
  ;; Example: (is ?x (+ 3 (* ?y (+ ?z 4))))
  ;; Or even: (is (?x ?y ?x) (cons (first ?z) ?l))
  (if (and (not (find-if-anywhere #'unbound-book-variable-p exp))
           (book-unify var (eval (deref-exp exp))))
      (funcall cont)))

(defprolog var/1 (?arg1 cont)
  "Succeeds if ?arg1 is an uninstantiated variable."
  (if (unbound-book-variable-p ?arg1)
      (funcall cont)))

(defprolog lisp/2 (?result exp cont)
  "Apply (first exp) to (rest exp), and return the result."
  (if (and (consp (deref exp))
           (book-unify ?result (apply (first exp) (rest exp))))
      (funcall cont)))

(defprolog repeat/0 (cont) 
  (loop (funcall cont)))

(defun deref-copy (exp)
  "Copy the expression, replacing variables with new ones.
  The part without variables can be returned as is."
  ;; Bug fix by farquhar and norvig, 12/12/92.  Forgot to deref var.
  (sublis (mapcar (cl-lambda (var) (cons (deref var) (?)))
                  (unique-book-find-anywhere-if #'book-variable-p exp))
          exp))

(defun unbound-book-variable-p (exp)
  "Is EXP an unbound var?"
  (and (book-variable-p exp) (not (bound-p exp))))


(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))

#|
(defun book-unify (x y)
  "Destructively book-unify two expressions"
  (cond ((eql (deref x) (deref y)) t)
        ((book-variable-p x) (set-binding! x y))
        ((book-variable-p y) (set-binding! y x))
        ((and (consp x) (consp y))
         (and (book-unify (first x) (first y))
              (book-unify (rest x) (rest y))))
        (t nil)))

(defun set-binding! (var value)
  "Set var's binding to value.  Always succeeds (returns t)."
  (setf (var-binding var) value)
  t)

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))

;;;; File prologc.lisp: Final version of the compiler,
;;;; including all improvements from the chapter.

(defconstant unbound "Unbound")

(cl::defstruct var name (binding unbound))

(defun bound-p (var) (not (eq (var-binding var) unbound)))

(defmacro deref (exp)
  "Follow pointers for bound variables."
  `(progn (loop while (and (book-variable-p ,exp) (bound-p ,exp))
             do (setf ,exp (var-binding ,exp)))
          ,exp))

(defun book-unify (x y)
  "Destructively book-unify two expressions"
  (cond ((eql (deref x) (deref y)) t)
        ((book-variable-p x) (set-binding! x y))
        ((book-variable-p y) (set-binding! y x))
        ((and (consp x) (consp y))
         (and (book-unify (first x) (first y))
              (book-unify (rest x) (rest y))))
        (t nil)))

(defun set-binding! (var value)
  "Set var's binding to value.  Always succeeds (returns t)."
  (setf (var-binding var) value)
  t)

(defun print-var (var stream depth)
  (if (or (and *print-level*
               (>= depth *print-level*))
          (book-variable-p (deref var)))
      (format stream "?~a" (var-name var))
      (write var :stream stream)))

(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t))

(defun set-binding! (var value)
  "Set var's binding to value, after saving the variable
  in the trail.  Always returns t."
  (unless (eq var value)
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun undo-bindings! (old-trail)
  "Undo all bindings back to a given point in the trail."
  (loop until (= (fill-pointer *trail*) old-trail)
     do (setf (var-binding (vector-pop *trail*)) unbound)))

(defvar *var-counter* 0)

;;dmiles (cl::defstruct (var (:constructor ? ())(:print-function print-var))(name (incf *var-counter*))(binding unbound))

(defun prolog-compile (symbol &optional (clauses (get-db-clauses symbol)))
  "Compile a symbol; make a separate function for each arity."
  (unless (null clauses)
    (let ((arity (relation-arity (clause-head (first clauses)))))
      ;; Compile the clauses with this arity
      (compile-predicate
        symbol arity (clauses-with-arity clauses #'= arity))
      ;; Compile all the clauses with any other arity
      (prolog-compile
        symbol (clauses-with-arity clauses #'/= arity)))))

(defun clauses-with-arity (clauses test arity)
  "Return all clauses whose head has given arity."
  (book-find-all arity clauses :key (cl-lambda (clause) (relation-arity (clause-head clause))) :test test))

(defun relation-arity (relation)
  "The number of arguments to a relation.
  Example: (relation-arity '(p a b c)) => 3"
  (length (args relation)))

(defun args (x) "The arguments of a relation" (rest x))

(defun make-parameters (arity)
  "Return the list (?arg1 ?arg2 ... ?arg-arity)"
  (loop for i from 1 to arity
        collect (new-symbol '?arg i)))

(defun make-predicate (symbol arity)
  "Return the symbol: symbol/arity"
  (book-symbol symbol '/ arity))

(defun make-= (x y) `(= ,x ,y))

(defun compile-call (predicate args cont)
  "Compile a call to a prolog predicate."
  `(,predicate ,@args ,cont))

(defun prolog-compiler-macro (name)
  "Fetch the compiler macro for a Prolog predicate."
  ;; Note NAME is the raw name, not the name/arity
  (get name 'prolog-compiler-macro))

(defmacro def-prolog-compiler-macro (name arglist &body body)
  "Define a compiler macro for Prolog."
  `(setf (get ',name 'prolog-compiler-macro)
         (cl-lambda ,arglist .,body)))

(defun compile-arg (arg)
  "Generate code for an argument to a goal in the body."
  (cond ((book-variable-p arg) arg)
        ((not (has-book-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar #'compile-arg arg)))
        (t `(cons ,(compile-arg (first arg))
                  ,(compile-arg (rest arg))))))

(defun has-book-variable-p (x)
  "Is there a variable anywhere in the expression x?"
  (find-if-anywhere #'book-variable-p x))

(defun proper-listp (x)
  "Is x a proper (non-dotted) list?"
  (or (null x)
      (and (consp x) (proper-listp (rest x)))))

(defun maybe-add-undo-bindings (compiled-exps)
  "Undo any bindings that need undoing.
  If there are any, bind the trail before we start."
  (if (length=1 compiled-exps)
      compiled-exps
      `((let ((old-trail (fill-pointer *trail*)))
          ,(first compiled-exps)
          ,@(loop for exp in (rest compiled-exps)
                  collect '(undo-bindings! old-trail)
                  collect exp)))))

(defun bind-unbound-vars (parameters exp)
  "If there are any variables in exp (besides the parameters)
  then bind them to new vars."
  (let ((exp-vars (set-difference (variables-in exp)
                                  parameters)))
    (if exp-vars
        `(let ,(mapcar (cl-lambda (var) `(,var (?)))
                       exp-vars)
           ,exp)
        exp)))

(defmacro <- (&rest clause)
  "Add a clause to the data base."
  `(add-clause ',(make-anonymous clause)))

(defun make-anonymous (exp &optional
                       (anon-vars (anonymous-variables-in exp)))
  "Replace variables that are only used once with ?."
  (cond ((consp exp)
         (reuse-cons (make-anonymous (first exp) anon-vars)
                     (make-anonymous (rest exp) anon-vars)
                     exp))
        ((member exp anon-vars) '?)
        (t exp)))

(defun anonymous-variables-in (tree)
  "Return a list of all variables that occur only once in tree."
  (values (anon-vars-in tree nil nil)))
 
(defun anon-vars-in (tree seen-once seen-more)
  "Walk the data structure TREE, returning a list of variabless
   seen once, and a list of variables seen more than once."
  (cond
    ((consp tree)
     (multiple-value-bind (new-seen-once new-seen-more)
         (anon-vars-in (first tree) seen-once seen-more)
       (anon-vars-in (rest tree) new-seen-once new-seen-more)))
    ((not (book-variable-p tree)) (values seen-once seen-more))
    ((member tree seen-once)
     (values (delete tree seen-once) (cons tree seen-more)))
    ((member tree seen-more)
     (values seen-once seen-more))
    (t (values (cons tree seen-once) seen-more))))

(defun compile-book-unify (x y bindings)
  "Return 2 values: code to test if x and y book-unify,
  and a new binding list."
  (cond
    ;; book-unify constants and conses:                       ; Case
    ((not (or (has-book-variable-p x) (has-book-variable-p y)))    ; 1,2
     (values (equal x y) bindings))
    ((and (consp x) (consp y))                           ; 3
     (multiple-value-bind (code1 bindings1)
         (compile-book-unify (first x) (first y) bindings)
       (multiple-value-bind (code2 bindings2)
           (compile-book-unify (rest x) (rest y) bindings1)
         (values (compile-if code1 code2) bindings2))))
    ;; Here x or y is a variable.  Pick the right one:
    ((book-variable-p x) (compile-book-unify-variable x y bindings))
    (t              (compile-book-unify-variable y x bindings))))

(defun compile-if (pred then-part)
  "Compile a Lisp IF form. No else-part allowed."
  (case pred
    ((t) then-part)
    ((nil) nil)
    (otherwise `(if ,pred ,then-part))))

(defun compile-book-unify-variable (x y bindings)
  "X is a variable, and Y may be."
  (let* ((xb (follow-binding x bindings))
         (x1 (if xb (cdr xb) x))
         (yb (if (book-variable-p y) (follow-binding y bindings)))
         (y1 (if yb (cdr yb) y)))
    (cond                                                 ; Case:
      ((or (eq x '?) (eq y '?)) (values t bindings))      ; 12
      ((not (and (equal x x1) (equal y y1)))              ; deref
       (compile-book-unify x1 y1 bindings))
      ((book-find-anywhere x1 y1) (values nil bindings))       ; 11
      ((consp y1)                                         ; 7,10
       (values `(book-unify ,x1 ,(compile-arg y1 bindings))
               (bind-variables-in y1 bindings)))
      ((not (null xb))
       ;; i.e. x is an ?arg variable
       (if (and (book-variable-p y1) (null yb))
           (values 't (extend-bindings y1 x1 bindings))   ; 4
           (values `(book-unify ,x1 ,(compile-arg y1 bindings))
                   (extend-bindings x1 y1 bindings))))    ; 5,6
      ((not (null yb))
       (compile-book-unify-variable y1 x1 bindings))
      (t (values 't (extend-bindings x1 y1 bindings)))))) ; 8,9

(defun bind-variables-in (exp bindings)
  "Bind all variables in exp to themselves, and add that to
  bindings (except for variables already bound)."
  (dolist (var (variables-in exp))
    (unless (get-binding var bindings)
      (setf bindings (extend-bindings var var bindings))))
  bindings)

(defun follow-binding (var bindings)
  "Get the ultimate binding of var according to bindings."
  (let ((b (get-binding var bindings)))
    (if (eq (car b) (cdr b))
        b
        (or (follow-binding (cdr b) bindings)
            b))))

(defun compile-arg (arg bindings)
  "Generate code for an argument to a goal in the body."
  (cond ((eq arg '?) '(?))
        ((book-variable-p arg)
         (let ((binding (get-binding arg bindings)))
           (if (and (not (null binding))
                    (not (eq arg (binding-val binding))))
             (compile-arg (binding-val binding) bindings)
             arg)))
        ((not (find-if-anywhere #'book-variable-p arg)) `',arg)
        ((proper-listp arg)
         `(list .,(mapcar (cl-lambda (a) (compile-arg a bindings))
                          arg)))
        (t `(cons ,(compile-arg (first arg) bindings)
                  ,(compile-arg (rest arg) bindings)))))

(defun bind-new-variables (bindings goal)
  "Extend bindings to include any unbound variables in goal."
  (let ((variables (remove-if (cl-lambda (v) (assoc v bindings))
                              (variables-in goal))))
    (nconc (mapcar #'self-cons variables) bindings)))

(defun self-cons (x) (cons x x))

(def-prolog-compiler-macro = (goal body cont bindings)
  "Compile a goal which is a call to =."
  (let ((args (args goal)))
    (if (/= (length args) 2)
        :pass ;; decline to handle this goal
        (multiple-value-bind (code1 bindings1)
            (compile-book-unify (first args) (second args) bindings)
          (compile-if
            code1
            (compile-body body cont bindings1))))))

(defun compile-clause (parms clause cont)
  "Transform away the head, and compile the resulting body."
  (bind-unbound-vars       
    parms                  
    (compile-body
      (nconc
        (mapcar #'make-= parms (args (clause-head clause)))
        (clause-body clause))
      cont
      (mapcar #'self-cons parms))))                    ;***

(defvar *uncompiled* nil 
  "Prolog symbols that have not been compiled.")

(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate."
  ;; The predicate must be a non-variable symbol.
  (let ((pred (get-predicate (clause-head clause))))
    (assert (and (symbolp pred) (not (book-variable-p pred))))
    (pushnew pred *db-predicates*)
    (pushnew pred *uncompiled*)                          ;***
    (setf (get pred 'clauses)
          (nconc (get-db-clauses pred) (list clause)))
    pred))

(defun top-level-prove (goals)
  "Prove the list of goals by compiling and calling it."
  ;; First redefine top-level-query
  (clear-predicate 'top-level-query)
  (clet ((vars (delete '? (variables-in goals))))
    (add-clause `((top-level-query)
                  ,@goals
                  (show-prolog-vars ,(mapcar #'symbol-name vars)
                                    ,vars))))
  ;; Now run it
  (run-prolog 'top-level-query/0 #'book-ignore)
  (format t "~&No.")
  (values))

(defun run-prolog (procedure cont)
  "Run a 0-ary prolog procedure with a given continuation."
  ;; First compile anything else that needs it
  (prolog-compile-symbols)
  ;; Reset the trail and the new variable counter
  (csetf (fill-pointer *trail*) 0)
  (csetf *var-counter* 0)
  ;; Finally, call the query
  (catch 'top-level-prove
    (funcall procedure cont)))

(defun prolog-compile-symbols (&optional (symbols *uncompiled*))
  "Compile a list of Prolog symbols.
  By default, the list is all symbols that need it."
  (mapc #'prolog-compile symbols)
  (setf *uncompiled* (set-difference *uncompiled* symbols)))

(defun book-ignore (&rest args)
  (declare (ignore args))
  nil)

(defprolog show-prolog-vars/2 (var-names vars cont)
  "Display the variables, and prompt the user to see
  if we should continue.  If not, return to the top level."
  (if (null vars)
      (format t "~&Yes")
      (loop for name in var-names
            for var in vars do
            (format t "~&~a = ~a" name (deref-exp var))))
  (if (continue-p)
      (funcall cont)
      (throw 'top-level-prove nil)))

(defun deref-exp (exp)
  "Build something equivalent to EXP with variables dereferenced."
  (if (atom (deref exp))
      exp
      (reuse-cons
        (deref-exp (first exp))
        (deref-exp (rest exp))
        exp)))

(defvar *predicate* nil
  "The Prolog predicate currently being compiled")

(defun compile-predicate (symbol arity clauses)
  "Compile all the clauses for a given symbol/arity
  into a single LISP function."
  (let ((*predicate* (make-predicate symbol arity))    ;***
        (parameters (make-parameters arity)))
    (compile
     (eval
      `(defun ,*predicate* (,@parameters cont)
	.,(maybe-add-undo-bindings
	   (mapcar (cl-lambda (clause)
		       (compile-clause parameters clause 'cont))
	    clauses)))))))

(defun compile-body (body cont bindings)
  "Compile the body of a clause."
  (cond
    ((null body)
     `(funcall ,cont))
    ((eq (first body) '!)                              ;*** 
     `(progn ,(compile-body (rest body) cont bindings) ;***
             (return-from ,*predicate* nil)))          ;***
    (t (let* ((goal (first body))
              (macro (prolog-compiler-macro (get-predicate goal)))
              (macro-val (if macro 
                             (funcall macro goal (rest body) 
                                      cont bindings))))
        (if (and macro (not (eq macro-val :pass)))
            macro-val
            `(,(make-predicate (get-predicate goal)
                               (relation-arity goal))
              ,@(mapcar (cl-lambda (arg)
                            (compile-arg arg bindings))
                        (args goal))
              ,(if (null (rest body))
                   cont
                   `(cl-lambda ()
                        ,(compile-body 
                           (rest body) cont
                           (bind-new-variables bindings goal))))))))))
|#
;;;; End prologc.lisp: Final version of the compiler,

(defvar *loaded-once* nil)

(unless *loaded-once*
    (setq *loaded-once* t)

    (force-print "loading preds")

    (eval `(<- (,*prologFn* "member" ?item (,*prologFn* "." ?item  ?rest))))
    (eval `(<- (,*prologFn* "member" ?item (,*prologFn* "." ??x ?rest)) (,*prologFn* "member" ?item ?rest)))

    (<- (if ?test ?then) (if ?then ?else (fail)))
    
    (<- (if ?test ?then ?else)
        (call ?test)
        !
        (call ?then))
    
    (<- (if ?test ?then ?else)
        (call ?else))
    
    (<- (member ?item (?item . ?rest)))
    (<- (member ?item (?x . ?rest)) (member ?item ?rest))
    
    (<- (length () 0))  (<- (length (?x . ?y) (1+ ?n)) (length ?y ?n))
    
    (defun numberp/1 (x cont)
      (when (numberp (deref x))
        (funcall cont)))
    
    (defun atom/1 (x cont)
      (when (atom (deref x))
        (funcall cont)))
    
    (<- (or ?a ??b) (call ?a))
    (<- (or ??a ?b) (call ?b))
    
    (<- (|eq| ?X ?X))
    
    (<- (foo 1))
    (<- (foo 2))
    
    (<- (and ?a ?b) (call ?a) (call ?b))
    
    (<- (append () ?X ?X))
    (<- (append (?X . ?L1) ?L2 (?X . ?L3)) (append ?L1 ?L2 ?L3))
)

#|
e
(remove-comments ";; lisp comment threre")
c
%% prolog comment
b
/* c comment */
ok
")
(load "cb-prolog.lisp")
|#
(defun remove-comments (str)
 #|
(setq str "sdfsdf")
(setq str (string-remove-segment (string-concat str (string #\Newline)) "%" (string #\Newline)))
(setq str (string-remove-segment (string-concat str (string #\Newline)) "/*" "*/"))
(setq str (string-remove-segment (string-concat str (string #\Newline)) (string-concat (string #\Newline) " ;") (string #\Newline)))
(setq str (string-remove-segment (string-concat str (string #\Newline)) "#|" "|#"))
(setq str (string-trim *WHITESPACE-CHARS* str))
str)|#
  (string-trim *WHITESPACE-CHARS* (string-remove-segment 
     (string-concat (string-remove-segment (string-concat 
       (string-remove-segment (string-concat (string-remove-segment 
        (string-concat str (string #\Newline)) "%" (string #\Newline)) (string #\Newline)) "/*" "*/") (string #\Newline)) 
          (string-concat (string #\Newline) ";") (string #\Newline)) (string #\Newline)) "#|" "|#")))


(defun string-to-term (str &optional (fun #'read_term)) 
 (setq str (remove-comments str))
 (let ((len (length str)))
  (if (= len 0) (return-from string-to-term :EOF)
   (let* ((*read-sofar* 0)
      (*standard-input* (MAKE-STRING-INPUT-STREAM str))
      (res (funcall fun (rch))))
    (RETURN-from string-to-term (values res *read-sofar* (substring str *read-sofar*)))))))

;; TODO - make prolog reader

(defun prolog-read (stream varinfo)
 (THROW-UNEVALUATABLE-ON-ERROR 
  (if (stringp stream) (string-to-term stream)
   (let* ((*standard-input* (if (streamp stream) stream *standard-input*))) (list  (read_term (rch)) varinfo)))))


(defun search-obey-escapes (seq1 str &optional (test #'eql) (key #'identity) (start1 0) end1 (start2 0) end2) 
 (let ((start (search seq1 str test key start1 end1 start2 end2)))
  (if (or (not (numberp start))(= start 0)) start
   (if (not (member (char str (- start 1)) '(#\\ #\' #\`) #'char=)) start
    (search-obey-escapes seq1 str test key start1 end1 (+ 1 start) end2)))))

;(defun impl (l) (intern (map 'string #'code-char l)))
(defun implstring (l) (let ((codelist (mapcar #'code-char l))(code (make-string (length l)))) 
   (do ((trm 0 (+ 1 trm))) ((= trm (length l)) (return-from implstring code )) (set-char code trm (nth trm codelist)))))
(defun impl (l) (intern (implstring l)))

;(defun implode (lch) (intern (map 'string #'identity lch)))
(defun implode (l) (let ((codelist (mapcar #'identity l))(code (make-string (length l)))) 
  (do ((trm 0 (+ 1 trm))) ((= trm (length l)) (return-from implode (intern code ))) (set-char code trm (nth trm codelist)))))

;(defun expl (at) (map 'list #'char-code (string at)))
(defun expl (l) (let ((l (string l))(codelist ()))
         (do ((trm 0 (+ 1 trm))) ((= trm (length l)) (return-from expl codelist)) (setq codelist (append codelist (list  (char-code (char l trm))))))))

#|
(defun read_prompt (&optional varinfo) 
(terpri)
(format t "| ?- ") (force-output) ;(gc)
(let ((retval (read_code_tail)))
 (force-print retval) 
 retval))

(defun read_code_tail ()
 (let ((trm (read_comma_list (rch) #\.)))
  (cons (cons (length *lvarloc) (length *lvarglob)) (append (ccc trm) (list  '(|true|))))))
(defun banner () (terpri)(format t "Prolog REPL~%")(terpri)(force-output) T)

(defun l () (format t "Back to Prolog top-level~%")(force-output)(prolog_repl))
|#

(defvar *saved-standard-input* *standard-input*)

(defvar *saved-standard-output* *standard-output*)
(defvar *saved-terminal-io* *terminal-io*)


(defvar *read-sofar* 0)

;;(defun chars-ready (stream) (let (read-char-no-hang stream))
;; end-of-stream exits true of there are no chars to peek at
(defun end-of-stream ()
 (null (peek-char nil *standard-input* nil)))

;; peek-char that can emit #\Null
(defun pch ()
 ;;(read-char-no-hang)
 (if (end-of-stream) (code-char 0)(peek-char)))

(defvar *LAST-READ-CHAR* '(#\Null))
(setq *LAST-READ-CHAR* '(#\Null))
(defvar *LOCAL-READ-CHAR* ())
(setq *LOCAL-READ-CHAR* ())


(defun applicable-function (usefnstr &optional symbol (requestpack *package*) )  
  (when (symbolp usefnstr) (setq usefnstr (symbol-name usefnstr)))
  (setq symbol (find-symbol (concatenate 'string  "CL-" usefnstr)))
  (if (fboundp symbol) (return symbol))
  (setq symbol (find-symbol usefnstr))
  (if (fboundp symbol) (ret symbol))
  (unless symbol (setq symbol (intern usefnstr)))
  (format t "do function ~A~%" symbol)
  (print (list  usefnstr 'to symbol 'fboundp= (fboundp symbol) 'as symbol 'from (symbol-package symbol)))
  symbol)



(DEFINE MY-CONCAT (&REST REST) (RET (APPLY #'CONCAT REST)))

(DEFINE BREAK-STRING-AT (STRING BREAK-CHAR) 
  (FUNLESS (STRING= STRING "") 
    (CLET ((CHARAT (POSITION BREAK-CHAR STRING)))(RET (FIF CHARAT (CONS (SUBSEQ STRING 0 CHARAT) 
      (BREAK-STRING-AT (SUBSEQ STRING (+ 1 CHARAT)) BREAK-CHAR)) (list  STRING))))))

(DEFINE BINDINGS-FOR (PATTERN)
  (CLET ((COLLECT ()))
   (DOLIST (VAR (VARIABLES-IN PATTERN))
	 (SETQ COLLECT (APPEND COLLECT (list  (CDR (ASSOC VAR *BINDINGS*))))))
    (RET COLLECT)))

(DEFINE COMPILE-RULES (RULES VAR)
 ;; "A RULES IS OF THE FORM (PAT CODE) WHERE CODE MAY REFERENCE VARS IN PAT."
 (CLET ((COLLECT ()))
  (DOLIST (PATTERN+CONSEQUENT RULES)
    (CSETQ COLLECT (APPEND COLLECT (list  (COMPILE-RULE (FIRST PATTERN+CONSEQUENT)(SECOND PATTERN+CONSEQUENT) VAR)))))
  (RET (CREDUCE #'MERGE-CODE COLLECT))))

(DEFINE EXPLODE (STRING) 
   (FUNLESS (STRING= STRING "")  
     (CLET ((RESULT ())(LEN (LENGTH STRING)))
      (CDO ((NDX (- LEN 1) (- NDX 1))(RESULT (CONS (CHAR STRING NDX) RESULT)(CONS (CHAR STRING NDX) RESULT))) ((= NDX 0)(RET RESULT))))))



;(DEFINE IMPL (L) (CLET ((CODELIST (MAPCAR #'CODE-CHAR L))) (INTERN (STRING CODELIST ))))
(DEFINE IMPL (L) (CLET ((CODELIST (MAPCAR #'CODE-CHAR L))(NEWSTR (MAKE-STRING (LENGTH L)))) 
	(CDO ((X 0 (+ 1 X))) ((= X (LENGTH L)) (RET (INTERN NEWSTR ))) (SET-CHAR NEWSTR X (NTH X CODELIST)))))
(DEFINE IMPLODE (L) (CLET ((CODELIST (MAPCAR #'IDENTITY L))(NEWSTR (MAKE-STRING (LENGTH L)))) 
	(CDO ((X 0 (+ 1 X))) ((= X (LENGTH L)) (RET (INTERN NEWSTR  ))) (SET-CHAR NEWSTR X (NTH X CODELIST)))))

(DEFINE IMPLODE (L) (CLET ((CODELIST L)(NEWSTR (MAKE-STRING (LENGTH L)))) 
	(CDO ((X 0 (+ 1 X))) ((= X (LENGTH L)) (RET (INTERN NEWSTR  ))) (SET-CHAR NEWSTR X (NTH X CODELIST)))))


(DEFINE EXPL (L) (CLET ((L (STRING L))(CODELIST ()))
                  (CDO ((TRM 0 (+ 1 TRM))) ((= TRM (LENGTH L)) 
                    (RET CODELIST)) (CSETQ CODELIST (APPEND CODELIST (list  (CHAR-CODE (CHAR L TRM))))))))




(defun uch (c)
 "(unread-char c) that accepts #\Null"
 ;;;(format t "unread: ~S stack: ~S~%" c *LAST-READ-CHAR*)
 (if (char= c #\Null) (ret NIL))
;; (pop *LAST-READ-CHAR*)
 ;;(push c *LOCAL-READ-CHAR*)
 (decf *read-sofar*)
 (unread-char c)(return-from uch t))

(defun nch () "(read-char) that can emit #\Null"
 (when (end-of-stream) (return-from nch (code-char 0)))
 (incf *read-sofar*)
 (let ((c (read-char))) 
    (push c *LAST-READ-CHAR*) c))

(defun rch () "(read-char) (non-white) that can emit #\Null"
 (if (end-of-stream) (return-from rch (code-char 0)))
 (let ((ch (nch))) 
  (RETURN-from rch (if (member ch *WHITESPACE-CHARS*) (rch) ch ))))

(defun charvarp (ch) (ret (or (upper-case-p ch) (char= ch #\_) (char= ch #\?) )))
(defun atomcharp (ch) (ret (or (alphanumericp ch) (charvarp ch))))

;;(defun read_number (ch)(return-from read_number (do ((v (digit-char-p ch) (+ (* v 10) (digit-char-p (nch)))))((not (digit-char-p (pch))) v))))
(defun read_number (&optional (ch (rch)))
  (trace-lisp (uch ch))
  (read))

;; (delete-if (cl-lambda (x) (ret (member x '(#\. #\! #\( #\) #\? #\, ))))  "This is a test! Really more than a sentence(s).")
(defun read_atom (&optional (ch (rch)))
 (if (char= ch #\Null ) (ret :EOF))
 (let ((op (infix-op ch))) (if op (ret op)))
 (return-from read_atom 
  (do ((lch (list  ch) (push (nch) lch)))
    ((not (atomcharp (pch))) (implode (reverse lch))))))

(defun read_qatom (&optional (ch (rch)))
 (return-from read_qatom (do ((lch (list  ch) (push (nch) lch)))
      ((char= (pch) #\') (nch) (implode (reverse lch))))))


(defun do_char_list (trm)
 (return-from do_char_list (if (atom trm) trm (prolog-make-cons (car trm) (do_char_list (cdr trm))))))
#|(defun read_string (&optional (ch (rch)))
(return-from read_string (do ((lch (list  (char-code ch)) 
         (push (char-code (nch)) lch)))
      ((char= (pch) *DOUBLE-QUOTE-CHAR* ) (nch ) (do_char_list (reverse lch)))))) 
|#
(defun read_string (&optional (ch (nch)))
 (uch ch)(read))

(defun skipto (char &optional prestr)
 (if (null prestr) (setq prestr ""))
 (let ((ch (nch)))
  (if (or (char= ch char)(char= ch #\Null)) prestr (string-concat (string ch) (skipto char prestr)))))

(defun make_var_ref (trm)
 (unless (CYC-VAR? trm) (setq trm (MAKE-EL-VAR trm)))
 (setq trm (corRECT-VARIABLE trm ))
 ;; (force-print (list  :make_var_ref trm))
 (unless (member trm *lvarglob)(pushnew trm *lvarloc))
 (ret trm))

(defun read_atomic (&optional (ch (nch)) varinfo)
  (when (char= ch #\Null) (return-from read_atomic :EOF))
 ;;operators 
  ;; Prolog Numbers
  (when (digit-char-p ch) (return-from read_atomic (read_number ch)))
  ;;comments
  (when (char= ch #\% ) (skipto #\Newline "%")(return-from read_atomic (read_atomic (rch))))
  (when (and (char= ch #\/ ) (char= (pch) #\* )) (skipto #\/ "/")(return-from read_atomic (read_atomic (rch))))
  ;; Prolog lists [ ... ]
  (when (char= ch #\[ ) (return-from read_atomic (read_list (rch))))
  ;; Prolog vectors { ... }
  (when (char= ch #\{) (return-from read_atomic (cons '|{}| (read_comma_list (rch) #\}))))
  ;; Prolog conjs ( ... )
  ;;(when (char= ch #\() (return-from read_atomic (cons '|{}| (read_comma_list (rch) #\)))))
  ;; Prolog chars `A
  (when (char= ch #\` )(return-from read_atomic (rch)))
  ;; Prolog quoted atoms 'Abc'
  (when (char= ch #\' ) (return-from read_atomic (read_qatom (nch))))
  ;; Prolog variables Abc
  (when (charvarp ch) (return-from read_atomic (make_var_ref (read_atom ch))))
  (when (char= ch *DOUBLE-QUOTE-CHAR* ) (return-from read_atomic (prolog-make-string (read_string ch))))
  (when (and (char= ch #\# )(member (pch) '(#\S )))(nch)(return-from read_atomic (read)))
  ;;(when (and (char= ch #\# )(member (pch) '(#\$ #\\ #\( )))(unread-char ch)(return-from read_atomic (read)))
  (when (char= ch #\# ) (uch ch) (return-from read_atomic (read)))
  (read_atom ch))

(defun prolog-make-string (str)(ret str))

(defun read_term (&optional (ch (rch)) varinfo)
 (let ((trm (read_atomic ch)) (c (rch)))
  ;;(force-print c)
  (when (member c '(#\Null #\.)) (return-from read_term trm))
  (when (char= c #\( ) 
    (let ( (args (read_comma_list (rch) #\) ) ))
     (setq trm (ret (cons *prologFn* (cons (prolog-make-pred trm) (prolog-make-cdr args)))))
     (setq c (rch))))
  (when (member c '(#\Null #\.)) (return-from read_term trm))
  ;;(let ((op (infix-op c))) (if op (return-from read_term (list  op trm (read_term (rch) )))))
  ;;(force-print c)
  (uch c) trm))

#|
countSuccess(Call,_):-flag(Call,_,0),fail.
countSuccess(Call,_):-call(Call),flag(Call,N,N+1),fail.
countSuccess(Call,N):-flag(Call,N,0).
|#


#|
CYC(65): (string-to-term "member(ITEM,[X|REST])")
(prolog:clause (prolog:FunctorFn "member" 2) ?ITEM (|.| ?X ?REST))
CYC(66): (string-to-term "a,b")
(|,| |a| |b|)
CYC(67): (string-to-term "a:-b")
(:- |a| |b|)
CYC(68): (string-to-term "a(X):-b(X)")
(:- (prolog:clause (prolog:FunctorFn "a" 1) ?X) (prolog:clause (prolog:FunctorFn "b" 1) ?X))
CYC(69): (string-to-term "a([X]):-b(X)")
(:- (prolog:clause (prolog:FunctorFn "a" 1) (|.| ?X NIL)) (prolog:clause (prolog:FunctorFn "b" 1) ?X))
CYC(70): (string-to-term "a([X|B])")
(prolog:clause (prolog:FunctorFn "a" 1) (|.| ?X ?B))
CYC(71): (string-to-term "a([X|B]):-true")
(prolog:clause (prolog:FunctorFn "a" 1) (|.| ?X ?B))
CYC(71): (string-to-term "[X|B]:-true")
CYC(83): (string-to-term "")
:EOF
(string-to-term "[]") => NIL
(string-to-term "[1]") => (|.| 1 NIL)

|#

(defun charstring (&rest chars) 
 ;; (print chars)(force-output)
 (ret (implstring (mapcar #'char-code chars))))

;; this one ERROR "Attempt to unread too many characters to #<STREAM :TWO-WAY (open) 4000250238>." DURRING: '(RETURN-FROM READ_ATOMIC (READ_NUMBER CH))
(defun infix-op-no (c &optional list)
 (if (null list) (setq list (append *xfy* *xfx* *fx* *fy*)))
 (let ((opstr "")(nc1 (nch)))
  (setq opstr (string-concat c nc1 (peek-char)))
  (if (member opstr list #'equal) (ret (values (intern opstr) (read-char))))
  (setq opstr (string-concat c nc1))
  (if (member opstr list #'equal) (ret (intern opstr)))
  (uch nc1)
  (setq opstr (string-concat c))
  (if (member opstr list #'equal) (ret (intern opstr)))))

;; temp workarround
(defun infix-op (c &optional list)
 (unless list (setq list (append *xfy* *xfx* *fx* *fy*)))
 (let ((opstr ""))
  (setq opstr (string-concat c (pch)))
  (pwhen (member opstr list #'equal) (ret (values (intern opstr) (read-char))))
  (setq opstr (string-concat c))
  (pwhen (member opstr list #'equal) (ret (values (intern opstr))))))

;; (load "prolog_reader.lisp")(read_term (rch)) 
;; (load "prolog_reader.lisp")(prolog-read)
(defun read_comma_list (ch lchar)
 (if (member ch (cons lchar '(#\Null #\.))) (ret ()))
 (let ((*xfy* (remove "," *xfy* #'equal))(*xfx* (remove "," *xfx* #'equal))
    (trm (read_term ch)) (c (rch)))
  (if (char= c lchar) (ret (list  trm)))
  (if (char= c #\,) (ret (cons trm (read_comma_list (rch) lchar))))
  (uch c) (ret (list  trm))))


#|
CYC(91): (string-to-term "[A,B]")
(|.| ?A (|.| ?B NIL))
(string-to-term "[1.0,2.0 ]")
(|.| 1.0 (|.| 2.0 NIL))
|#
(defun read_list (&optional (ch (rch)) frame)
 (if (char= ch #\])
   *prologNil*
  (let ((*xfy* (remove "," *xfy* #'equal))(*xfx* (remove "," *xfx* #'equal))(trm (read_term ch)))
   (case (rch)
    (#\, (prolog-make-cons trm (read_list (rch))))
    (#\| (prog1 (prolog-make-cons trm (read_term (rch))) (rch))) 
    (#\] (prolog-make-cons trm *prologNil*))))))


(defun ccc (l)
 (ret 
  (if (atom l) 
    (if (member l *lvarloc) 
      (cons 'L (position l *lvarloc))
     (if (member l *lvarglob) 
       (cons 'G (position l *lvarglob)) l))
   (if (eq (car l) '! ) (list  '! (length *lvarloc))
    (cons (ccc (car l)) (ccc (cdr l)))))))

; Version 3
; lecteur.lsp
;
(defun unsafe? (trm h q) (ret (and (member trm q) (not (member trm h)))))


(defun maj_locglob (h q)
 (ret (mapc (cl-lambda (trm) 
         (when (unsafe? trm h q)
 (setq *lvarloc (delete trm *lvarloc))
 (push trm *lvarglob))) 
     *lvarloc)))

(defun read_code_cl ()
 (let ((*lvarloc ()) (*lvarglob ()))
  (ret (let ((trm (read_term (rch))))
      (maj_locglob (car trm) (car (last trm)))
      (cons (cons (length *lvarloc) (length *lvarglob)) (ccc trm))))))

(defun prolog-assert-now (sent &optional (mt *prologmt*))
 (clet ((found (car (FIND-ASSERTIONS-CYCL sent mt))))
 (force-output)
  (fif 
    (assertion-p found) 
     (print found)
     (if (eval (print (list  'ke-assert-now (quotify sent) (quotify mt) :monotonic :FORWARD)))
         (car (FIND-ASSERTIONS-CYCL sent mt))
         (progn (print (list :ERROR (HL-EXPLANATION-OF-WHY-NOT-WFF sent mt)))(force-output) nil)))))

(defun prolog-sentences-to-string (trms )
 (ret (if (null trms) " " (string-concat (prolog-to-string (car trms)) "." (make-string 2 #\Newline) (prolog-sentences-to-string (cdr trms))))))

(defun prolog-need-quote (str) (let ((ch (char str 0)))
        (if (member ch '(#\: #\' #\` #\#) #'char=) (ret nil))
        (ret (or (not (lower-case-p ch)) (find #\: str)))))


(defun prolog-list-to-string (fun trm &optional (begstr "") (endstr "") (nullstr "")) 
 (if (null trm ) (ret (string-concat begstr endstr)))
 (if (atom trm ) (ret (funcall fun trm)))
 (ret (string-concat begstr (funcall fun (car trm)) (prolog-cdr-to-string fun (cdr trm) "," "|" nullstr) endstr )))

(defun prolog-cdr-to-string (fun trm &optional (commastr ",")(barstr "|") (nullstr "")) 
 (cond
  ((null trm) (ret nullstr))
  ((consp trm) (ret (string-concat commastr (funcall fun (car trm)) 
         (prolog-cdr-to-string fun (cdr trm) commastr barstr nullstr))))
  (t (ret (string-concat barstr (funcall fun trm))))))


(defun prolog-to-name (trm)
 (cond
  ((stringp trm) (ret (if (prolog-need-quote trm) (string-concat "'" trm "'") trm)))
  ((constant-p trm) (ret (string-concat "#$" (constant-name trm) "")))
  (t (ret (prolog-to-name (prolog-to-string trm))))))


(defun prolog-to-string (trm )
 (cond 
  ((nart-p trm) (ret (string-concat "nart(" (prolog-to-string (nart-el-formula trm)) ")")))
  ((assertion-p trm) (ret (prolog-to-string (assertion-el-ist-formula trm))))
  ((equal trm *prologNil*) (ret "[]"))
  ((null trm) (ret "[]"))
  ((EL-variable-p trm) (ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))
  ((HL-variable-p trm) (ret (SUBSTITUTE #\_ #\- (EL-VAR-NAME-WITHOUT-PREFIX trm))))
  ((constant-p trm) (ret (string-concat "#$" (constant-name trm) "")))  
  ((stringp trm) (ret (write-to-string trm)))
  ((numberp trm) (ret (write-to-string trm)))
  ((keywordp trm)(ret (prolog-to-name (symbol-name trm))))
  ((symbolp trm) (ret (prolog-to-name (symbol-name trm))))
  ((not (consp trm)) (ret (prolog-to-name (write-to-string trm))))
  (t 
  (let ((pred (elt trm 0)))
    (setq trm (cdr trm))
    (when (equal pred *ist*) (ret (prolog-to-string (cadr trm))))
    (when (equal pred *prologFn*) (setq pred (car trm)) (setq trm (cdr trm)))
    (unless trm (ret (prolog-to-name pred)))
    (when (equal pred ".") 
        (let* ((lcdr (second trm)))
           (if (prolog-consp lcdr)
               (ret (string-concat "[" (prolog-to-string (first trm)) "|" (prolog-to-string lcdr) "]" ))
               (ret (string-concat "[" (prolog-to-string (first trm)) "|" (prolog-to-string lcdr) "]" )))))
    (when (member pred *xfx* #'equal) 
        (ret (string-concat (prolog-to-string (car trm)) " " (prolog-to-name pred) " " 
         (prolog-list-to-string #'prolog-to-string (cdr trm) "" "" ))))
    (ret (string-concat (prolog-to-name pred) (prolog-list-to-string #'prolog-to-string trm "(" ")")))))))

#|
(print (prolog-to-cycl '("member" ?item (?item . ?rest))))
(print (prolog-make-pred '("member" ?item (?item . ?rest))))
(print (prolog-to-cycl '(|:-| ("member" ?item (?x . ?rest)) ("member" ?item ?rest))))
(print (prolog-make-pred '(|:-| ("member" ?item (?x . ?rest)) ("member" ?item ?rest))))
(load "prolog_to_cycl.lisp")
(print (prolog-to-cycl 1))
|#

(defun prolog-to-cycl (trm &optional (mt *prologmt*))
 (cond 
  ((assertion-p trm) (ret trm))
  ((null trm) (ret *prologNil*))
  ((EL-variable-p trm) (ret trm))
  ((HL-variable-p trm) (ret trm))
  ((constant-p trm) (ret trm))
  ((fort-p trm) (ret trm))
  ((stringp trm) (ret trm))
  ((numberp trm) (ret trm))
  ((prolog-predicatep trm) (ret trm))
  ((nart-p trm) (ret trm))
  ((keywordp trm) (ret trm))
  ((symbolp trm) (ret (list  *prologFn* (symbol-name trm))))
  ((atom trm) (ret (list  *prologFn* (write-to-string trm))))
  ;;((atom trm) (ret (list  *prologFn* (write-to-string trm))))
  (t (clet ((pred (car trm))(trm (cdr trm)))
    (if (equal pred *prologFn*) (setq pred (car trm)) (setq trm (cdr trm)))
    (if (equal pred *ist*) (ret (prolog-to-cycl (elt trm 2) (elt trm 1))))
    (if (member pred '("." '|.| *prologCons*)) 
      (ret (prolog-make-cons (prolog-to-cycl (car trm) mt)(prolog-to-cycl (cdr trm) mt))))
    ;; (if (not (equal pred *prologFn*) ) (ret (prolog-make-cons (prolog-to-cycl pred)(prolog-to-cycl trm))))
    (ret (cons *prologFn* (cons (prolog-make-pred pred) (prolog-make-cdr trm))))))))

(defun prolog-make-cdr (trm &optional (mt *prologmt*))
 (if 
   (atom trm) 
   (if trm (prolog-to-cycl trm mt) trm)
  (cons (prolog-to-cycl (car trm) mt) (prolog-make-cdr (cdr trm) mt))))

(defun prolog-make-cons (car cdr) (ret (cons *prologFn* (cons "." (list  car cdr)))))

(defun prolog-car (trm)
 (if (prolog-consp trm) (third trm) (car trm)))

(defun prolog-cdr (trm)
 (if (prolog-consp trm) (fourth trm) (cdr trm)))

(defun prolog-consp (trm)
 (and (consp trm) (if (equal (car trm) *prologfn*) (equal (second trm) *prologCons*) t)))

(defun prolog-predicatep (fort)
 (and (fort-p fort) 
   (or (predicate? fort)(cyc-query (list  (foc "isa") fort *prologPredicate*) *prologMt*))))


(defun prolog-make-op (trm)
 (cond
  ((stringp trm) (ret (list  (foc "OperatorFn") trm)))
  ((EL-variable-p trm) (ret trm))
  ((HL-variable-p trm) (ret trm))
  ((fort-p trm) (ret trm))
  ((nart-p trm) (ret trm))
  ((constant-p trm) (ret trm))
  ((symbolp trm) (ret (list  (foc "OperatorFn") (symbol-name trm))))
  (t (ret trm))))

(defun prolog-make-pred (trm &optional arity)
 ;; (print (list  'prolog-make-pred (list  'quote trm)))
 (cond
  ((EL-variable-p trm) (ret trm))
  ((prolog-predicatep trm) (ret trm))
  ((constant-p trm) (ret trm))
  ((stringp trm) (ret trm))
  ((symbolp trm) (ret (prolog-make-pred (symbol-name trm))))
  ((atom trm) (ret trm))
  ((dotted-list-p trm) (ret (prolog-make-pred (car trm) '??)))
  (t
  (let ((pred (elt trm 0)))
   (if (equal *prologFn* pred) (ret (prolog-make-pred (cdr trm))))
   (if (equal '|:-| pred) (ret (prolog-make-pred (elt trm 1))))
   (if (equal *ist* pred) (ret (prolog-make-pred (elt trm 2))))
   ;;(if (equal *entailedBy* pred) (ret (prolog-make-pred (elt trm 1))))
   (if (null arity) (setq arity (- (length trm) 1)))
   (ret (prolog-make-pred pred arity))))))


(defun first-answer (ansrs)
 (ret (if (consp ansrs) (car ansrs))))


(define unassert-match (sent mt) (cdo-list (matche (ask-template `(,sent) `(#$ist-Asserted  ,mt ,sent) #$EverythingPSC)) (fi-unassert `,matche mt)))

(defun prolog-pred-source (pred &optional (mt *prologmt*))
 (first-answer (ask-template '?cycl (list  *prologSource* (prolog-make-op pred) (cons *theList* '?cycl)) mt)))

(defun prolog-update-code (pred new &optional old (mt *prologMt*))
 (setq pred (prolog-make-op pred))
 (if (null old) (setq old (prolog-pred-source pred mt)))
 (if old (fi-unassert (list  *prologSource* pred (cons *theList* old)) mt))
 (prolog-assert-now (list  *prologSource* pred (cons *theList* new)) mt))

(defun prolog-assertz (sent &optional (mt *prologmt*) pred)
 (unless pred (setq pred (prolog-make-pred sent)))
 (clet 
   ((code (prolog-pred-source pred))
    (nxt (prolog-to-cycl sent mt))
    (new (append code (list nxt))))
  (print (list code '=> new))(force-output)
  (print (list  *prologSource* pred (cons *theList* new)))
  ;;(prolog-assert-now (list  *prologSource* pred (cons *theList* new)) mt)))
  (prolog-update-code pred new code mt)))


;;(fi-assert (list  *prologSource* pred (cons *theList* new)) mt)))
;; (if (null nxt) (throw :UNEVALUATABLE (list  :ERROR "unassertable" nxt mt)))
;;


;;(prolog-to-cycl '("member" ?item (?item . ??rest)) *prologMt*)


;; TODO - dump mt to FOPL
(defun prolog-listing (mt &optional scope) 
 (let ((res nil))
  (if (mt? mt) (setq mt (ask-template '?pred (list  *prologSource* '?pred '?first) mt)))
  (if (atom mt) (setq mt (list  mt)))
  (mapcar (cl-lambda (x) (setq res (append res (prolog-pred-source x)))) mt)
  (ret res)))



#|

CODE 
CODESTR 
HTTPVARSIN 
MT 
MTSTRING 
PROLOGCMDSTR 


|#

(defun prolog-readmacro-prolog (stream char)
 (let* ((*standard-input* (if (streamp stream) stream *standard-input*))
    (red (read_term (rch))))
  (format t "; Read: ~S~%" red )(force-output)
  (ret (values red))))


(set-macro-character #\% (get-macro-character #\;))
(set-macro-character #\$ #'prolog-readmacro-prolog)

(force-print "loaded")


(defun fix-httpvars (httpvars)
 (if (null httpvars) ()
  (let ((item (car httpvars))(httpvars (cdr httpvars)))
   (if (and (consp item) (stringp (second item)) (stringp (car httpvars)))
     (fix-httpvars (cons (list  (car item) (string-concat (second item) "|" (car httpvars))) (cdr httpvars)))
    (cons item (fix-httpvars httpvars))))))

(defun httpvar (name httpvars) 
 (let ((retval ()))
  (and httpvars name 
     (mapcar 
       (cl-lambda (trm) 
         (cond 
         ((and (atom trm) (equal name trm)) (setq retval (cons t retval)))
         ((and (consp trm) (equal name (car trm))) (setq retval (cons (if (consp (cdr trm)) (cadr trm)(cdr trm)) retval)))
         (t nil)))
      httpvars ))
  retval))

(csetq *CB-USER-TOOLBAR-LINKS* (cons :CURRENT-CB-PROLOG *CB-USER-TOOLBAR-LINKS*))

(force-print "loaded2")

(declare-cb-tool
 :current-cb-prolog
 "Prolog Interpreter" "Prolog" "Current Prolog Interpreter")

(define-cb-link-method :current-cb-prolog (&optional linktext)
 (punless linktext
      (csetq linktext "Prolog"))
 (frame-link
  (html-princ "cb-prolog")
  (html-princ linktext))
 (ret nil))

(pushnew '("cb-start|cb-prolog" . "Prolog") *MAIN-MENU-LIST* :test #'EQUAL )
(define current-cb-prolog (&optional linktext) (cb-link :current-cb-smartworld linktext) (ret nil))


;;(defstub :cb-prolog prolog-call)
(defstub :cb-prolog prolog-asserta)
(defstub :cb-prolog prolog-retract)
(defstub :cb-prolog prolog-retractall)
(defstub :cb-prolog prolog_repl )


(defun prolog-call (sent &optional (mt *prologMt*))
    (let ((callable (make-callable sent)))
     (format-comment "~S => ~S"  sent callable)
     ;; (push-call callable)
      callable))
      


(defun make-callable (sent &optional (mt *prologMt*)) (prolog-to-cycl sent mt))
    
        
        

(defvar *comment-string-start* "%% ")
(defun format-comment (string &rest args)
    (princ *comment-string-start*)
    (apply #'format (cons 't (cons string args)))
    (fresh-line))

(define setup-prolog-ke ()
    (prolog-assert-now (list  (foc "isa") *prologMt* (foc "DataMicrotheory")) (foc "BaseKB"))
    (prolog-assert-now (list  (foc "isa") *prologMt* (foc "ApplicationContext")) (foc "BaseKB"))
    (prolog-assert-now (list  (foc "comment") *prologMt* "The #$DataMicrotheory that holds a the prolog program used by <a href=\"cg?cb-prolog&mt=PrologDataMt&prolog=Listing&code=true.\" target=\"cyc-main\">Listing of #$PrologDataMt</a>") *prologMt*)
    (prolog-assert-now (list  (foc "isa") *prologPredicate* (foc "Collection")) *prologMt*)
    (prolog-assert-now (list  (foc "genls") *prologPredicate* (foc "Relation")) *prologMt*)
    ;;(prolog-assert-now (list  (foc "genls") *prologPredicate* (foc "Predicate")) *prologMt* )
    (PROLOG-ASSERT-NOW
     (list  (FOC "implies") (list  (FOC "isa") '?PRED *PROLOGPREDICATE*)
        (list  (FOC "and") 
           (list  (FOC "completeExtentEnumerable") '?PRED)
           ;; (list  (FOC "argsQuotedIsa") '?PRED (FOC "CycLIndexedTerm")) 
           (list  (FOC "argsIsa") '?PRED (FOC "Thing"))
           ;; (list  (FOC "argFormat") '?PRED 2 (FOC "openEntryFormatInArgs"))
           (list  (FOC "argFormat") '?PRED 1 (FOC "openEntryFormatInArgs"))
           ;;(list  (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveSomeTermsAtEL"))
           (list  (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "DontReOrderCommutativeTerms"))
           ;;(list  (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "AllowKeywordVariables"))
           (list  (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveSomeTermsAtELAndAllowKeywordVariables"))
           (list  (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "AllowGenericArgVariables"))
           (list  (FOC "canonicalizerDirectiveForAllArgs") '?PRED (FOC "LeaveVariablesAtEL"))
           ))
     *PROLOGMT*)
    
        
    
    (prolog-assert-now (list  (foc "isa") *entailedBy* (foc "TransitiveBinaryPredicate")) *prologMt*)
    (prolog-assert-now (list  (foc "isa") *entailedBy* (foc "AsymmetricBinaryPredicate")) *prologMt*)
    (prolog-assert-now (list  (foc "isa") *entailedBy* *prologPredicate*) *prologMt*)
    ;;(prolog-assert-now (list  (foc "argsQuotedIsa") *entailedBy* (foc "CycLSentence")) *prologMt*)
    (prolog-assert-now (list  (foc "comment") *entailedBy* "The prolog neck (:-) predicate - ?arg1 :- ?arg2, ?arg3, ... .") *prologMt*)
    
    ;;(prolog-assert-now (list  (foc "argsQuotedIsa") *prologClause* (foc "CycLExpression")) *prologMt*)
    ;;(prolog-assert-now (list  (foc "isa") *prologFn* (foc "IndividualDenotingFunction")) *prologMt*)
    ;;(prolog-assert-now (list  (foc "isa") *prologFn* (foc "TotalFunction")) *prologMt*)
    (prolog-assert-now (list  (foc "isa") *prologFn* (foc "VariableArityPredicate")) *prologMt*)
    ;;(prolog-assert-now (list  (foc "isa") *prologFn* (foc "UnreifiableFunction")) *prologMt*)
    (prolog-assert-now (list  (foc "comment") *prologFn* "A prolog predicate clause - ?arg1(?arg2, ?arg3, ...).") *prologMt*)
    ;;(prolog-assert-now (list  (foc "resultIsa") *prologFn* (foc "Thing")) *prologMt*)
    
    #|
    (prolog-assert-now (list  (foc "arity") (list  *functorFn* '?STRING '?ARITY) '?ARITY) *prologMt* )
    (prolog-assert-now 
     (list  (foc "implies")
        (list  (foc "isa") (list  *functorFn* '?STRING '?ARITY) (foc "Relation"))
        (list  (foc "arity") (list  *functorFn* '?STRING '?ARITY) '?ARITY)) *prologMt* )
    (prolog-assert-now 
     (list  (foc "implies")
        (list  (foc "and")
           (list  (foc "isa") (list  *functorFn* '?STRING '?ARITY) (foc "Relation"))
           (list  (foc "resultIsa") *functorFn* '?TYPE))
        (list  (foc "isa") (list  *functorFn* '?STRING '?ARITY) '?TYPE)) *prologMt* )
    
    (prolog-assert-now (list  (foc "isa") *prologNil* *prologPredicate*) *prologMt* )
    (prolog-assert-now (list  (foc "isa") *prologCons* *prologPredicate*) *prologMt* )
    (prolog-assert-now (list  (foc "isa") *prologCons* (foc "ProgramFunction")) *prologMt* )
    |#

    (prolog-assert-now (list  (foc "isa") *prologFn* *prologPredicate*) *prologMt*)
    (prolog-assert-now (list (foc "afterAdding") *prologSource* '(#$SubLQuoteFn RECACHE-PROLOG-ADDING)) *prologMt*)
    (prolog-assert-now (list (foc "afterRemoving") *prologSource* '(#$SubLQuoteFn RECACHE-PROLOG-REMOVING)) *prologMt*)

    (prolog-assert-now (list (foc "isa") *prologFn* #$IntangibleObjectPredicate) *prologMt*)
    (prolog-assert-now (list (foc "isa") *prologFn* #$RemovalModuleSupportedPredicate-Specific) (foc "CycAPIMt"))

    (prolog-assert-now (list (foc "isa") *prologSource* #$AsymmetricBinaryPredicate) *prologMt*)
    (prolog-assert-now (list (foc "isa") *prologSource* #$IntangibleObjectPredicate) *prologMt*)
    (prolog-assert-now (list (foc "isa") *prologSource* #$RemovalModuleSupportedPredicate-Specific) (foc "CycAPIMt"))
    (prolog-assert-now (list #$comment *prologSource* "(*prologSource* ?Predicate ?Result)") *prologMt*)
    (prolog-assert-now (list #$arity *prologSource* 2) *prologMt*)
    (prolog-assert-now (list #$arg1Isa *prologSource* #$Thing) *prologMt*)
    (prolog-assert-now (list #$arg2Isa *prologSource* #$Thing) *prologMt*)
    (prolog-assert-now (list  (foc "isa") *prologSource* (foc "BinaryPredicate")) *prologMt*)
    (prolog-assert-now (list  (foc "isa") *prologSource* (foc "StrictlyFunctionalSlot")) *prologMt*)
    (prolog-assert-now (list  (foc "isa") *prologSource* *prologPredicate*) *prologMt*)
    (prolog-assert-now (list  (foc "arg1Isa") *prologSource* (foc "Individual")) *prologMt*)
    (prolog-assert-now (list  (foc "arg2Isa") *prologSource* (foc "List")) *prologMt*)
    (prolog-assert-now (list  (foc "argFormat") *prologSource* 2 (foc "openEntryFormatInArgs")) *prologMt*)

    (prolog-assert-now (list (foc "isa") *entailedBy* #$AsymmetricBinaryPredicate) *prologMt*)
    (prolog-assert-now (list (foc "isa") *entailedBy* #$IntangibleObjectPredicate) *prologMt*)
    (prolog-assert-now (list (foc "isa") *entailedBy* #$RemovalModuleSupportedPredicate-Specific) (foc "CycAPIMt"))
    (prolog-assert-now (list #$comment *entailedBy* "(*entailedBy* ?Instance ?Result)") *prologMt*)
    (prolog-assert-now (list #$arity *entailedBy* 2) *prologMt*)
    (prolog-assert-now (list #$arg1Isa *entailedBy* #$Thing) *prologMt*)
    (prolog-assert-now (list #$arg2Isa *entailedBy* #$Thing) *prologMt*)
;;(prolog-assert-now (list  (foc "isa") (list  *functorFn* "member" 2) (foc "Thing")) *prologMt* )
)
(force-print "setting up prolog KE") 
(setup-prolog-ke )
(force-print "DONE setting up prolog KE")

;; TODO - implement prolog writer
(define setup-prolog-removals () 

    (DEFINE-AFTER-ADDING RECACHE-PROLOG-ADDING (str asrt) 
       (ret (list :RECACHE-PROLOG-ADDING str (recache-cycl-the-pred (second (assertion-el-formula asrt))))))
    (DEFINE-AFTER-REMOVING RECACHE-PROLOG-REMOVING (str asrt) 
       (ret (list :RECACHE-PROLOG-REMOVING str (recache-cycl-the-pred (second (assertion-el-formula asrt))))))

    (inference-removal-module :removal-prologFn-prolog-bound-unbound
     `(:sense :pos 
    	:predicate ,*prologFn* 
    	:required-pattern (,*prologFn* :fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete :input-extract-pattern (:template  (,*prologFn* (:bind the-value) :anything) (:value the-value))
    	:input-verify-pattern :anything
    	:output-generate-pattern (:call removal-prologFn-book-unify-generate :input)
    	:output-construct-pattern  (,*prologFn* (:value the-value) :input)
    	:documentation "(,*prologFn* <fully-bound> <not-fully-bound>)"
    	:example "(,*prologFn* -1 ?WHAT)"))
    
    (inference-removal-module :removal-prologFn-prolog-bound-bound 
    `( :sense :pos 
    	:predicate ,*prologFn* 
    	:check t 
    	:required-pattern (,*prologFn* :fully-bound :fully-bound)
    	:cost-expression 0
    	;;*cheap-hl-module-check-cost*
    	:completeness :complete
    	:input-extract-pattern (:template (,*prologFn* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-check-pattern (:call removal-prologFn-pos-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    	:documentation "(,*prologFn* <fully-bound> <fully-bound>)"
    	:example "(,*prologFn* 1 -1)" ))
       
    (register-solely-specific-removal-module-predicate *prologFn*)
        
    (inference-removal-module :removal-prologSource-prolog-bound-unbound
     `(:sense :pos 
    	:predicate ,*prologSource* 
    	:required-pattern (,*prologSource* :fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete :input-extract-pattern (:template  (,*prologSource* (:bind the-value) :anything) (:value the-value))
    	:input-verify-pattern :anything
    	:output-generate-pattern (:call removal-prologSource-book-unify-generate :input)
    	:output-construct-pattern  (,*prologSource* (:value the-value) :input)
    	:documentation "(,*prologSource* <fully-bound> <not-fully-bound>)"
    	:example "(,*prologSource* (#$OperatorFn \"member\") ?WHAT)"))
    
    (inference-removal-module :removal-prologSource-prolog-bound-bound 
    `( :sense :pos 
    	:predicate ,*prologSource* 
    	:check t 
    	:required-pattern (,*prologSource* :fully-bound :fully-bound)
    	:cost-expression 0
    	;;*cheap-hl-module-check-cost*
    	:completeness :complete
    	:input-extract-pattern (:template (,*prologSource* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-check-pattern (:call removal-prologSource-pos-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    	:documentation "(,*prologSource* <fully-bound> <fully-bound>)"
    	:example "(,*prologSource* 1 -1)" ))
       
    (register-solely-specific-removal-module-predicate *prologSource*)
    
    (inference-removal-module :removal-entailedBy-prolog-bound-unbound
     `(:sense :pos 
    	:predicate ,*entailedBy* 
    	:required-pattern (,*entailedBy* :fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete :input-extract-pattern (:template  (,*entailedBy* (:bind the-value) :anything) (:value the-value))
    	:input-verify-pattern :anything
    	:output-generate-pattern (:call removal-entailedBy-book-unify-generate :input)
    	:output-construct-pattern  (,*entailedBy* (:value the-value) :input)
    	:documentation "(,*entailedBy* <fully-bound> <not-fully-bound>)"
    	:example "(,*entailedBy* (#$OperatorFn \"member\") ?WHAT)"))
    
    (inference-removal-module :removal-entailedBy-prolog-unbound-unbound
     `(:sense :pos 
    	:predicate ,*entailedBy* 
    	:required-pattern (,*entailedBy* :not-fully-bound :not-fully-bound) 
    	:cost-expression 0 :completeness :complete 
    	:input-extract-pattern (:template (,*entailedBy* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-generate-pattern (:call removal-entailedBy-book-unify-unbound :input)
    	:output-construct-pattern  (,*entailedBy* (:call print :input) (:call print :input))
    	:documentation "(,*entailedBy* <not-fully-bound> <not-fully-bound>)"
    	:example "(,*entailedBy* (#$OperatorFn \"member\") ?WHAT)"))
    
    (inference-removal-module :removal-entailedBy-prolog-bound-bound 
    `( :sense :pos 
    	:predicate ,*entailedBy* 
    	:check t 
    	:required-pattern (,*entailedBy* :fully-bound :fully-bound)
    	:cost-expression 0 :completeness :complete
    	:input-extract-pattern (:template (,*entailedBy* (:bind value-1) (:bind value-2)) ((:value value-1) (:value value-2)))
    	:input-verify-pattern (:anything :anything)
    	:output-check-pattern (:call removal-entailedBy-pos-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    	:documentation "(,*entailedBy* <fully-bound> <fully-bound>)"
    	:example "(,*entailedBy* 1 -1)" ))
       
    (register-solely-specific-removal-module-predicate *entailedBy*)
    
    (inference-removal-module
     :removal-prologFn-prolog-bound-bound
     `(
       :sense :neg
       :predicate ,*prologFn*
       :check t
       :required-pattern
       (,*prologFn* :fully-bound :fully-bound)
       :cost-expression 0
       ;;*cheap-hl-module-check-cost*
       :completeness :complete
       :input-extract-pattern
       (:template 
        (,*prologFn* (:bind value-1) (:bind value-2))
        ((:value value-1) (:value value-2)))
       :input-verify-pattern
        (:anything :anything)
       :output-check-pattern
        (:call removal-prologFn-neg-check (:tuple (value-1 value-2) ((:value value-1) (:value value-2))))
    
       :documentation
       "(,*prologFn* <fully-bound> <fully-bound>)"
       :example
       "(#$not (,*prologFn* 1 -1))"))
    
    (inference-removal-module :removal-prologFn-prolog-unbound-bound
     `(
       :sense :pos
       :predicate ,*prologFn*
       :required-pattern (,*prologFn* :not-fully-bound :fully-bound)
       :cost-expression 0
       :completeness :complete
       :input-extract-pattern
       (:template 
        (,*prologFn* :anything (:bind the-value))
        (:value the-value))
       :input-verify-pattern
         :anything
       :output-generate-pattern
       (:call removal-prologFn-book-unify-generate :input)
       :output-construct-pattern 
       (,*prologFn* :input (:value the-value))
    
       :documentation
       "(,*prologFn* <not-fully-bound> <fully-bound>)"
       :example
       "(,*prologFn* ?WHAT -1)"
       ))


(DEFINE-CB-LINK-METHOD :current-cb-prolog (&optional linktext)
  (punless linktext
    (csetq linktext "Prolog interpreter"))
  (frame-link 
   (html-princ "cb-prolog")
   (html-princ linktext))
  (ret nil))

(DECLARE-CB-TOOL :current-cb-prolog "Prolog interpreter" "Prolog interpreter" "Prolog interpreter")

;; todo my appoligies for using format - just quick way to maintain html insead of cyc's better html interface
;; test with 
(define-html-handler cb-prolog (httpvarsIn)
 ;;(load "cb-prolog.lisp")
 (let* ((*standard-output* *html-stream*)(read-error NIL)(httpvars (fix-httpvars httpvarsIn))
    (prologcmdstr (car (httpvar "prolog" httpvars)))
    (mtstring (car (httpvar "mt" httpvars)))
    (codestr (remove-comments (car (httpvar "code" httpvars))))
    (code :UNREAD)
    (mt NIL))
  (unwind-protect (trace-progn (if (and (stringp mtstring) (CONSTANT-NAME-SPEC-P mtstring)) (foc mtstring))) t)
  (if (null mt) (setq mt *prologMt*))
  (setq mtstring (constant-name mt))
      (format t "<HTML>
    <HEAD>
    <TITLE>Prolog In Cyc</TITLE>
    <META NAME='GENERATOR' Content='Microsoft Visual Studio .NET 8.0'>
    </HEAD>
    <BODY>
    <FORM method='get' action='cg?cb-prolog' ID='prolog_form'>
    <INPUT id='cb-prolog' type='hidden' name='cb-prolog'>
    <BR>
    Interpret in&nbsp;Mt &nbsp; <INPUT id='mt' type='text' name='mt' value='~a'> 
    <INPUT id='listing' type='submit' value='Listing' name='prolog'> &nbsp; <a href='cg?cb-cf&c~a'>Examine ~a</a><BR>
    <BR>
    <TEXTAREA wrap=off id='code' name='code' rows='16' cols='100%'>" 
   mtstring (constant-internal-id mt) mtstring )
  (force-output)
  (unwind-protect
    (cond
     ((weak-string-equal prologcmdstr "Listing") 
         (setq code (prolog-listing mt))
         (format-comment "cycl = ~S" code)
         (princ (prolog-sentences-to-string code)))         
     ;;(unwind-protect (ccatch :UNEVALUATABLE read-error (setq code (prolog-read codestr))) t)
     ((weak-string-equal prologcmdstr "call (?-)") 
         (let 
           ((code (string-to-term codestr)))
          (format-comment "cycl = ~S" code)
          (format-comment "?- ~a." (prolog-to-string code))
          (prolog-call code mt)))
     ((weak-string-equal prologcmdstr "assert(z)") 
         (let 
           ((code (string-to-term codestr)))
          (format-comment "cycl = ~S" code)
          (format-comment "?- assert(~a)." (prolog-to-string code))
          (prolog-assertz code mt)))
     ((weak-string-equal prologcmdstr "asserta") 
         (let 
           ((code (string-to-term codestr)))
          (format-comment "cycl = ~S" code)
          (format-comment "?- asserta(~a)." (prolog-to-string code))
          (prolog-asserta code mt)))
     ((weak-string-equal prologcmdstr "retract") 
         (let 
           ((code (string-to-term codestr)))
          (format-comment "cycl = ~S" code)
          (format-comment "?- retract(~a)." (prolog-to-string code))
          (prolog-retract code mt)))
     ((weak-string-equal prologcmdstr "retractall") 
         (let 
           ((code (string-to-term codestr)))
          (format-comment "cycl = ~S" code)
          (format-comment "?- retractall(~a)." (prolog-to-string code))
          (prolog-retractall code mt)))
     ((weak-string-equal prologcmdstr "listing") 
         (let 
           ((code (string-to-term codestr)))
          (format-comment "cycl = ~S" code)
          (format-comment "?- listing(~a)." (prolog-to-string code))
          (prolog-listing code mt)))
     (t (princ codestr)))
   
   (format t 
    "</TEXTAREA><BR>
    <BR>
    <INPUT id='call' type='submit' value='call (?-)' name='prolog'> &nbsp; 
    <INPUT id='assert' type='submit' value='assert(z)' name='prolog'> &nbsp;
]    <INPUT id='asserta' type='submit' value='asserta' name='prolog'> &nbsp;
    <INPUT id='retract' type='submit' value='retract' name='prolog'>
    <INPUT id='retractall' type='submit' value='retractall' name='prolog'>
    <INPUT id='eval-subLisp' type='submit' value='eval_SubLisp' name='prolog'>
    <INPUT id='eval-subProlog' type='submit' value='eval_SubProlog' name='prolog'>
    <INPUT id='Listing' type='submit' value='Listing' name='prolog'>
    <BR>
    </FORM>
    <BR>
    <FORM method='post' action='cg?cb-prolog-upload' ID='upload_form'>
    <INPUT id='cb-prolog-upload' type='hidden' name='cb-prolog'>
    Upload Prolog Source Code<BR>
    <INPUT id='filename' type='file' name='filename'> &nbsp;&nbsp; <INPUT id='upload' type='submit' value='upload' name='upload'>
    <P>
    </P>
    </FORM>
    DebugInfo:
    <pre>HTTPVARS = ~s</pre>
    </BODY>
    </HTML>"
    httpvars)) 
  (force-output)))

) 
(setup-prolog-removals)



;;(DEFINE RECACHE-PROLOG-ADDING (&rest info) (print (cons 'RECACHE-PROLOG-ADDING info)))
;;(DEFINE RECACHE-PROLOG-REMOVING (&rest info) (print (cons 'RECACHE-PROLOG-REMOVING info)))

(defun recache-cycl-the-pred (pred) (print (list  :recache-cycl-the-pred pred)) (prolog-set-prolog-def pred nil (prolog-pred-source pred)))
(defun recache-def-pred (pred) (print (list  :recache-def-pred pred))(ret (prolog-pred-source pred)))


(defun pred-symbol-key (pred) 
 (cond 
  ((symbolp pred) (ret pred))
  ((stringp pred) (ret (intern pred)))
  ((consp pred) (ret (pred-symbol-key (second pred))))
  (t (throw :UNEVALUATABLE (list  'pred-symbol-key pred)))))

(defun prolog-set-prolog-def (pred eval def) 
 (unless (symbolp pred) (setq pred (pred-symbol-key pred)))
 (unless (equal (get pred 'EVALUABLE) eval) (put pred 'EVALUABLE eval)) 
 (unless (equal (get pred 'DEF) def) (put pred 'DEF def))
 pred)
;; (translisp-format "keeping definition of ~S as ~S" pred def )

;;(prolog-to-cycl '(|member| ?item (?item . ??rest)) *prologMt*)
(unless (prolog-pred-source '|member|)
 (trace-progn 
  ;;(prolog-to-cycl '(|:-| ("member" ?item (??skip . ?rest)) ("member" ?item ?rest)) *prologMt*)
  (prolog-assertz '(|member| ?item (|.| ?item ??rest)) *prologMt*)
  (prolog-assertz '(|:-| (|member| ?item (|.| ??skip ?rest)) (|member| ?item ?rest)) *prologMt*)
  ))

(force-print ";;;;;;;;;; SUPPRTING Setting up removals")

(define weakql (v1 v2) (ret (cor (eql v1 v2) (cand (numberp v1)(numberp v2)(= v1 v2))(cand (stringp v1) (cor (cand (stringp v2) (string-equal v1 v2)) (cand v2 (eql (find-constant v1) v2))))(cand	(stringp v2) (cand v1 (eql (find-constant v2) v1))))))
(define meakql (v1 v2) (ret (cor (weakql v1 v2)(cand (consp v2) (weakql v1 (car v2)))(cand (consp v1) (weakql v2 (car v1))))))
(define removal-prologFn-pos-check (values) (ret (meakql (removal-prologFn-book-unify-generate (first values)) (second values))))
(define removal-prologFn-neg-check (values) (ret (cnot (meakql (removal-prologFn-book-unify-generate (first values)) (second values)))))
(define removal-prologFn-book-unify-generate (value) (ret (cyc-prolog-eval-stub value)))
(define removal-prologSource-book-unify-generate (value) (ret (cyc-prolog-eval-stub (list  "prologSource" value))))
(define removal-prologSource-pos-check (values) (ret (meakql (removal-prologSource-book-unify-generate (first values)) (second values))))
(define removal-entailedBy-book-unify-generate (value) (ret (cyc-prolog-eval-stub (list  "entailedBy" value))))
(define removal-entailedBy-pos-check (values) (ret (meakql (removal-entailedBy-book-unify-generate (first values)) (second values))))
(define removal-entailedBy-book-unify-unbound (values) (ret (cyc-prolog-eval-stub (list  "entailedBy" values))))

(define cyc-prolog-eval-stub (outval) 
  (ret (clet (*retval* (*stream* (OPEN-TCP-STREAM "10.1.1.104" 3699)))
      (prin1 outval *stream*)
      (terpri *stream*)(force-output *stream*)
      (csetq *retval* (read *stream*))
      (close *stream*) *retval*)))

(define trans-prolog () 
    (clet ((ts-file (TRANSLATE-FILE "PROLOG" "cb-prolog.lisp"))
        (fout (OPEN-TEXT "cb-prolog.trans" :output)))
        (SHOW-TRANS-SUBL-FILE ts-file fout)(close FOUT)
        (C-BACKEND-OUTPUT-FILE-AND-HEADER-FILE ts-file "cb-prolog.c")
        (ret ts-file)))

(defun main ()
    (force-format t "~%?- ")
    (do ((term (read_term) (read_term))) ((eql '|lisp| term))
       (force-format t "~%% ; prolog: call(~A). ~%" (prolog-to-string term))
       (eval `(?- ,term))
       (force-format t "~%?- ")))

;;(main)
#|

(?- (#$prolog:fn "member" ?item (#$prolog:fn "." 1 (#$prolog:fn "." 2 (#$prolog:fn "[]")))))
(?- ((foc "isa") ?item #$Cyclist))



|#     
;; read_term was defined in cb-prolog.lisp

(force-print "done loading cb-prolog.lisp")
#|

(load "cynd/cb-prolog.lisp")

(?- (foo ?X))
(?- (append ?X ?Y (1 2 3)))
(?- (is ?X (+ 1 1)))
|#
