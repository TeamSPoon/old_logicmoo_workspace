
:-ensure_loaded(dbase_i_sexp_reader).


;;; 31 December 03

;THIS IS A TRANSLATION IN KIF (ACCORDING TO THE KIF-DRAFT
;PROPOSED TO THE AMERICAN NATIONAL STANDARD NCITS.T2/98-004
;http://logic.stanford.edu/kif/dpans.html) OF DOLCE V2.1
;For comments on this version, please contact:
;borgo@loa-cnr.it

;REVIEW INFO
;CHANGES - COMMENTS

;(D13) changed WORD into WORLD - Typo
;(NA3)-(NA9) have been dropped - These occur already
;somewhere else
;(NA10)-(NA12) are left as comments - These are guaranteed
;by def. (ND5)
;(NA13) has been dropped -	It follows from (NA14) and (D2)


; Basic functions and relations
; new non-rigid universals introduced in specialized
; theories or in new versions of DOLCE need to be added in
; this definition as new disjunction clauses of
; form (= ?f =8A)
; (ND1): universals
(defrelation UNIVERSAL (?f) :=
    (or (X ?f)))

; new rigid universals introduced in new versions of DOLCE
; (or by the user) need to be added in this definition
; (ND2) rigid universals
(defrelation X (?f) :=
    (or (= ?f ALL) (= ?f AB) (= ?f R) (= ?f TR) (= ?f T) (= ?f PR) (= ?f S) (= ?f AR)
        (= ?f Q) (= ?f TQ) (= ?f TL) (= ?f PQ) (= ?f SL) 
        (= ?f AQ) (= ?f ED) (= ?f M) (= ?f PED) (= ?f F) (= ?f POB) (= ?f APO) (= ?f NAPO)
        (= ?f NPED) (= ?f NPOB) (= ?f MOB) (= ?f SOB) (= ?f ASO) (= ?f SAG) (= ?f SC)
        (= ?f NASO) (= ?f AS) (= ?f PD) (= ?f EV) (= ?f ACH) (= ?f ACC) (= ?f STV)
        (= ?f ST) (= ?f PRO))))


; there are no particulars in this version of DOLCE, any
; particular has to be added in this definition, the def.
; will have form : (or (= ?x =8A) (= ?x =8A))
; (ND3) particulars
(defrelation PARTICULAR(?x) :=
   )

; there are no named worlds in this version of DOLCE, any
; world has to be added in this definition, the def. Will
; have form : (or (= ?w =8A) (= ?w =8A))
; (ND4) worlds
(defrelation WORLD(?w) :=  )

; (ND5) accessibility relation on worlds
(defrelation WLDR(?w ?v) :=
   (and (WORLD ?w) (WORLD ?v)))

; (ND6) Parthood
(defrelation P (?w ?x ?y) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y)))

; (ND7) Temporal Parthood
(defrelation P (?w ?x ?y ?t) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))

; (ND8) Constitution
(defrelation K (?w ?x ?y ?t) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))

; (ND9) Participation
(defrelation PC (?w ?x ?y ?t) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))

; (ND10) Quality
(defrelation qt (?w ?x ?y) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y)))

; (ND11) Quale
(defrelation ql (?w ?x ?y) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y)))

; (ND12) Quale (temporal)
(defrelation ql (?w ?x ?y ?t) :=>
   (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))


;*****************************************************

; (NA1) NEW AXIOM: total domain
(forall (?x)
       (or (PARTICULAR ?x) (UNIVERSAL ?x) (WORLD ?x)))

; (NA2) partition of the domain
(forall (?x)
       (and (<=> (PARTICULAR ?x)
                 (and (not (UNIVERSAL ?x)) (not (WORLD ?x))))
            (<=> (UNIVERSAL ?x)
                 (and (not (PARTICULAR ?x)) (not (WORLD ?x))))
            (<=> (WORLD ?x)
                 (and (not (PARTICULAR ?x)) (not (UNIVERSAL ?x))))))

; Formal Characterization
;PRINCIPLES USED IN THE TRANSLATION IN KIF:
;Modal operators of possibility and necessity are translated in the standard
;  way, see for instance p516 of Handbook of Logic in AI and Logic Prog. Vol4;
;The indeces of relations are included prefixing
a dot (we preserve the capital or
;  lower case distinction)
;These are the only predicates (with their arity)
that do not have possible worlds
;  as arguments:
;  X_1,PARTICULAR_1,UNIVERSAL_1, =_2


;No need for Barcan formulas, the domain of particulars turns out to be unique
;  in the translation

;WLDR is an equivalence relation (from corrispondence theory, this implies
;  that WLDR is a relation for S5). The axioms (NA10)-(NA12) are not necessary
;  because of our definition of WLDR.
; (NA10)
;(forall (?w0) (=> (WORLD ?w0) (WLDR ?w0 ?w0)))
; (NA11)
;(forall (?w0 ?w1)
;    (=> (and (WLDR ?w0 ?w1) (WORLD ?w0) (WORLD ?w1))
;        (WLDR ?w1 ?w0)))
; (NA12)
;(forall (?w0 ?w1 ?w2)
;    (=> (and (WLDR ?w0 ?w1)
;             (WLDR ?w1 ?w2)
;             (WORLD ?w0)
;             (WORLD ?w1)
;             (WORLD ?w2))
;        (WLDR ?w0 ?w2)))


; ***THE UNIVERSALS ARE NECESSARILY NON-EMPY***-- axiom
; (NA14) -- axiom
(forall (?w ?f) (=> (and (UNIVERSAL ?f) (WORLD ?w))
                      (NEP ?w ?f)))

; (NA15) -- axiom
(forall (?w ?f) (=> (and (UNIVERSAL ?f) (WORLD ?w))
                      (or (not (X ?f)) (RG ?w ?f))))

; (NA16) Instances of PT -- axiom
   (forall (?w0) (=> (WORLD ?w0)
				     	(and (PT ?w0 ALL ED PD Q AB)
                         (PT ?w0 ED PED NPED AS)
                         (PT ?w0 PED M F POB)
                         (PT ?w0 POB APO NAPO)
                         (PT ?w0 NPOB MOB SOB)
                         (PT ?w0 SOB ASO NASO)
                         (PT ?w0 ASO SAG SC)
                         (PT ?w0 PD EV STV)
                         (PT ?w0 EV ACH ACC)
                         (PT ?w0 STV ST PRO)
                         (PT ?w0 Q TQ PQ AQ)
                         (PT ?w0 R TR PR AR))))


; (NA17) Instances of SB -- axiom
(forall (?w0)
      (=> (WORLD ?w0)
          (and (SB ?w0 ALL ED) (SB ?w0 ALL PD) (SB ?w0 ALL Q) (SB ?w0 ALL AB)
               (SB ?w0 ED PED) (SB ?w0 ED NPED) (SB ?w0 ED AS)
               (SB ?w0 PED M) (SB ?w0 PED F) (SB ?w0 PED POB)
               (SB ?w0 POB APO) (SB ?w0 POB NAPO)
               (SB ?w0 NPED NPOB)
               (SB ?w0 NPOB MOB) (SB ?w0 NPOB SOB)
               (SB ?w0 SOB ASO) (SB ?w0 SOB NASO)
               (SB ?w0 ASO SAG) (SB ?w0 ASO SC)
               (SB ?w0 PD EV) (SB ?w0 PD STV)
               (SB ?w0 EV ACH) (SB ?w0 EV ACC)
               (SB ?w0 STV ST) (SB ?w0 STV PRO)
               (SB ?w0 Q TQ) (SB ?w0 Q PQ) (SB ?w0 Q AQ)
               (SB ?w0 TQ TL)
               (SB ?w0 PQ SL)
               (SB ?w0 AB FACT) (SB ?w0 AB SET) (SB ?w0 AB R)
               (SB ?w0 R TR) (SB ?w0 R PR) (SB ?w0 R AR)
               (SB ?w0 TR T)
               (SB ?w0 PR S))))

; (NA18) Existence of sum
(forall (?w0 ?x ?y)
          (=> (and (PARTICULAR ?x) (PARTICULAR ?y) (WORLD ?w0))
              (exists (?z)
                      (and (PARTICULAR ?z) (+ ?w0 ?x ?y ?z)))))

; (NA19) Existence of sigma
(forall (?w0 ?f)
          (=> (and (UNIVERSAL ?f) (WORLD ?w0))
              (exists (?z)
                      (and (PARTICULAR ?z) (sigma ?w0 ?f ?z)))))

; (NA20) Existence of sum.t
(forall (?w0 ?x ?y)
          (=> (and (PARTICULAR ?x) (PARTICULAR ?y) (WORLD ?w0))
              (exists (?z)
                      (and (PARTICULAR ?z) (+.t ?w0 ?x ?y ?z)))))

; (NA21) Existence of sigma.t
(forall (?w0 ?f)
          (=> (and (UNIVERSAL ?f) (WORLD ?w0))
              (exists (?z)
                      (and (PARTICULAR ?z) (sigma.t ?w0 ?f ?z)))))

; this could be added in the def. of UNIVERSAL
;(forall (@f)
;        (<=> (UNIVERSAL @f)
;             (exists (?g @h) (and (UNIVERSAL ?g)
;                                  (or (UNIVERSAL @h) (= @h (listof)))
;                                  (= @f (listof ?g @h))))))

; this could be added in the def. of PARTICULAR
;(forall (@x)
;        (<=> (PARTICULAR @x)
;             (exists (?y @z) (and (PARTICULAR ?y)
;                                  (or (PARTICULAR @z) (= @z (listof)))
;                                  (= @x (listof ?y @z))))))


;********************************************************
;(D1)  RG: Rigid Universal
(defrelation RG (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (forall (?w ?x)
                (=> (and (WLDR ?w0 ?w) (WORLD ?w) (PARTICULAR ?x))
                    (=> (?f ?w ?x)
                        (forall (?u)
                                (=> (and (WLDR ?w ?u) (WORLD ?u))
                                    (?f ?u ?x))))))))

;(D2) NEP: Non-Empty Universal
(defrelation NEP (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (forall (?w)
                (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                    (exists (?y)
                            (and (PARTICULAR ?y) (?f ?w ?y)))))))

;(D3) DJ: Disjoint Universals
(defrelation DJ (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (forall (?w ?x)
                (=> (and (WLDR ?w0 ?w)
                         (WORLD ?w)
                         (PARTICULAR ?x))
                    (not (and (?f ?w ?x) (?g ?w ?x)))))))

;(D4) SB: Subsumption
(defrelation SB (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (forall (?w ?x)
                (=> (and (WLDR ?w0 ?w)
                         (WORLD ?w)
                         (PARTICULAR ?x))
                    (or (not (?g ?w ?x)) (?f ?w ?x))))))

;(D5) EQ: Equal Universals
(defrelation EQ (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0)=20
(SB ?w0 ?f ?g) (SB ?w0 ?g ?f)))

;(D6) PSB: Properly Subsuming
(defrelation PSB (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g)
        (not (SB ?w0 ?f ?g))))

;(D7) L: Leaf Universal
(defrelation L (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (forall (?w ?g)
                (=> (and (WLDR ?w0 ?w)
                         (WORLD ?w)
                         (UNIVERSAL ?g))
                    (or (not (?SB ?w0 ?f ?g)) (EQ ?w0 ?f ?g))))))

;(D8) SBL: Leaf Subsumed by
(defrelation SBL (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g) (L ?w0 ?g)))

;(D9) PSBL: Leaf Properly Subsumed by
(defrelation PSBL (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (PSB ?w0 ?f ?g) (L ?w0 ?g)))

;(D10) L__: Leaf in the set X
   (defrelation L.X (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (X ?f)
        (forall (?w ?g)
                (=> (and (WLDR ?w0 ?w) (WORLD ?w) (UNIVERSAL ?g))
                    (=> (and (?SB ?w ?f ?g) (X ?g))
                        (EQ ?w ?f ?g))))))

;(D11) SBL__
(defrelation SBL.X (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g) (L.X ?w0 ?g)))

;(D12) PSBL__
(defrelation PSBL.X (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (PSB ?w0 ?f ?g) (L.X ?w0 ?g)))

; Definition (D13) is left for expressivity. In practice it becomes superfluous
; since the user needs to give a list of the n-tuple satisfying relation PT in
; axiom (NA17)
;(D13) PT: Partition
(defrelation PT (?w0 ?f @g) :=
   (and (UNIVERSAL ?f)
      (UNIVERSAL @g)
      (WORLD ?w0)
      (not (item ?f @g))
      (forall (?h ?k)
            (and (=> (and (UNIVERSAL ?h)
                          (UNIVERSAL ?k)
                          (item ?h @g)
                          (item ?k @g)
                          (/= ?h ?k))
                     (DJ ?w0 ?h ?k))
                 (forall (?w ?x)
                      (=> (and (WLDR ?w0 ?w)
                               (WORLD ?w)
                               (PARTICULAR ?x))
                          (<=> (?f ?w ?x)
                               (exists (?h)
                                    (and (UNIVERSAL ?h)
                                         (item ?h @g)
                                         (?h ?w ?x))))))))))

; Mereological Definitions
;(D14) PP: Proper Part
(defrelation PP (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (P ?w0 ?x ?y)
        (not (P ?w0 ?y ?x))))

;(D15) O: Overlap
(defrelation O (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?z) (and (PARTICULAR ?z)
                          (P ?w0 ?z ?x)
                          (P ?w0 ?z ?y)))))

;(D16) At: Atom
(defrelation At (?w0 ?x) :=
   (and (PARTICULAR ?x)
        (WORLD ?w0)
        (not (exists (?y) (and (PARTICULAR ?y)
                               (PP ?w0 ?y ?x))))))

;(D17) AtP: Atomic Part
(defrelation AtP (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (P ?w0 ?x ?y)
        (At ?w0 ?x)))

;(D18) __ Binary Sum
(defrelation + (?w0 ?x ?y ?z) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?z)
        (WORLD ?w0)
        (forall (?u)
                (=> (PARTICULAR ?u)
                    (<=> (O ?w0 ?u ?z)
                         (or (O ?w0 ?u ?x) (O ?w0 ?u ?y)))))
        (forall (?z1)
                (=> (and (PARTICULAR ?z1)
                         (forall (?u)
                              (=> (PARTICULAR ?u)
                                  (<=> (O ?w0 ?u ?z1)
                                       (or (O ?w0 ?u ?x) (O ?w0 ?u ?y))))))
                    (= ?z1 ?z)))))

;(D19) (general) Sum
; Note: the rendition in KIF is weaker than the corresponding definition  in
;modal FOL; here ?f has to be one of the universal introduced explicitly.
;[A possible way out: use string-variables (@f) to code Boolean
;combinations of universals.]
(defrelation sigma (?w0 ?f ?z) :=
   (and (PARTICULAR ?z)
        (UNIVERSAL ?f)
        (WORLD ?w0)
        (forall (?y)
                (=> (PARTICULAR ?y)
                    (<=> (O ?w0 ?y ?z)
                         (exists (?v)
                                 (and (PARTICULAR ?v)
                                      (?f ?w0 ?v)
                                      (O ?w0 ?y ?v))))))
        (forall (?z1)
            (=> (PARTICULAR ?z1)
                (exists (?y)
                   (and (PARTICULAR ?y)
                        (=> (<=> (O ?w0 ?y ?z1)
                                 (exists (?v)
                                     (and (PARTICULAR ?v)
                                          (?f ?w0 ?v)
                                          (O ?w0 ?y ?v)))))
                        (= ?z1 ?z)))))))

;(D20) PP: Temporary Proper Part
(defrelation PP (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (P ?w0 ?x ?y ?t)
        (not (P ?w0 ?y ?x ?t))))

;(D21) O: Temporary Overlap
(defrelation O (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?z) (and (PARTICULAR ?z)
                          (P ?w0 ?z ?x ?t)
                          (P ?w0 ?z ?y ?t)))))

;(D22) At: Temporary Atom
(defrelation At (?w0 ?x ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (not (exists (?y)
                   (and (PARTICULAR ?y) (PP ?w0 ?y ?x ?t))))))

;(D23) AtP: Temporary Atomic Part
(defrelation AtP (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (P ?w0 ?x ?y ?t)
        (At ?w0 ?x ?t)))

;(D24) Coincidence
(defrelation =.t (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (P ?w0 ?x ?y ?t)
        (P ?w0 ?y ?x ?t)))

;(D25) CP: Constant Part
(defrelation CP (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?t)
                (and (PARTICULAR ?t) (PRE ?w0 ?y ?t)))
        (forall (?t)
                (=> (and (PARTICULAR ?t) (PRE ?w0 ?y ?t))
                    (P ?w0 ?x ?y ?t)))))

;(D26)
(defrelation +.t (?w0 ?x ?y ?z) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?z)
        (WORLD ?w0)
        (forall (?u ?t)
             (=> (and (PARTICULAR ?u) (PARTICULAR ?t))
                 (<=> (O ?w0 ?u ?z ?t)
                      (or (O ?w0 ?u ?x ?t) (O ?w0 ?u ?y ?t)))))
        (forall (?z1 ?t)
             (=> (and (PARTICULAR ?z1)
                      (PARTICULAR ?t)
                      (forall (?u)
                          (=> (PARTICULAR ?u)
                              (<=> (O ?w0 ?u ?z1 ?t)
                                   (or (O ?w0 ?u ?x ?t) (O ?w0 ?u ?y ?t))))))
                  (= ?z1 ?z)))))

;(D27)
; NOTE: this rendition includes only the listed universal, for instance,
; no Boolean combination  of universals is included [see also comment on (D19)]
(defrelation sigma.t (?w0 ?f ?z) :=
   (and (PARTICULAR ?z)
        (UNIVERSAL ?f)
        (WORLD ?w0)
        (forall (?y ?t)
                (=> (and (PARTICULAR ?y) (PARTICULAR ?t))
                    (<=> (O ?w0 ?y ?z ?t)
                         (exists (?v)
                                 (and (PARTICULAR ?v)
                                      (?f ?w0 ?v)
                                      (O ?w0 ?y ?v ?t))))))
        (forall (?z1 ?t)
            (=> (and (PARTICULAR ?z1) (PARTICULAR ?t))
                (exists (?y)
                   (and (PARTICULAR ?y)
                        (=> (<=> (O ?w0 ?y ?z1 ?t)
                                 (exists (?v)
                                    (and (PARTICULAR ?v)
                                         (?f ?w0 ?v)
                                         (O ?w0 ?y ?v ?t))))
                            (= ?z1 ?z))))))))

; Quality
;(D28) dqt: Direct Quality
(defrelation dqt (?w0 ?x ?y) :=
   (and (WORLD ?w0)
        (PARTICULAR ?x)
        (PARTICULAR ?y)
        (qt ?w0 ?x ?y)
        (not (exists (?z)
                     (and (PARTICULAR ?z)
                          (qt ?w0 ?x ?z)
                          (qt ?w0 ?z ?y))))))

;(D29) qt: Quality of type
(defrelation qtf (?w0 ?f ?x ?y) :=
   (and (UNIVERSAL ?f)
        (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (qt ?w0 ?x ?y)
        (?f ?w0 ?x)
        (SBL.X ?w0 Q ?f)))

; Temporal and Spatial Quale
;(D30) ql_T,PD
(defrelation ql.T.PD (?w0 ?t ?x) :=
   (and (PARTICULAR ?t)
        (PARTICULAR ?x)
        (WORLD ?w0)
        (PD ?w0 ?x)
        (exists (?z) (and (PARTICULAR ?z)
                          (qtf ?w0 TL ?z ?x)
                          (ql ?w0 ?t ?z)))))

;(D31) ql_T,ED
   (defrelation ql.T.ED (?w0 ?t ?x) :=
   (and (PARTICULAR ?t)
        (PARTICULAR ?x)
        (WORLD ?w0)
        (ED ?w0 ?x)
        (forall (?u)
             (=> (PARTICULAR ?u)
                 (<=> (O ?w0 ?u ?t)
                      (exists (?v ?y)
                           (and (PARTICULAR ?v)
                                (PARTICULAR ?y)
                                (PC ?w0 ?x ?y ?v)
                                (O ?w0 ?u ?v))))))
        (forall (?t1)
             (=> (PARTICULAR ?t1)
                 (exists (?u)
                     (and (PARTICULAR ?u)
                          (=> (<=> (O ?w0 ?u ?t1)
                                   (exists (?v ?y)
                                       (and (PARTICULAR ?v)
                                            (PARTICULAR ?y)
                                            (PC ?w0 ?x ?y ?v)
                                            (O ?w0 ?u ?v))))
                              (= ?t1 ?t))))))))

;(D32) ql_T,TQ
(defrelation ql.T.TQ (?w0 ?t ?x) :=
   (and (PARTICULAR ?t)
        (PARTICULAR ?x)
        (WORLD ?w0)
        (TQ ?w0 ?x)
        (exists (?z) (and (PARTICULAR ?z)
                          (qt ?w0 ?x ?z)
                          (ql.T.PD ?w0 ?t ?z)))))

;(D33) ql_T,PQ_or_AQ
(defrelation ql.T.PQAQ (?w0 ?t ?x) :=
   (and (PARTICULAR ?t)
        (PARTICULAR ?x)
        (WORLD ?w0)
        (or (PQ ?w0 ?x) (AQ ?w0 ?x))
        (exists (?z) (and (PARTICULAR ?z)
                          (qt ?w0 ?x ?z)
                          (ql.T.ED ?w0 ?t ?z)))))

;(D34) ql_T,Q
(defrelation ql.T.Q (?w0 ?t ?x) :=
   (and (PARTICULAR ?t)
        (PARTICULAR ?x)
        (WORLD ?w0)
        (or (ql.T.TQ ?w0 ?t ?x)
            (ql.T.PQAQ ?w0 ?t ?x))))

;(D35) ql_T: Temporal Quale
(defrelation ql.T (?w0 ?t ?x) :=
   (and (PARTICULAR ?t)
        (PARTICULAR ?x)
        (WORLD ?w0)
        (or (ql.T.ED ?w0 ?t ?x)
            (ql.T.PD ?w0 ?t ?x)
            (ql.T.Q ?w0 ?t ?x))))

;(D36) ql_S,PED
(defrelation ql.S.PED (?w0 ?s ?x ?t) :=
   (and (PARTICULAR ?s)
        (PARTICULAR ?x)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PED ?w0 ?x)
        (exists (?z) (and (PARTICULAR ?z)
                          (qtf ?w0 SL ?z ?x)
                          (ql ?w0 ?s ?z ?t)))))

;(D37) ql_S,PQ
(defrelation ql.S.PQ (?s ?x ?t) :=
   (and (PARTICULAR ?s)
        (PARTICULAR ?x)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PQ ?w0 ?x)
        (exists (?z) (and (PARTICULAR ?z)
                          (qt ?w0 ?x ?z)
                          (ql.S.PED ?w0 ?s ?z ?t)))))

;(D38) ql_S,PD
(defrelation ql.S.PD (?w0 ?s ?x ?t) :=
   (and (PARTICULAR ?s)
        (PARTICULAR ?x)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PD ?w0 ?x)
        (exists (?z) (and (PARTICULAR ?z)
                          (mppc ?w0 ?z ?x)
                          (ql.S.PED ?w0 ?s ?z ?t)))))

;(D39) ql_S: Spatial Quale
(defrelation ql.S (?w0 ?s ?x ?t) :=
   (and (PARTICULAR ?s)
        (PARTICULAR ?x)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (or (ql.S.PED ?w0 ?s ?x ?t)
            (ql.S.PQ ?w0 ?s ?x ?t)
            (ql.S.PD ?w0 ?s ?x ?t))))

;Being present
;(D40) PRE: Being Present at
(defrelation PRE (?w0 ?x ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?u) (and (PARTICULAR ?u)
                          (ql.T ?w0 ?u ?x)
                          (P ?w0 ?t ?u)))))

;(D41) PRE: Being Present in at
(defrelation PRE (?w0 ?x ?s ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?s)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PRE ?w0 ?x ?t)
        (exists (?u) (and (PARTICULAR ?u)
                          (ql.S ?w0 ?u ?x ?t)
                          (P ?w0 ?s ?u)))))

; Inclusion and Coincidence
;(D42) Temporal Inclusion
(defrelation incl.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?t ?u) (and (PARTICULAR ?t)
                             (PARTICULAR ?u)
                             (ql.T ?w0 ?t ?x)
                             (ql.T ?w0 ?u ?y)
                             (P ?w0 ?t ?u)))))

;(D43) Proper Temporal Inclusion
(defrelation sincl.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?t ?u) (and (PARTICULAR ?t)
                             (PARTICULAR ?u)
                             (ql.T ?w0 ?t ?x)
                             (ql.T ?w0 ?u ?y)
                             (PP ?w0 ?t ?u)))))

;(D44) Temporary Spatial Inclusion
(defrelation incl.S.t (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?s ?r) (and (PARTICULAR ?s)
                             (PARTICULAR ?r)
                             (ql.S ?w0 ?s ?x ?t)
                             (ql.S ?w0 ?r ?y ?t)
                             (P ?w0 ?s ?r)))))

;(D45) Temp. Proper Sp. Inclusion
(defrelation sincl.S.t (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?s ?r) (and (PARTICULAR ?s)
                             (PARTICULAR ?r)
                             (ql.S ?w0 ?s ?x ?t)
                             (ql.S ?w0 ?r ?y ?t)
                             (PP ?w0 ?s ?r)))))

;(D46) Spatio-temporal Inclusion
(defrelation incl.S.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
      (PARTICULAR ?y)
      (WORLD ?w0)
      (exists (?t) (and (PARTICULAR ?t) (PRE ?w0 ?x ?t)))
      (forall (?t) (=> (and (PARTICULAR ?t) (PRE ?w0 ?x ?t))
                       (incl.S.t ?w0 ?x ?y ?t)))))

;(D47) Spatio-temp. Incl. during
(defrelation incl.S.T.t (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PRE ?w0 ?x ?t)
        (forall (?u) (=> (and (PARTICULAR ?u) (AtP ?w0 ?u ?t))
                         (incl.S.t ?w0 ?x ?y ?u)))))

;(D48) Temporal Coincidence
(defrelation ~.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (incl.T ?w0 ?x ?y)
        (incl.T ?w0 ?y ?x)))

;(D49) Temporary Spatial Coincidence
(defrelation ~.S.t (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (incl.S.t ?w0 ?x ?y ?t)
        (incl.S.t ?w0 ?y ?x ?t)))

;(D50) Spatio-temporal Coincidence
(defrelation ~.S.T (?w0 ?x ?y) :=
   (and (WORLD ?w0)
        (PARTICULAR ?x)
        (PARTICULAR ?y)
        (incl.S.T ?w0 ?x ?y)
        (incl.S.T ?w0 ?y ?x)))

;(D51) Spatio-temp. Coincidence during
(defrelation ~.S.T.t (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PRE ?w0 ?x ?t)
        (forall (?u) (=> (and (PARTICULAR ?u) (AtP ?w0 ?u ?t))
                         (~.S.t ?w0 ?x ?y ?u)))))

;(D52) O_T: Temporal Overlap
(defrelation O.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?t ?u) (and (PARTICULAR ?t)
                             (PARTICULAR ?u)
                             (ql.T ?w0 ?t ?x)
                             (ql.T ?w0 ?u ?y)
                             (O ?w0 ?t ?u)))))

;(D53) O_S,t: Temporary Spatial Overlap
(defrelation O.S.t (?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?s ?r) (and (PARTICULAR ?s)
                             (PARTICULAR ?r)
                             (ql.S ?w0 ?s ?x ?t)
                             (ql.S ?w0 ?r ?y ?t)
                             (O ?w0 ?s ?r)))))

; Perdurant
;(D54) P_T: Temporal Part
(defrelation P.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (PD ?w0 ?x)
        (P ?w0 ?x ?y)
        (forall (?z) (=> (and (PARTICULAR ?z)
                              (P ?w0 ?z ?y)
                              (incl.T ?w0 ?z ?x))
                         (P ?w0 ?z ?x)))))

;(D55) P_S: Spatial Part
(defrelation P.S (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (PD ?w0 ?x)
        (P ?w0 ?x ?y)
        (~.T ?w0 ?x ?y)))

;(D56) NEP_S: Strongly Non-Empty
(defrelation NEP.S (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (SB ?w0 PD ?f)
        (forall (?w) (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                         (exists (?x ?y)
                              (and (PARTICULAR ?x)
                                   (PARTICULAR ?y)
                                   (?f ?w ?x)
                                   (?f ?w ?y)
                                   (not (P ?w ?x ?y))
                                   (not (P ?w ?y ?x))))))))

;(D57) CM: Cumulative
(defrelation CM (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (SB ?w0 PD ?f)
        (forall (?w ?x ?y ?z)
                (=> (and (WLDR ?w0 ?w)
                         (WORLD ?w)
                         (PARTICULAR ?x)
                         (PARTICULAR ?y)
                         (PARTICULAR ?z)
                         (+ ?w ?x ?y ?z)
                         (?f ?w ?x)
                         (?f ?w ?y))
                    (?f ?w ?z)))))

;(D58) CM=97: Anti-Cumulative
(defrelation CM~ (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (SB ?w0 PD ?f)
        (forall (?w ?x ?y ?z)
                (=> (and (WLDR ?w0 ?w)
                         (WORLD ?w)
                         (PARTICULAR ?x)
                         (PARTICULAR ?y)
                         (PARTICULAR ?z)
                         (+ ?w ?x ?y ?z)
                         (?f ?w ?x)
                         (?f ?w ?y)
                         (not (P ?w ?x ?y))
                         (not (P ?w ?y ?x)))
                    (not (?f ?w ?z))))))

;(D59) HOM: Homeomerous
(defrelation HOM (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (SB ?w0 PD ?f)
        (forall (?w ?x ?y) (=> (and (WLDR ?w0 ?w)
                                    (WORLD ?w)
                                    (PARTICULAR ?x)
                                    (PARTICULAR ?y)
                                    (?f ?w ?x)
                                    (P.T ?w ?y ?x))
                                (?f ?w ?y)))))

;(D60) HOM=97: Anti-Homeom.
(defrelation HOM~ (?w0 ?f) :=
   (and (UNIVERSAL ?f)
     (WORLD ?w0)
     (SB ?w0 PD ?f)
     (forall (?w ?x)
             (=> (and (WLDR ?w0 ?w)
                      (WORLD ?w)
                      (PARTICULAR ?x)
                      (?f ?w ?x))
                 (exists (?y)
                      (and (PARTICULAR ?y)
                           (P.T ?w ?y ?x)
                           (not (?f ?w ?y))))))))

;(D61) AT: Atomic
(defrelation AT (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (SB ?w0 PD ?f)
        (forall (?w ?x) (=> (and (WLDR ?w0 ?w)
                                 (WORLD ?w)
                                 (PARTICULAR ?x)
                                 (?f ?w ?x))
                            (At ?w ?x)))))

;(D62) AT=97: Anti-Atomic
(defrelation AT~ (?w0 ?f) :=
   (and (UNIVERSAL ?f)
        (WORLD ?w0)
        (SB ?w0 PD ?f)
        (forall (?w ?x) (=> (and (WLDR ?w0 ?w)
                                 (WORLD ?w)
                                 (PARTICULAR ?x)
                                 (?f ?w ?x))
                            (not (At ?w ?x))))))

;Participation
;(D63) PC_C: Constant Participation
(defrelation PC.C (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?t) (and (PARTICULAR ?t) (PRE ?w0 ?y ?t)))
        (forall (?t) (=> (and (PARTICULAR ?t)
                              (PRE ?w0 ?y ?t))
                         (PC ?w0 ?x ?y ?t)))))

;(D64) PC_T: Temporary Total Particip.
(defrelation PC.T (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (PD ?w0 ?y)
        (forall (?z)
              (=> (and (PARTICULAR ?z)
                       (P ?w0 ?z ?y)
                       (PRE ?w0 ?z ?t))
                  (PC ?w0 ?x ?z ?t)))))

;(D65) PC_T: Total Participation
(defrelation PC.T (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (exists (?t) (and (PARTICULAR ?t)
                          (ql.T ?w0 ?t ?y)
                          (PC.T ?w0 ?x ?y ?t)))))

;(D66) mpc: Maximal Participant
(defrelation mpc (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (forall (?z ?t)
                (=> (and (PARTICULAR ?z) (PARTICULAR ?t))
                    (<=> (O ?w0 ?z ?x ?t)
                         (exists (?v)
                                 (and (PARTICULAR ?v)
                                      (PC.T ?w0 ?v ?y ?t)
                                      (O ?w0 ?z ?v ?t))))))
        (forall (?z ?x1 ?t)
                (=> (and (PARTICULAR ?z)
                         (PARTICULAR ?x1)
                         (PARTICULAR ?t)
                         (<=> (O ?w0 ?z ?x1 ?t)
                              (exists (?v)
                                  (and (PARTICULAR ?v)
                                       (PC.T ?w0 ?v ?y ?t)
                                       (O ?w0 ?z ?v ?t)))))
                    (= ?x1 ?x)))))

;(D67) mppc: Maximal Physical Participant
(defrelation mppc (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (forall (?z ?t)
                (=> (and (PARTICULAR ?z) (PARTICULAR ?t))
                    (<=> (O ?w0 ?z ?x ?t)
                         (exists (?v)
                                 (and (PARTICULAR ?v)
                                      (PC.T ?w0 ?v ?y ?t)
                                      (PED ?w0 ?z)
                                      (O ?w0 ?z ?v ?t))))))
        (forall (?z ?x1 ?t)
                (=> (and (PARTICULAR ?z)
                         (PARTICULAR ?x1)
                         (PARTICULAR ?t)
                         (<=> (O ?w0 ?z ?x1 ?t)
                              (exists (?v)
                                  (and (PARTICULAR ?v)
                                       (PC.T ?w0 ?v ?y ?t)
                                       (PED ?w0 ?z)
                                       (O ?w0 ?z ?v ?t)))))
                    (= ?x1 ?x)))))

;(D68) lf: Life
(defrelation lf (?w0 ?x ?y) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (WORLD ?w0)
        (forall (?z)
                (=> (PARTICULAR ?z)
                    (<=> (O ?w0 ?z ?x)
                         (exists (?v)
                                 (and (PARTICULAR ?v)
                                      (PC.T ?w0 ?y ?v)
                                      (O ?w0 ?z ?v))))))
        (forall (?z ?u)
                (=> (and (PARTICULAR ?z) (PARTICULAR ?u)
                         (<=> (O ?w0 ?z ?u)
                              (exists (?v)
                                  (and (PARTICULAR ?v)
                                       (PC.T ?w0 ?y ?v)
                                       (O ?w0 ?z ?v)))))
                    (= ?u ?x)))))

; Dependence
;(D69) SD: Specific Constant Dep.
(defrelation SD (?w0 ?x ?y) :=
   (or (and (PARTICULAR ?x)
            (PARTICULAR ?y)
            (WORLD ?w0)
            (forall (?w)
                (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                    (and (exists (?t)
                              (and (PARTICULAR ?t) (PRE ?w ?x ?t)))
                         (forall (?t)
                              (=> (and (PARTICULAR ?t) (PRE ?w ?x ?t))
                                  (PRE ?w ?y ?t)))))))
       (and (UNIVERSAL ?x)
            (UNIVERSAL ?y)
            (WORLD ?w0)
            (DJ ?w0 ?x ?y)
            (forall (?w ?x1)
                (=> (and (WLDR ?w0 ?w)
                         (WORLD ?w)
                         (PARTICULAR ?x1)
                         (?x ?w ?x1))
                    (exists (?y1) (and (PARTICULAR ?y1)
                                       (?y ?w ?y1)
                                       (SD ?w ?x1 ?y1))))))))

;(D70) SD: Specific Const. Dep.
;included in def (D69)

;(D71) GD: Generic Const. Dep.
(defrelation GD (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (DJ ?w0 ?f ?g)
        (forall (?w ?x ?t)
             (=> (and (WLDR ?w0 ?w)
                      (WORLD ?w)
                      (PARTICULAR ?x)
                      (PARTICULAR ?t)
                      (?f ?w ?x))
                 (and (exists (?t1)
                          (and (PARTICULAR ?t1) (PRE ?w ?x ?t1)))
                      (=> (and (At ?w ?t) (PRE ?w ?x ?t))
                          (exists (?y)
                                  (and (PARTICULAR ?y)
                                       (?g ?w ?y)
                                       (PRE ?w ?y ?t)))))))))

;(D72) D: Constant Dependence
(defrelation D (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (or (SD ?w0 ?f ?g) (GD ?w0 ?f ?g))))

;(D73) OD: One-sided Constant Dependence
(defrelation OD (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (D ?w0 ?f ?g)
        (not (D ?w0 ?g ?f))))

;(D74) OSD: One-sided Specific Constant Dependence
(defrelation OSD (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (SD ?w0 ?f ?g)
        (not (D ?w0 ?g ?f))))

;(D75) OGD: One-sided Generic Constant Dependence
(defrelation OGD (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GD ?w0 ?f ?g)
        (not (D ?w0 ?g ?f))))

;(D76) MSD: Mutual Specific Constant Dependence
(defrelation MSD (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (SD ?w0 ?f ?g)
        (SD ?w0 ?g ?f)))

;(D77) MGD: Mutual Generic Constant Dependence
(defrelation MGD (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GD ?w0 ?f ?g)
        (GD ?w0 ?g ?f)))

; Spatial Dependence
;(D78) SD_S: Specific Spatial Dependence
(defrelation SD.S (?w0 ?x ?y) :=
   (or (and (WORLD ?w0)
            (PARTICULAR ?x)
            (PARTICULAR ?y)
            (forall (?w)
               (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                   (and (exists (?t ?s)
                                (and (PARTICULAR ?t)
                                     (PARTICULAR ?s)
                                     (PRE ?w ?x ?s ?t)))
                        (forall (?t ?s)
                                (=> (and (PARTICULAR ?t)
                                         (PARTICULAR ?s)
                                         (PRE ?w ?x ?s ?t))
                                    (PRE ?w ?y ?s ?t)))))))
       (and (WORLD ?w0)
            (UNIVERSAL ?x)
            (UNIVERSAL ?y)
            (DJ ?w0 ?x ?y)
            (forall (?w ?x1)
                  (=> (and (WLDR ?w0 ?w)
                           (WORLD ?w)
                           (PARTICULAR ?x1)
                           (?x ?w ?x))
                      (exists (?y1)
                           (and (PARTICULAR ?y1)
                                (?y ?w ?y1)
                                (SD.S ?w ?x1 ?y1))))))))

;(D79) PSD_S: Partial Specific Spatial Dependence
(defrelation PSD.S (?w0 ?x ?y) :=
   (or (and (WORLD ?w0)
        (PARTICULAR ?x)
        (PARTICULAR ?y)
        (forall (?w)
           (=> (and (WLDR ?w0 ?w) (WORLD ?w))
               (and (exists (?t ?s)
                         (and (PARTICULAR ?t)
                              (PARTICULAR ?s)
                              (PRE ?w ?x ?s ?t)))
                    (forall (?t ?s)
                        (=> (and (PARTICULAR ?t)
                                 (PARTICULAR ?s)
                                 (PRE ?w ?x ?s ?t))
                            (exists (?r)
                                (and (PARTICULAR ?r)
                                     (PP ?w ?r ?s)
                                     (PRE ?w ?y ?r ?t)))))))))
       (and (WORLD ?w0)
            (UNIVERSAL ?x)
            (UNIVERSAL ?y)
            (DJ ?w0 ?x ?y)
            (forall (?w ?x1)
                  (=> (and (WLDR ?w0 ?w)
                           (WORLD ?w)
                           (PARTICULAR ?x1)
                           (?x ?w ?x1))
                      (exists (?y1)
                           (and (PARTICULAR ?y1)
                                (?y ?w ?y1)
                                (PSD.S ?w ?x1 ?y1))))))))

;(D80) P-1SD_S: Inverse Partial Specific Spatial Dependence
(defrelation P1SD.S (?w0 ?x ?y) :=
   (or (and (WORLD ?w0)
         (PARTICULAR ?x)
         (PARTICULAR ?y)
         (forall (?w)
            (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                (and (exists (?t ?s)
                         (and (PARTICULAR ?t)
                              (PARTICULAR ?s)
                              (PRE ?w ?x ?s ?t)))
                     (forall (?t ?s)
                        (=> (and (PARTICULAR ?t)
                                 (PARTICULAR ?s)
                                 (PRE ?w ?x ?s ?t))
                            (exists (?r)
                                (and (PARTICULAR ?r)
                                     (PP ?w ?s ?r)
                                     (PRE ?w ?y ?r ?t)))))))))
       (and (WORLD ?w0)
            (UNIVERSAL ?x)
            (UNIVERSAL ?y)
            (DJ ?w0 ?x ?y)
            (forall (?w ?x1)
                  (=> (and (WLDR ?w0 ?w)
                           (WORLD ?w)
                           (PARTICULAR ?x1)
                           (?x ?w ?x1))
                      (exists (?y1)
                           (and (PARTICULAR ?y1)
                                (?y ?w ?y1)
                                (P1SD.S ?w ?x1 ?y1))))))))

;(D81) SD_S
;included in def (D78)

;(D82) PSD_S
;included in def (D79)

;(D83) P-1SD_S
;included in def (D80)

;(D84) GD_S: Generic Spatial Dependence
(defrelation GD.S (?w0 ?f ?g) :=
   (and (WORLD ?w0)
        (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (DJ ?w0 ?f ?g)
        (forall (?w ?x ?s ?t)
              (=> (and (WLDR ?w0 ?w)
                       (WORLD ?w)
                       (PARTICULAR ?x)
                       (PARTICULAR ?t)
                       (PARTICULAR ?s)
                       (?f ?w ?x))
                  (and (exists (?t1 ?s1)
                           (and (PARTICULAR ?t1)
                                (PARTICULAR ?s1)
                                (PRE ?w ?x ?s1 ?t1)))
                       (=> (and (At ?w ?t) (PRE ?w ?x ?s ?t))
                           (exists (?y)
                                (and (PARTICULAR ?y)
                                     (?g ?w ?y)
                                     (PRE ?w ?y ?s ?t)))))))))

;(D85) PGD_S: Partial Generic Spatial Dependence
(defrelation PGD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
      (UNIVERSAL ?g)
      (WORLD ?w0)
      (DJ ?w0 ?f ?g)
      (forall (?w ?x ?s ?t)
           (=> (and (WLDR ?w0 ?w)
                    (WORLD ?w))
                    (PARTICULAR ?x)
                    (PARTICULAR ?s)
                    (PARTICULAR ?t)
                    (?f ?w ?x))
               (and (exists (?s1 ?t1)
                         (and (PRE ?w ?x ?s1 ?t1)
                              (PARTICULAR ?s1)
                              (PARTICULAR ?t1))
                    (=> (and (At ?w ?t) (PRE ?w ?x ?s ?t))
                        (exists (?y ?u)
                             (and (PARTICULAR ?y)
                                  (PARTICULAR ?u)
                                  (?g ?w ?y)
                                  (PP ?w ?u ?s)
                                  (PRE ?w ?y ?u ?t)))))))))

;(D86) P-1GD_S: Inverse Partial Generic Spatial Dependence
(defrelation P1GD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
      (UNIVERSAL ?g)
      (WORLD ?w0)
      (DJ ?w0 ?f ?g)
      (forall (?w ?x ?s ?t)
           (=> (and (WLDR ?w0 ?w)
                    (WORLD ?w))
                    (PARTICULAR ?x)
                    (PARTICULAR ?s)
                    (PARTICULAR ?t)
                    (?f ?w ?x))
               (and (exists (?t1 ?s1)
                        (and (PARTICULAR ?t1)
                             (PARTICULAR ?s1)
                             (PRE ?w ?x ?s1 ?t1))
                    (=> (and (At ?w ?t) (PRE ?w ?x ?t))
                        (exists (?y ?u)
                             (and (PARTICULAR ?y)
                                  (PARTICULAR ?u)
                                  (?g ?w ?y)
                                  (PP ?w ?s ?u)
                                  (PRE ?w ?y ?u ?t)))))))))

;(D87) DGD_S: Direct Generic Spatial Dependence
(defrelation DGD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GD.S ?w0 ?f ?g)
        (not (exists (?h) (and (UNIVERSAL ?h)
                               (GD.S ?w0 ?f ?h)
                               (GD.S ?w0 ?h ?g))))))

;(D88) Sdt_S: Temporary Specific Spatial Dependence
(defrelation SDt.S (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (SD.S ?w0 ?x ?y)
        (PRE ?w0 ?x ?t)))

;(D89) GDt_S: Temp. Gen. Sp. Dep.
(defrelation GDt.S (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?f ?g) (and (UNIVERSAL ?f)
                             (UNIVERSAL ?g)
                             (?f ?w0 ?x)
                             (?g ?w0 ?y)
                             (GD.S ?w0 ?f ?g)
                             (~.S.t ?w0 ?x ?y ?t)))))

;(D90) DGDt_S: Temp. Direct Sp. Dep.
(defrelation DGDt.S (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (exists (?f ?g) (and (UNIVERSAL ?f)
                             (UNIVERSAL ?g)
                             (?f ?w0 ?x)
                             (?g ?w0 ?y)
                             (DGD.S ?w0 ?f ?g)
                             (~.S.t ?w0 ?x ?y ?t)))))

;(D91) OSD_S: One-sided Specific Spatial Dependence
(defrelation OSD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (SD.S ?w0 ?f ?g)
        (not (D ?w0 ?g ?f))))

;(D92) OGD_S: One-sided Generic Spatial Dependence
(defrelation OGD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GD.S ?w0 ?f ?g)
        (not (D ?w0 ?g ?f))))

;(D93) MSD_S: Mutual Specific Spatial Dependence
(defrelation MSD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (SD.S ?w0 ?f ?g)
        (SD.S ?w0 ?g ?f)))

;(D94) MGD_S: Mutual Generic Spatial Dependence
(defrelation MGD.S (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GD.S ?w0 ?f ?g)
        (GD.S ?w0 ?g ?f)))

; Constitution
;(D95) DK: Direct Constitution
(defrelation DK (?w0 ?x ?y ?t) :=
   (and (PARTICULAR ?x)
        (PARTICULAR ?y)
        (PARTICULAR ?t)
        (WORLD ?w0)
        (K ?w0 ?x ?y ?t)
        (not (exists (?z) (and (PARTICULAR ?z)
                               (K ?w0 ?x ?z ?t)
                               (K ?w0 ?z ?y ?t))))))

;(D96) SK: Constantly Specifically Constituted by
(defrelation SK (?w0 ?x ?y) :=
   (or (and (WORLD ?w0)
            (PARTICULAR ?x)
            (PARTICULAR ?y)
            (forall (?w)
                (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                    (and (exists (?t)
                                (and (PARTICULAR ?t) (PRE ?w ?x ?t))
                         (forall (?t)
                                (=> (and (PARTICULAR ?t)
                                         (PRE ?w ?x ?t))
                                    (K ?w ?y ?x ?t))))))))
       (and (UNIVERSAL ?x)
            (UNIVERSAL ?y)
            (WORLD ?w0)
            (DJ ?w0 ?f ?g)
            (forall (?w ?x1)
               (=> (and (WLDR ?w0 ?w)
                        (WORLD ?w)
                        (PARTICULAR ?x1)
                        (?f ?w ?x1))
                   (exists (?y1)
                        (and (PARTICULAR ?y1)
                             (?y ?w ?y1)
                             (SK ?w ?x1 ?y1))))))))

;(D97) SK: Constantly Specifically Constituted by
;included in def (D96)

;(D98) GK: Constantly Generically Constituted by
(defrelation GK (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (DJ ?w0 ?f ?g)
        (forall (?w ?x ?t)
             (=> (and (WLDR ?w0 ?w)
                      (WORLD ?w)
                      (PARTICULAR ?x)
                      (PARTICULAR ?t)
                      (?f ?w ?x))
                 (and (exists (?t1)
                          (and (PARTICULAR ?t1) (PRE ?w ?x ?t1)))
                      (=> (and (At ?w ?t) (PRE ?w ?x ?t))
                          (exists (?y)
                               (and (PARTICULAR ?y)
                                    (?g ?w ?y)
                                    (K ?w ?y ?x ?t)))))))))

;(D99) K__Constituted by
(defrelation K (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (or (SK ?w0 ?f ?g) (GK ?w0 ?f ?g))))

;(D100) OSK: One-sided Cons. Specif. Const. by
(defrelation OSK (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (SK ?w0 ?f ?g)
        (not (K ?w0 ?g ?f))))

;(D101) OGK: One-sided Cons. Generic. Const. by
(defrelation OGK (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GK ?w0 ?f ?g)
        (not (K ?w0 ?g ?f))))

;(D102) MSK: Mutual Specific Constitution
(defrelation MSK (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (SK ?w0 ?f ?g)
        (SK ?w0 ?g ?f)))

;(D103) MGK: Mutual Generic Constitution
(defrelation MSK (?w0 ?f ?g) :=
   (and (UNIVERSAL ?f)
        (UNIVERSAL ?g)
        (WORLD ?w0)
        (GK ?w0 ?f ?g)
        (GK ?w0 ?g ?f)))

; Characterization of functions and relations
; Parthood
; Argument Restrictions
;(A1)
(forall (?w0 ?x ?y)
      (=> (and (P ?w0 ?x ?y)
               (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y))
          (and (or (AB ?w0 ?x) (PD ?w0 ?x))
               (or (AB ?w0 ?y) (PD ?w0 ?y)))))

;(A2)
(forall (?w0 ?x ?y)
      (=> (and (P ?w0 ?x ?y)
               (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y))
          (<=> (PD ?w0 ?x) (PD ?w0 ?y))))

;(A3)
(forall (?w0 ?x ?y)
      (=> (and (P ?w0 ?x ?y)
               (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y))
          (<=> (AB ?w0 ?x)
               (AB ?w0 ?y))))

;(A4)
(forall (?w0 ?x ?y ?f)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (UNIVERSAL ?f)
               (P ?w0 ?x ?y)
               (SB ?w0 R ?f)
               (X ?f))
          (<=> (?f ?w0 ?x) (?f ?w0 ?y))))

; Ground Axioms
;(A5)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (or (AB ?w0 ?x) (PD ?w0 ?x)))
          (P ?w0 ?x ?x)))

;(A6)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (P ?w0 ?x ?y)
               (P ?w0 ?y ?x))
          (= ?x ?y)))

;(A7)
(forall (?w0 ?x ?y ?z)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (P ?w0 ?x ?y)
               (P ?w0 ?y ?z))
          (P ?w0 ?x ?z)))

;(A8)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (or (AB ?w0 ?x) (PD ?w0 ?x))
               (not (P ?w0 ?x ?y)))
          (exists (?z)
               (and (PARTICULAR ?x)
                    (P ?w0 ?z ?x)
                    (not (O ?w0 ?z ?y))))))

;(A9)
; Note: this version in KIF consider only the universal explicitly listed
;[see comment on (D19)]
(forall (?w0 ?f)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (exists (?x)
                  (and (PARTICULAR ?x) (?f ?w0 ?x)))
               (or (forall (?x)
                       (=> (and (PARTICULAR ?x) (?f ?w0 ?x))
                           (AB ?w0 ?x)))
                   (forall (?x)
                       (=> (and (PARTICULAR ?x) (?f ?w0 ?x))
                           (PD ?w0 ?x)))))
          (exists (?y)
               (and (PARTICULAR ?y) (sigma ?w0 ?f ?y)))))

; Temporary Parthood
; Argument restrictions
;(A10)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (P ?w0 ?x ?y ?t))
          (and (ED ?w0 ?x) (ED ?w0 ?y) (T ?w0 ?t))))

;(A11)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (P ?w0 ?x ?y ?t))
          (<=> (PED ?w0 ?x) (PED ?w0 ?y))))

;(A12)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (P ?w0 ?x ?y ?t))
          (<=> (NPED ?w0 ?x) (NPED ?w0 ?y))))

; Ground Axioms
;(A13)
(forall (?w0 ?x ?y ?z ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (PARTICULAR ?t)
               (P ?w0 ?x ?y ?t)
               (P ?w0 ?y ?z ?t))
          (P ?w0 ?x ?z ?t)))

;(A14)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (ED ?w0 ?x)
               (ED ?w0 ?y)
               (PRE ?w0 ?x ?t)
               (PRE ?w0 ?y ?t)
               (not (P ?w0 ?x ?y ?t)))
          (exists (?z)
               (and (PARTICULAR ?z)
                    (P ?w0 ?z ?x ?t)
                    (not (O ?w0 ?z ?y ?t))))))

;(A15)
;[see comment on (D19)]
(forall (?w0 ?f)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (exists (?x)
                  (and (PARTICULAR ?x) (?f ?w0 ?x)))
               (forall (?x)
                       (=> (and (PARTICULAR ?x) (?f ?w0 ?x))
                           (ED ?w0 ?x))))
          (exists (?y)
               (and (PARTICULAR ?y) (sigma.t ?w0 ?f ?y)))))

; Links With Other Primitives
;(A16)
(forall (?w0 ?x ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?t)
               (ED ?w0 ?x)
               (PRE ?w0 ?x ?t))
          (P ?w0 ?x ?x ?t)))

;(A17)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (P ?w0 ?x ?y ?t))
          (and (PRE ?w0 ?x ?t) (PRE ?w0 ?y ?t))))

;(A18)
(forall (?w0 ?x ?y ?t ?u)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (PARTICULAR ?u)
               (P ?w0 ?x ?y ?t)
               (P ?w0 ?u ?t))
          (P ?w0 ?x ?y ?u)))

;(A19)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (PED ?w0 ?x)
               (P ?w0 ?x ?y ?t))
          (incl.S.t ?w0 ?x ?y ?t)))

; Constitution
; Argument restrictions
;(A20)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t))
          (and (or (ED ?w0 ?x) (PD ?w0 ?x))
               (or (ED ?w0 ?y) (PD ?w0 ?y))
               (T ?w0 ?t))))

;(A21)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t))
          (<=> (PED ?w0 ?x) (PED ?w0 ?y))))

;(A22)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t))
          (<=> (NPED ?w0 ?x) (NPED ?w0 ?y))))

;(A23)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t))
          (<=> (PD ?w0 ?x) (PD ?w0 ?y))))

; Ground Axioms
;(A24)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t))
          (not (K ?w0 ?y ?x ?t))))

;(A25)
(forall (?w0 ?x ?y ?z ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t)
               (K ?w0 ?y ?z ?t))
          (K ?w0 ?x ?z ?t)))

; Links with other Primitives
;(A26)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t))
          (and (PRE ?w0 ?x ?t) (PRE ?w0 ?y ?t))))

;(A27)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t))
          (<=> (K ?w0 ?x ?y ?t)
               (forall (?u)
                   (=> (and (PARTICULAR ?u) (P ?w0 ?u ?t))
                       (K ?w0 ?x ?y ?u))))))

;(A28)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (PED ?w0 ?x)
               (K ?w0 ?x ?y ?t))
          (~.S.t ?w0 ?x ?y ?t)))

;(A29)
(forall (?w0 ?x ?y ?y1 ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?y1)
               (PARTICULAR ?t)
               (K ?w0 ?x ?y ?t)
               (P ?w0 ?y1 ?y ?t))
          (exists (?x1)
               (and (PARTICULAR ?x1)
                    (P ?w0 ?x1 ?x ?t)
                    (K ?w0 ?x1 ?y1 ?t)))))

; Links between Categories
;(A30)
(forall (?w0) (=> (WORLD ?w0) (GK ?w0 NAPO M)))

;(A31)
(forall (?w0) (=> (WORLD ?w0) (GK ?w0 APO NAPO)))

;(A32)
(forall (?w0) (=> (WORLD ?w0) (GK ?w0 SC SAG)))

; Participation
; Argument restrictions
;(A33)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (PC ?w0 ?x ?y ?t))
          (and (ED ?w0 ?x) (PD ?w0 ?y) (T ?w0 ?t))))

; Existential Axioms
;(a34)
(forall (?w0 ?x ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?t)
               (PD ?w0 ?x)
               (PRE ?w0 ?x ?t))
          (exists (?y)
               (and (PARTICULAR ?y) (PC ?w0 ?y ?x ?t)))))

;(a35)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (ED ?w0 ?x))
          (exists (?y ?t)
               (and (PARTICULAR ?y) (PARTICULAR ?t) (PC ?w0 ?x ?y ?t)))))

; Links with other Primitives
;(a36)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (PC ?w0 ?x ?y ?t))
          (and (PRE ?w0 ?x ?t) (PRE ?w0 ?y ?t))))

;(a37)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t))
          (<=> (PC ?w0 ?x ?y ?t)
               (forall (?u)
                   (=> (and (PARTICULAR ?u) (P ?w0 ?u ?t))
                       (PC ?w0 ?x ?y ?u))))))

; Quality
; Argument restrictions:
;(a38)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (qt ?w0 ?x ?y))
          (and (Q ?w0 ?x)
               (or (Q ?w0 ?y) (ED ?w0 ?y) (PD ?w0 ?y)))))

;(a39)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (qt ?w0 ?x ?y))
          (<=> (TQ ?w0 ?x)
               (or (TQ ?w0 ?y) (PD ?w0 ?y)))))

;(a40)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (qt ?w0 ?x ?y))
          (<=> (PQ ?w0 ?x)
               (or (PQ ?w0 ?y) (PED ?w0 ?y)))))

;(a41)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (qt ?w0 ?x ?y))
          (<=> (AQ ?w0 ?x)
               (or (AQ ?w0 ?y) (NPED ?w0 ?y)))))

; Ground Axioms:
;(a42)
(forall (?w0 ?x ?y ?z)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (qt ?w0 ?x ?y)
               (qt ?w0 ?y ?z))
          (qt ?w0 ?x ?z)))

;(a43)
(forall (?w0 ?x ?y ?z)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (qt ?w0 ?x ?y)
               (qt ?w0 ?x ?z))
          (= ?y ?z)))

;(a44)
(forall (?w0 ?f ?x ?y ?z)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (qtf ?w0 ?f ?x ?y)
               (qtf ?w0 ?f ?z ?y))
          (= ?x ?z)))

;(a45)
(forall (?w0 ?f ?g ?x ?y ?z)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?z)
               (qtf ?w0 ?f ?x ?y)
               (qtf ?w0 ?g ?y ?z))
          (DJ ?w0 ?f ?g)))

; Existential Axioms:
;(a46)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (TQ ?w0 ?x))
          (exists (?y)
               (and (PARTICULAR ?y)
                    (qt ?w0 ?x ?y)
                    (PD ?w0 ?y)
                    (forall (?z)
                        (=> (and (PARTICULAR ?z)
                                 (qt ?w0 ?x ?z)
                                 (PD ?w0 ?z))
                            (= ?z ?y)))))))

;(a47)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (PQ ?w0 ?x))
          (exists (?y)
               (and (PARTICULAR ?y)
                    (qt ?w0 ?x ?y)
                    (PED ?w0 ?y)
                    (forall (?z)
                        (=> (and (PARTICULAR ?z)
                                 (qt ?w0 ?x ?z)
                                 (PED ?w0 ?z))
                            (= ?z ?y)))))))

;(a48)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (AQ ?w0 ?x))
          (exists (?y)
               (and (PARTICULAR ?y)
                    (qt ?w0 ?x ?y)
                    (NPED ?w0 ?y)
                    (forall (?z)
                        (=> (and (PARTICULAR ?z)
                                 (qt ?w0 ?x ?z)
                                 (NPED ?w0 ?z))
                            (= ?z ?y)))))))

;(a49)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (PD ?w0 ?x))
          (exists (?y)
               (and (PARTICULAR ?y) (qtf ?w0 TL ?y ?x)))))

;(a50)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (PED ?w0 ?x))
          (exists (?y)
               (and (PARTICULAR ?y) (qtf ?w0 SL ?y ?x)))))

;(a51)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (NPED ?w0 ?x))
          (exists (?f ?y)
               (and (PARTICULAR ?y)
                    (UNIVERSAL ?f)
                    (SBL ?w0 AQ ?f)
                    (qtf ?w0 ?f ?y ?x)))))

; Quale
; Immediate Quale
; Argument restrictions:
;(A52)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (ql ?w0 ?x ?y))
          (and (TR ?w0 ?x) (TQ ?w0 ?y))))

;(A53)
(forall (?w0 ?x ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (ql ?w0 ?x ?y)
               (TL ?w0 ?y))
          (T ?w0 ?x)))

; Basic Axioms:
;(A54)
(forall (?w0 ?x ?x1 ?y)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?x1)
               (PARTICULAR ?y)
               (ql ?w0 ?x ?y)
               (ql ?w0 ?x1 ?y))
          (= ?x ?x1)))

; Existential Axioms:
;(A55)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (TQ ?w0 ?x))
          (exists (?y)
               (and (PARTICULAR ?y) (ql ?w0 ?y ?x)))))

;(A56)
(forall (?w0 ?f ?x ?y ?r ?r1)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?r)
               (PARTICULAR ?r1)
               (L.X ?w0 ?f)
               (?f ?w0 ?x)
               (?f ?w0 ?y)
               (ql ?w0 ?r ?x)
               (ql ?w0 ?r1 ?y))
          (exists (?g)
               (and (UNIVERSAL ?g)
                    (L.X ?w0 ?g)
                    (?g ?w0 ?r)
                    (?g ?w0 ?r1)))))

;(A57)
(forall (?w0 ?f ?x ?y ?r ?r1)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?r)
               (PARTICULAR ?r1)
               (L.X ?w0 ?f)
               (?f ?w0 ?x)
               (not (?f ?w0 ?y))
               (ql ?w0 ?r ?x)
               (ql ?w0 ?r1 ?y))
          (not (exists (?g)
                  (and (UNIVERSAL ?g)
                       (L.X ?w0 ?g)
                       (?g ?w0 ?r)
                       (?g ?w0 ?r1))))))

; Temporary Quale
; Argument restrictions:
;(A58)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (ql ?w0 ?x ?y ?t))
          (and (or (PR ?w0 ?x) (AR ?w0 ?x))
               (or (PQ ?w0 ?y) (AQ ?w0 ?y))
               (T ?w0 ?t))))

;(A59)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (ql ?w0 ?x ?y ?t))
          (<=> (PR ?w0 ?x) (PQ ?w0 ?y))))

;(A60)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (ql ?w0 ?x ?y ?t))
          (<=> (AR ?w0 ?x) (AQ ?w0 ?y))))

;(A61)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (ql ?w0 ?x ?y ?t)
               (SL ?w0 ?y))
          (S ?w0 ?x)))

; Existential Axioms:
;(A62)
(forall (?w0 ?x)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (or (PQ ?w0 ?x) (AQ ?w0 ?x))
               (PRE ?w0 ?x ?t))
          (exists (?y)
               (and (PARTICULAR ?y) (ql ?w0 ?y ?x ?t)))))

;(A63)
(forall (?w0 ?f ?x ?y ?r ?r1 ?t)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?r)
               (PARTICULAR ?r1)
               (PARTICULAR ?t)
               (L.X ?w0 ?f)
               (?f ?w0 ?x)
               (?f ?w0 ?y)
               (ql ?w0 ?r ?x ?t)
               (ql ?w0 ?r1 ?y ?t))
          (exists (?g)
               (and (UNIVERSAL ?g)
                    (L.X ?w0 ?g)
                    (?g ?w0 ?r)
                    (?g ?w0 ?r1)))))

;(A64)
(forall (?w0 ?f ?x ?y ?r ?r1 ?t)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?r)
               (PARTICULAR ?r1)
               (PARTICULAR ?t)
               (L.X ?w0 ?f)
               (?f ?w0 ?x)
               (not (?f ?w0 ?y))
               (ql ?w0 ?r ?x ?t)
               (ql ?w0 ?r1 ?y ?t))
          (not (exists (?g)
                  (and (UNIVERSAL ?g)
                       (L.X ?w0 ?g)
                       (?g ?w0 ?r)
                       (?g ?w0 ?r1))))))

; Link with Parthood and extension:
;(A65)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (ql ?w0 ?x ?y ?t))
          (PRE ?w0 ?y ?t)))

;(A66)
(forall (?w0 ?x ?y ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t))
          (<=> (ql ?w0 ?x ?y ?t)
               (forall (?u)
                       (=> (and (PARTICULAR ?u) (P ?w0 ?u ?t))
                           (ql ?w0 ?x ?y ?u))))))

; Dependence and Spatial Dependence
; Links between categories
;(A67)
(forall (?w0) (=> (WORLD ?w0) (MSD ?w0 TQ PD)))

;(A68)
(forall (?w0) (=> (WORLD ?w0) (MSD.S ?w0 PQ PED)))

;(A69)
(forall (?w0) (=> (WORLD ?w0) (MSD ?w0 AQ NPED)))

;(A70)
(forall (?w0) (=> (WORLD ?w0) (OGD ?w0 F NAPO)))

;(A71)
(forall (?w0) (=> (WORLD ?w0) (OSD ?w0 MOB APO)))

;(A72)
(forall (?w0) (=> (WORLD ?w0) (OGD ?w0 SAG APO)))

;(A73)
(forall (?w0) (=> (WORLD ?w0) (OGD ?w0 NASO SC)))

;(A74)
(forall (?w0) (=> (WORLD ?w0) (OD ?w0 NPED PED)))

; Characterization of Categories
; Perdurant
; Conditions on Perdurant's Leaves
;(A75)
(forall (?w0 ?f)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PSBL ?w0 ACH ?f))
          (and (NEP.S ?w0 ?f) (CM~ ?w0 ?f) (AT ?w0 ?f))))

;(A76)
(forall (?w0 ?f)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PSBL ?w0 ACC ?f))
          (and (NEP.S ?w0 ?f) (CM~ ?w0 ?f) (AT~ ?w0 ?f))))

;(A77)
(forall (?w0 ?f)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PSBL ?w0 ST ?f))
          (and (NEP.S ?w0 ?f) (CM ?w0 ?f) (HOM ?w0 ?f))))

;(A78)
(forall (?w0 ?f)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (PSBL ?w0 PRO ?f))
          (and (NEP.S ?w0 ?f) (CM ?w0 ?f) (HOM~ ?w0 ?f))))

; Existential Axioms
;(A79)
   (forall (?w0)
      (=> (WORLD ?w0)
          (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 ACH ?f)))))

;(A80)
   (forall (?w0)
      (=> (WORLD ?w0)
          (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 ACC ?f)))))

;(A81)
   (forall (?w0)
      (=> (WORLD ?w0)
          (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 ST ?f)))))

;(A82)
   (forall (?w0)
      (=> (WORLD ?w0)
          (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 PRO ?f)))))
; =========================================
; THEOREMS
; General Properties
; (T1)
   (forall (?w0 ?x ?t)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (PARTICULAR ?t))
          (not (K ?w0 ?x ?x ?t))))

; (T2)
   (forall (?w0 ?f ?g)
      (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (SK ?w0 ?f ?g))
          (SD ?w0 ?f ?g)))

; (T3)
   (forall (?w0 ?f ?g)
      (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (GK ?w0 ?f ?g))
          (GD ?w0 ?f ?g)))

; (T4)
   (forall (?w0 ?f ?g ?h)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (UNIVERSAL ?h)
               (SK ?w0 ?f ?g)
               (SK ?w0 ?g ?h)
               (DJ ?w0 ?f ?h))
          (SK ?w0 ?f ?h)))

; (T5)
   (forall (?w0 ?f ?g ?h)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (UNIVERSAL ?h)
               (GK ?w0 ?f ?g)
               (GK ?w0 ?g ?h)
               (DJ ?w0 ?f ?h))
          (GK ?w0 ?f ?h)))

; Ground Properties
; (T6)
   (forall (?w0 ?x ?t)
      (=> (and (WORLD ?w0) (PARTICULAR ?x) (PARTICULAR ?t))
          (not (PC ?w0 ?x ?x ?t))))

; (T7)
   (forall (?w0 ?x ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?y)
               (PARTICULAR ?t)
               (PC ?w0 ?x ?y ?t))
          (not (PC ?w0 ?y ?x ?t))))

; (T8)
   (forall (?w0 ?x)
      (=> (and (WORLD ?w0) (PARTICULAR ?x))
          (not (qt ?w0 ?x ?x))))

; General properties
; (T9)
   (forall (?w0 ?f ?g ?h)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (UNIVERSAL ?h)
               (SD ?w0 ?f ?g)
               (SD ?w0 ?g ?h)
               (DJ ?w0 ?f ?h))
          (SD ?w0 ?f ?h)))

; (T10)
   (forall (?w0 ?f ?g ?h)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (UNIVERSAL ?h)
               (GD ?w0 ?f ?g)
               (GD ?w0 ?g ?h)
               (DJ ?w0 ?f ?h))
          (GD ?w0 ?f ?h)))

; (T11)
   (forall (?w0 ?f ?g ?h)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (UNIVERSAL ?h)
               (SD ?w0 ?f ?g)
               (GD ?w0 ?g ?h)
               (DJ ?w0 ?f ?h))
          (GD ?w0 ?f ?h)))

; (T12)
   (forall (?w0 ?f ?g ?h)
      (=> (and (WORLD ?w0)
               (UNIVERSAL ?f)
               (UNIVERSAL ?g)
               (UNIVERSAL ?h)
               (GD ?w0 ?f ?g)
               (SD ?w0 ?g ?h)
               (DJ ?w0 ?f ?h))
          (GD ?w0 ?f ?h)))

; (T13)
   (forall (?w0 ?f ?g)
      (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (SD.S ?w0 ?f ?g))
          (SD ?w0 ?f ?g)))

; (T14)
   (forall (?w0 ?f ?g)
      (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (GD.S ?w0 ?f ?g))
          (GD ?w0 ?f ?g)))

; Being Present
; (T15)
   (forall (?w0 ?x)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (or (ED ?w0 ?x) (PD ?w0 ?x) (Q ?w0 ?x)))
          (exists (?t)
               (and (PARTICULAR ?t) (PRE ?w0 ?x ?t)))))

; (T16)
   (forall (?w0 ?x ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?t)
               (or (PED ?w0 ?x) (PQ ?w0 ?x))
               (PRE ?w0 ?x ?t))
          (exists (?s)
               (and (PARTICULAR ?s) (PRE ?w0 ?s ?x ?t)))))

; (T17)
   (forall (?w0 ?x ?t ?t1)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?t)
               (PARTICULAR ?t1)
               (PRE ?w0 ?x ?t)
               (P ?w0 ?t1 ?t))
          (PRE ?w0 ?x ?t1)))

; (T18)
   (forall (?w0 ?x ?s ?t)
      (=> (and (WORLD ?w0)
               (PARTICULAR ?x)
               (PARTICULAR ?s)
               (PARTICULAR ?t)
               (PRE ?w0 ?s ?x ?t))
          (PRE ?w0 ?x ?t)))
