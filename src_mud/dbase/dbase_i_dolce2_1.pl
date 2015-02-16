% ==============================================
:-ensure_loaded(dbase_i_sexp_reader).
% 31 December 03 THIS IS A TRANSLATION IN KIF (ACCORDING TO THE KIF-DRAFT PROPOSED TO THE AMERICAN NATIONAL STANDARD NCITS.T2/98-004
%   http://logic.stanford.edu/kif/dpans.html) OF DOLCE V2.1 For comments on this version, please contact: borgo@loa-cnr.it REVIEW INFO CHANGES - COMMENTS
% ==============================================
%  D13==changed WORD into WORLD - Typo
% ==============================================
%  A3 ==-(NA9) have been dropped - These occur already somewhere else
% ==============================================
%  NA10 ==-(NA12) are left as comments - These are guaranteed by def. (ND5)
% ==============================================
%  NA13==has been dropped -	It follows from (NA14) and (D2)
% ==============================================
%  Basic functions and relations new non-rigid universals introduced in specialized theories or in new versions of DOLCE need to be added in this definition as new disjunction clauses of form (=?f=8A)
% ==============================================
:-load_kif(";;; ND1 ==: universals
(defrelation UNIVERSAL (?f) :=
     (or (X ?f)))
").
% ==============================================
% new rigid universals introduced in new versions of DOLCE or by the user) need to be added in this definition
% ==============================================
:-load_kif(";;; ND2==rigid universals
(defrelation X (?f) :=
     (or (=?f ALL) (=?f AB) (=?f R) (=?f TR) (=?f T) (=?f PR) (=?f S) (=?f AR)
         (=?f Q) (=?f TQ) (=?f TL) (=?f PQ) (=?f SL)
         (=?f AQ) (=?f ED) (=?f M) (=?f PED) (=?f F) (=?f POB) (=?f APO) (=?f NAPO)
         (=?f NPED) (=?f NPOB) (=?f MOB) (=?f SOB) (=?f ASO) (=?f SAG) (=?f SC)
         (=?f NASO) (=?f AS) (=?f PD) (=?f EV) (=?f ACH) (=?f ACC) (=?f STV)
         (=?f ST) (=?f PRO))))
").
% ==============================================
%   there are no particulars in this version of DOLCE, any particular has to be added in this definition, the def. will have form : (or (=?x=8A) (=?x=8A).
% ==============================================
:-load_kif(";;; ND3==particulars
(defrelation PARTICULAR(?x) :=
    )
").
% ==============================================
% there are no named worlds in this version of DOLCE, any world has to be added in this definition, the def.
%  Will have form : (or (=?w=8A) (=?w=8A).
% ==============================================
:-load_kif(";;; ND4==worlds
(defrelation WORLD(?w) :=
   )
").
% ==============================================
% ==============================================
:-load_kif(";;; ND5==accessibility relation on worlds
(defrelation WLDR(?w ?v) :=
    (and (WORLD ?w) (WORLD ?v)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND6==Parthood
(defrelation P (?w ?x ?y) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND7==Temporal Parthood
(defrelation P (?w ?x ?y ?t) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND8==Constitution
(defrelation K (?w ?x ?y ?t) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND9==Participation
(defrelation PC (?w ?x ?y ?t) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND10==Quality
(defrelation qt (?w ?x ?y) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND11==Quale
(defrelation ql (?w ?x ?y) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y)))
").
% ==============================================
% ==============================================
:-load_kif(";;; ND12==Quale (temporal)
(defrelation ql (?w ?x ?y ?t) :=>
    (and (WORLD ?w) (PARTICULAR ?x) (PARTICULAR ?y) (PARTICULAR ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA1==NEW total domain
(forall (?x)
        (or (PARTICULAR ?x) (UNIVERSAL ?x) (WORLD ?x)))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA2==partition of the domain
(forall (?x)
        (and (<=> (PARTICULAR ?x)
                  (and (not (UNIVERSAL ?x)) (not (WORLD ?x))))
             (<=> (UNIVERSAL ?x)
                  (and (not (PARTICULAR ?x)) (not (WORLD ?x))))
             (<=> (WORLD ?x)
                  (and (not (PARTICULAR ?x)) (not (UNIVERSAL ?x))))))
").
% ==============================================
% Formal Characterization PRINCIPLES USED IN THE TRANSLATION IN KIF: Modal operators of possibility and necessity are translated in the standard
% way, see for instance p516 of Handbook of Logic in AI and Logic Prog. Vol4; The indeces of relations are included prefixing a dot 
% (we preserve the capital or  lower case distinction)
%  These are the only predicates (with their arity) that do not have possible worlds  as arguments:  
%  X_1,PARTICULAR_1,UNIVERSAL_1,=_2 No need for Barcan formulas, the domain of particulars turns out to be unique  in the 
%  translation WLDR is an equivalence relation (from corrispondence theory, this implies  that WLDR is a relation for S5). 
%  The axioms (NA10)-(NA12) are not necessary  because of our definition of WLDR.
%
% ==============================================
%  NA10 == forall (?w0) (=> (WORLD ?w0) (WLDR ?w0 ?w0)))
% ==============================================
%  NA11 == forall (?w0 ?w1)
%      (=> (and (WLDR ?w0 ?w1) (WORLD ?w0) (WORLD ?w1))
%          (WLDR ?w1 ?w0)))
% ==============================================
%  NA12 == forall (?w0 ?w1 ?w2)
%      (=> (and (WLDR ?w0 ?w1)
%               (WLDR ?w1 ?w2)
%               (WORLD ?w0)
%               (WORLD ?w1)
%               (WORLD ?w2))
%          (WLDR ?w0 ?w2)))
% ==============================================
%  ***THE UNIVERSALS ARE NECESSARILY NON-EMPY***
% ==============================================
:-load_kif(";;; NA14==-- axiom
(forall (?w ?f) (=> (and (UNIVERSAL ?f) (WORLD ?w))
                       (NEP ?w ?f)))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA15==-- axiom
(forall (?w ?f) (=> (and (UNIVERSAL ?f) (WORLD ?w))
                       (or (not (X ?f)) (RG ?w ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA16==Instances of PT -- axiom
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
").
% ==============================================
% ==============================================
:-load_kif(";;; NA17==Instances of SB -- axiom
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
").
% ==============================================
% ==============================================
:-load_kif(";;; NA18==Existence of sum
(forall (?w0 ?x ?y)
           (=> (and (PARTICULAR ?x) (PARTICULAR ?y) (WORLD ?w0))
               (exists (?z)
                       (and (PARTICULAR ?z) (+ ?w0 ?x ?y ?z)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA19==Existence of sigma
(forall (?w0 ?f)
           (=> (and (UNIVERSAL ?f) (WORLD ?w0))
               (exists (?z)
                       (and (PARTICULAR ?z) (sigma ?w0 ?f ?z)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA20==Existence of sum.t
(forall (?w0 ?x ?y)
           (=> (and (PARTICULAR ?x) (PARTICULAR ?y) (WORLD ?w0))
               (exists (?z)
                       (and (PARTICULAR ?z) (+.t ?w0 ?x ?y ?z)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; NA21==Existence of sigma.t
(forall (?w0 ?f)
           (=> (and (UNIVERSAL ?f) (WORLD ?w0))
               (exists (?z)
                       (and (PARTICULAR ?z) (sigma.t ?w0 ?f ?z)))))
").
% ==============================================
%   this could be added in the def. of UNIVERSAL forall (@f)
%           (<=> (UNIVERSAL @f)
%                (exists (?g @h) (and (UNIVERSAL ?g)
%                                     (or (UNIVERSAL @h) (=@h (listof)))
%                                     (=@f (listof ?g @h))))))
% ==============================================
%    this could be added in the def. of PARTICULAR forall (@x)
%           (<=> (PARTICULAR @x)
%                (exists (?y @z) (and (PARTICULAR ?y)
%                                     (or (PARTICULAR @z) (=@z (listof)))
%                                     (=@x (listof ?y @z))))).
% ==============================================
:-load_kif(";;; D1== RG: Rigid Universal
(defrelation RG (?w0 ?f) :=
    (and (UNIVERSAL ?f)
         (WORLD ?w0)
       (forall (?w ?x)
                 (=> (and (WLDR ?w0 ?w) (WORLD ?w) (PARTICULAR ?x))
                     (=> (?f ?w ?x)
                       (forall (?u)
                                 (=> (and (WLDR ?w ?u) (WORLD ?u))
                                     (?f ?u ?x))))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D2==NEP: Non-Empty Universal
(defrelation NEP (?w0 ?f) :=
    (and (UNIVERSAL ?f)
         (WORLD ?w0)
       (forall (?w)
                 (=> (and (WLDR ?w0 ?w) (WORLD ?w))
                     (exists (?y)
                             (and (PARTICULAR ?y) (?f ?w ?y)))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D3==DJ: Disjoint Universals
(defrelation DJ (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
       (forall (?w ?x)
                 (=> (and (WLDR ?w0 ?w)
                          (WORLD ?w)
                          (PARTICULAR ?x))
                     (not (and (?f ?w ?x) (?g ?w ?x)))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D4==SB: Subsumption
(defrelation SB (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
       (forall (?w ?x)
                 (=> (and (WLDR ?w0 ?w)
                          (WORLD ?w)
                          (PARTICULAR ?x))
                     (or (not (?g ?w ?x)) (?f ?w ?x))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D5==EQ: Equal Universals
(defrelation EQ (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g) (SB ?w0 ?g ?f)).
% ==============================================
:-load_kif(";;; D6==PSB: Properly Subsuming
(defrelation PSB (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g)
         (not (SB ?w0 ?f ?g))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D7==L: Leaf Universal
(defrelation L (?w0 ?f) :=
    (and (UNIVERSAL ?f)
         (WORLD ?w0)
       (forall (?w ?g)
                 (=> (and (WLDR ?w0 ?w)
                          (WORLD ?w)
                          (UNIVERSAL ?g))
                     (or (not (?SB ?w0 ?f ?g)) (EQ ?w0 ?f ?g))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D8==SBL: Leaf Subsumed by
(defrelation SBL (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g) (L ?w0 ?g)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D9==PSBL: Leaf Properly Subsumed by
(defrelation PSBL (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (PSB ?w0 ?f ?g) (L ?w0 ?g)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D10==L__: Leaf in the set X
(defrelation L.X (?w0 ?f) :=
    (and (UNIVERSAL ?f)
         (WORLD ?w0)
         (X ?f)
       (forall (?w ?g)
                 (=> (and (WLDR ?w0 ?w) (WORLD ?w) (UNIVERSAL ?g))
                     (=> (and (?SB ?w ?f ?g) (X ?g))
                         (EQ ?w ?f ?g))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D11==SBL__
(defrelation SBL.X (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (SB ?w0 ?f ?g) (L.X ?w0 ?g)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D12==PSBL__
(defrelation PSBL.X (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f) (UNIVERSAL ?g) (WORLD ?w0) (PSB ?w0 ?f ?g) (L.X ?w0 ?g)))
").
% ==============================================
% Definition (D13) is left for expressivity. In practice it becomes superfluous since the user needs to
%  give a list of the n-tuple satisfying relation PT in axiom (NA17)
% ==============================================
:-load_kif(";;; D13==PT: Partition
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
                           (/=?h ?k))
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
").
% ==============================================
% Mereological Definitions
% ==============================================
:-load_kif(";;; D14==PP: Proper Part
(defrelation PP (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (P ?w0 ?x ?y)
         (not (P ?w0 ?y ?x))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D15==O: Overlap
(defrelation O (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?z) (and (PARTICULAR ?z)
                           (P ?w0 ?z ?x)
                           (P ?w0 ?z ?y)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D16==At: Atom
(defrelation At (?w0 ?x) :=
    (and (PARTICULAR ?x)
         (WORLD ?w0)
         (not (exists (?y) (and (PARTICULAR ?y)
                                (PP ?w0 ?y ?x))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D17==AtP: Atomic Part
(defrelation AtP (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (P ?w0 ?x ?y)
         (At ?w0 ?x)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D18==__ Binary Sum
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
                     (=?z1 ?z)))))
").
% ==============================================
% Sum Note: the rendition in KIF is weaker than the corresponding definition  in modal FOL here ?f has to be one of the universal introduced explicitly. 
% [A possible way out: use string-variables (@f) to code Boolean combinations of universals. ]
% ==============================================
:-load_kif(";;; D19==(general)
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
                         (=?z1 ?z)))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D20==PP: Temporary Proper Part
(defrelation PP (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (P ?w0 ?x ?y ?t)
         (not (P ?w0 ?y ?x ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D21==O: Temporary Overlap
(defrelation O (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (exists (?z) (and (PARTICULAR ?z)
                           (P ?w0 ?z ?x ?t)
                           (P ?w0 ?z ?y ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D22==At: Temporary Atom
(defrelation At (?w0 ?x ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (not (exists (?y)
                    (and (PARTICULAR ?y) (PP ?w0 ?y ?x ?t))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D23==AtP: Temporary Atomic Part
(defrelation AtP (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (P ?w0 ?x ?y ?t)
         (At ?w0 ?x ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D24==Coincidence
(defrelation=.t (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (P ?w0 ?x ?y ?t)
         (P ?w0 ?y ?x ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D25==CP: Constant Part
(defrelation CP (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?t)
                 (and (PARTICULAR ?t) (PRE ?w0 ?y ?t)))
       (forall (?t)
                 (=> (and (PARTICULAR ?t) (PRE ?w0 ?y ?t))
                     (P ?w0 ?x ?y ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D26 ==
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
                   (=?z1 ?z)))))
").
% ==============================================
% NOTE: this rendition includes only the listed universal, for instance, no Boolean combination of universals is included [see also comment on (D19)==
% ==============================================
:-load_kif(";;; D27 ==
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
                             (=?z1 ?z))))))))
").
% ==============================================
% Quality
% ==============================================
:-load_kif(";;; D28==dqt: Direct Quality
(defrelation dqt (?w0 ?x ?y) :=
    (and (WORLD ?w0)
         (PARTICULAR ?x)
         (PARTICULAR ?y)
         (qt ?w0 ?x ?y)
         (not (exists (?z)
                      (and (PARTICULAR ?z)
                           (qt ?w0 ?x ?z)
                           (qt ?w0 ?z ?y))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D29==qt: Quality of type
(defrelation qtf (?w0 ?f ?x ?y) :=
    (and (UNIVERSAL ?f)
         (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (qt ?w0 ?x ?y)
         (?f ?w0 ?x)
         (SBL.X ?w0 Q ?f)))
").
% ==============================================
% Temporal and Spatial Quale
% ==============================================
:-load_kif(";;; D30==ql_T,PD
(defrelation ql.T.PD (?w0 ?t ?x) :=
    (and (PARTICULAR ?t)
         (PARTICULAR ?x)
         (WORLD ?w0)
         (PD ?w0 ?x)
         (exists (?z) (and (PARTICULAR ?z)
                           (qtf ?w0 TL ?z ?x)
                           (ql ?w0 ?t ?z)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D31==ql_T,ED
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
                               (=?t1 ?t))))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D32==ql_T,TQ
(defrelation ql.T.TQ (?w0 ?t ?x) :=
    (and (PARTICULAR ?t)
         (PARTICULAR ?x)
         (WORLD ?w0)
         (TQ ?w0 ?x)
         (exists (?z) (and (PARTICULAR ?z)
                           (qt ?w0 ?x ?z)
                           (ql.T.PD ?w0 ?t ?z)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D33==ql_T,PQ_or_AQ
(defrelation ql.T.PQAQ (?w0 ?t ?x) :=
    (and (PARTICULAR ?t)
         (PARTICULAR ?x)
         (WORLD ?w0)
         (or (PQ ?w0 ?x) (AQ ?w0 ?x))
         (exists (?z) (and (PARTICULAR ?z)
                           (qt ?w0 ?x ?z)
                           (ql.T.ED ?w0 ?t ?z)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D34==ql_T,Q
(defrelation ql.T.Q (?w0 ?t ?x) :=
    (and (PARTICULAR ?t)
         (PARTICULAR ?x)
         (WORLD ?w0)
         (or (ql.T.TQ ?w0 ?t ?x)
             (ql.T.PQAQ ?w0 ?t ?x))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D35==ql_T: Temporal Quale
(defrelation ql.T (?w0 ?t ?x) :=
    (and (PARTICULAR ?t)
         (PARTICULAR ?x)
         (WORLD ?w0)
         (or (ql.T.ED ?w0 ?t ?x)
             (ql.T.PD ?w0 ?t ?x)
             (ql.T.Q ?w0 ?t ?x))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D36==ql_S,PED
(defrelation ql.S.PED (?w0 ?s ?x ?t) :=
    (and (PARTICULAR ?s)
         (PARTICULAR ?x)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (PED ?w0 ?x)
         (exists (?z) (and (PARTICULAR ?z)
                           (qtf ?w0 SL ?z ?x)
                           (ql ?w0 ?s ?z ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D37==ql_S,PQ
(defrelation ql.S.PQ (?s ?x ?t) :=
    (and (PARTICULAR ?s)
         (PARTICULAR ?x)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (PQ ?w0 ?x)
         (exists (?z) (and (PARTICULAR ?z)
                           (qt ?w0 ?x ?z)
                           (ql.S.PED ?w0 ?s ?z ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D38==ql_S,PD
(defrelation ql.S.PD (?w0 ?s ?x ?t) :=
    (and (PARTICULAR ?s)
         (PARTICULAR ?x)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (PD ?w0 ?x)
         (exists (?z) (and (PARTICULAR ?z)
                           (mppc ?w0 ?z ?x)
                           (ql.S.PED ?w0 ?s ?z ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D39==ql_S: Spatial Quale
(defrelation ql.S (?w0 ?s ?x ?t) :=
    (and (PARTICULAR ?s)
         (PARTICULAR ?x)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (or (ql.S.PED ?w0 ?s ?x ?t)
             (ql.S.PQ ?w0 ?s ?x ?t)
             (ql.S.PD ?w0 ?s ?x ?t))))
").
% ==============================================
% ==============================================
% =%  Being present
% ==============================================
:-load_kif(";;; D40==PRE: Being Present at
(defrelation PRE (?w0 ?x ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (exists (?u) (and (PARTICULAR ?u)
                           (ql.T ?w0 ?u ?x)
                           (P ?w0 ?t ?u)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D41==PRE: Being Present in at
(defrelation PRE (?w0 ?x ?s ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?s)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (PRE ?w0 ?x ?t)
         (exists (?u) (and (PARTICULAR ?u)
                           (ql.S ?w0 ?u ?x ?t)
                           (P ?w0 ?s ?u)))))
").
% ==============================================
% Inclusion and Coincidence
% ==============================================
:-load_kif(";;; D42==Temporal Inclusion
(defrelation incl.T (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?t ?u) (and (PARTICULAR ?t)
                              (PARTICULAR ?u)
                              (ql.T ?w0 ?t ?x)
                              (ql.T ?w0 ?u ?y)
                              (P ?w0 ?t ?u)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D43==Proper Temporal Inclusion
(defrelation sincl.T (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?t ?u) (and (PARTICULAR ?t)
                              (PARTICULAR ?u)
                              (ql.T ?w0 ?t ?x)
                              (ql.T ?w0 ?u ?y)
                              (PP ?w0 ?t ?u)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D44==Temporary Spatial Inclusion
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D45==Temp. Proper Sp. Inclusion
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D46==Spatio-temporal Inclusion
(defrelation incl.S.T (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
       (PARTICULAR ?y)
       (WORLD ?w0)
       (exists (?t) (and (PARTICULAR ?t) (PRE ?w0 ?x ?t)))
     (forall (?t) (=> (and (PARTICULAR ?t) (PRE ?w0 ?x ?t))
                        (incl.S.t ?w0 ?x ?y ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D47==Spatio-temp. Incl. during
(defrelation incl.S.T.t (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (PRE ?w0 ?x ?t)
       (forall (?u) (=> (and (PARTICULAR ?u) (AtP ?w0 ?u ?t))
                          (incl.S.t ?w0 ?x ?y ?u)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D48==Temporal Coincidence
(defrelation ~.T (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (incl.T ?w0 ?x ?y)
         (incl.T ?w0 ?y ?x)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D49==Temporary Spatial Coincidence
(defrelation ~.S.t (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (incl.S.t ?w0 ?x ?y ?t)
         (incl.S.t ?w0 ?y ?x ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D50==Spatio-temporal Coincidence
(defrelation ~.S.T (?w0 ?x ?y) :=
    (and (WORLD ?w0)
         (PARTICULAR ?x)
         (PARTICULAR ?y)
         (incl.S.T ?w0 ?x ?y)
         (incl.S.T ?w0 ?y ?x)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D51==Spatio-temp. Coincidence during
(defrelation ~.S.T.t (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (PRE ?w0 ?x ?t)
       (forall (?u) (=> (and (PARTICULAR ?u) (AtP ?w0 ?u ?t))
                          (~.S.t ?w0 ?x ?y ?u)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D52==O_T: Temporal Overlap
(defrelation O.T (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?t ?u) (and (PARTICULAR ?t)
                              (PARTICULAR ?u)
                              (ql.T ?w0 ?t ?x)
                              (ql.T ?w0 ?u ?y)
                              (O ?w0 ?t ?u)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D53==O_S,t: Temporary Spatial Overlap
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
").
% ==============================================
% Perdurant
% ==============================================
:-load_kif(";;; D54==P_T: Temporal Part
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D55==P_S: Spatial Part
(defrelation P.S (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (PD ?w0 ?x)
         (P ?w0 ?x ?y)
         (~.T ?w0 ?x ?y)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D56==NEP_S: Strongly Non-Empty
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D57==CM: Cumulative
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D58==CM=97: Anti-Cumulative
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D59==HOM: Homeomerous
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D60==HOM=97: Anti-Homeom.
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D61==AT: Atomic
(defrelation AT (?w0 ?f) :=
    (and (UNIVERSAL ?f)
         (WORLD ?w0)
         (SB ?w0 PD ?f)
       (forall (?w ?x) (=> (and (WLDR ?w0 ?w)
                                  (WORLD ?w)
                                  (PARTICULAR ?x)
                                  (?f ?w ?x))
                             (At ?w ?x)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D62==AT=97: Anti-Atomic
(defrelation AT~ (?w0 ?f) :=
    (and (UNIVERSAL ?f)
         (WORLD ?w0)
         (SB ?w0 PD ?f)
       (forall (?w ?x) (=> (and (WLDR ?w0 ?w)
                                  (WORLD ?w)
                                  (PARTICULAR ?x)
                                  (?f ?w ?x))
                             (not (At ?w ?x))))))
").
% ==============================================
% ==============================================
% =%  Participation
% ==============================================
:-load_kif(";;; D63==PC_C: Constant Participation
(defrelation PC.C (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?t) (and (PARTICULAR ?t) (PRE ?w0 ?y ?t)))
       (forall (?t) (=> (and (PARTICULAR ?t)
                               (PRE ?w0 ?y ?t))
                          (PC ?w0 ?x ?y ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D64==PC_T: Temporary Total Particip.
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D65==PC_T: Total Participation
(defrelation PC.T (?w0 ?x ?y) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (WORLD ?w0)
         (exists (?t) (and (PARTICULAR ?t)
                           (ql.T ?w0 ?t ?y)
                           (PC.T ?w0 ?x ?y ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D66==mpc: Maximal Participant
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
                     (=?x1 ?x)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D67==mppc: Maximal Physical Participant
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
                     (=?x1 ?x)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D68==lf: Life
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
                     (=?u ?x)))))
").
% ==============================================
% Dependence
% ==============================================
:-load_kif(";;; D69==SD: Specific Constant Dep.
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D70==SD: Specific Const. Dep. included in def (D69)
").
% ==============================================
% ==============================================
:-load_kif(";;; D71==GD: Generic Const. Dep.
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D72==D: Constant Dependence
(defrelation D (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (or (SD ?w0 ?f ?g) (GD ?w0 ?f ?g))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D73==OD: One-sided Constant Dependence
(defrelation OD (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (D ?w0 ?f ?g)
         (not (D ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D74==OSD: One-sided Specific Constant Dependence
(defrelation OSD (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (SD ?w0 ?f ?g)
         (not (D ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D75==OGD: One-sided Generic Constant Dependence
(defrelation OGD (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GD ?w0 ?f ?g)
         (not (D ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D76==MSD: Mutual Specific Constant Dependence
(defrelation MSD (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (SD ?w0 ?f ?g)
         (SD ?w0 ?g ?f)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D77==MGD: Mutual Generic Constant Dependence
(defrelation MGD (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GD ?w0 ?f ?g)
         (GD ?w0 ?g ?f)))
").
% ==============================================
% Spatial Dependence
% ==============================================
:-load_kif(";;; D78==SD_S: Specific Spatial Dependence
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D79==PSD_S: Partial Specific Spatial Dependence
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D80==P-1SD_S: Inverse Partial Specific Spatial Dependence
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
").
% ==============================================
% D81==SD_S included in def (D78)
% ==============================================
%  D82==PSD_S included in def (D79)
% ==============================================
%  D83==P-1SD_S included in def (D80)
% ==============================================
:-load_kif(";;; D84==GD_S: Generic Spatial Dependence
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D85==PGD_S: Partial Generic Spatial Dependence
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D86==P-1GD_S: Inverse Partial Generic Spatial Dependence
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D87==DGD_S: Direct Generic Spatial Dependence
(defrelation DGD.S (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GD.S ?w0 ?f ?g)
         (not (exists (?h) (and (UNIVERSAL ?h)
                                (GD.S ?w0 ?f ?h)
                                (GD.S ?w0 ?h ?g))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D88==Sdt_S: Temporary Specific Spatial Dependence
(defrelation SDt.S (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (SD.S ?w0 ?x ?y)
         (PRE ?w0 ?x ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D89==GDt_S: Temp. Gen. Sp. Dep.
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D90==DGDt_S: Temp. Direct Sp. Dep.
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D91==OSD_S: One-sided Specific Spatial Dependence
(defrelation OSD.S (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (SD.S ?w0 ?f ?g)
         (not (D ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D92==OGD_S: One-sided Generic Spatial Dependence
(defrelation OGD.S (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GD.S ?w0 ?f ?g)
         (not (D ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D93==MSD_S: Mutual Specific Spatial Dependence
(defrelation MSD.S (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (SD.S ?w0 ?f ?g)
         (SD.S ?w0 ?g ?f)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D94==MGD_S: Mutual Generic Spatial Dependence
(defrelation MGD.S (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GD.S ?w0 ?f ?g)
         (GD.S ?w0 ?g ?f)))
").
% ==============================================
% Constitution
% ==============================================
:-load_kif(";;; D95==DK: Direct Constitution
(defrelation DK (?w0 ?x ?y ?t) :=
    (and (PARTICULAR ?x)
         (PARTICULAR ?y)
         (PARTICULAR ?t)
         (WORLD ?w0)
         (K ?w0 ?x ?y ?t)
         (not (exists (?z) (and (PARTICULAR ?z)
                                (K ?w0 ?x ?z ?t)
                                (K ?w0 ?z ?y ?t))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D96==SK: Constantly Specifically Constituted by
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
").
% ==============================================
% D97==SK: Constantly Specifically Constituted by included in def (D96)
% ==============================================
:-load_kif(";;; D98==GK: Constantly Generically Constituted by
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
").
% ==============================================
% ==============================================
:-load_kif(";;; D99==K__Constituted by
(defrelation K (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (or (SK ?w0 ?f ?g) (GK ?w0 ?f ?g))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D100==OSK: One-sided Cons. Specif. Const. by
(defrelation OSK (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (SK ?w0 ?f ?g)
         (not (K ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D101==OGK: One-sided Cons. Generic. Const. by
(defrelation OGK (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GK ?w0 ?f ?g)
         (not (K ?w0 ?g ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; D102==MSK: Mutual Specific Constitution
(defrelation MSK (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (SK ?w0 ?f ?g)
         (SK ?w0 ?g ?f)))
").
% ==============================================
% ==============================================
:-load_kif(";;; D103==MGK: Mutual Generic Constitution
(defrelation MSK (?w0 ?f ?g) :=
    (and (UNIVERSAL ?f)
         (UNIVERSAL ?g)
         (WORLD ?w0)
         (GK ?w0 ?f ?g)
         (GK ?w0 ?g ?f)))
").
% ==============================================
% Characterization of functions and relations Parthood Argument Restrictions
% ==============================================
:-load_kif(";;; A1 ==
(forall (?w0 ?x ?y)
       (=> (and (P ?w0 ?x ?y)
                (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y))
           (and (or (AB ?w0 ?x) (PD ?w0 ?x))
                (or (AB ?w0 ?y) (PD ?w0 ?y)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A2 ==
(forall (?w0 ?x ?y)
       (=> (and (P ?w0 ?x ?y)
                (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y))
           (<=> (PD ?w0 ?x) (PD ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A3 ==
(forall (?w0 ?x ?y)
       (=> (and (P ?w0 ?x ?y)
                (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y))
           (<=> (AB ?w0 ?x)
                (AB ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A4 ==
(forall (?w0 ?x ?y ?f)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (UNIVERSAL ?f)
                (P ?w0 ?x ?y)
                (SB ?w0 R ?f)
                (X ?f))
           (<=> (?f ?w0 ?x) (?f ?w0 ?y))))
").
% ==============================================
% Ground Axioms
% ==============================================
:-load_kif(";;; A5 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (or (AB ?w0 ?x) (PD ?w0 ?x)))
           (P ?w0 ?x ?x)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A6 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (P ?w0 ?x ?y)
                (P ?w0 ?y ?x))
           (=?x ?y)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A7 ==
(forall (?w0 ?x ?y ?z)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?z)
                (P ?w0 ?x ?y)
                (P ?w0 ?y ?z))
           (P ?w0 ?x ?z)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A8 ==
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
").
% ==============================================
% ==============================================
:-load_kif(";;; A9 == Note: this version in KIF consider only the universal explicitly listed see comment on (D19)==
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
").
% ==============================================
% Temporary Parthood Argument restrictions
% ==============================================
:-load_kif(";;; A10 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (P ?w0 ?x ?y ?t))
           (and (ED ?w0 ?x) (ED ?w0 ?y) (T ?w0 ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A11 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (P ?w0 ?x ?y ?t))
           (<=> (PED ?w0 ?x) (PED ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A12 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (P ?w0 ?x ?y ?t))
           (<=> (NPED ?w0 ?x) (NPED ?w0 ?y))))
").
% ==============================================
% Ground Axioms
% ==============================================
:-load_kif(";;; A13 ==
(forall (?w0 ?x ?y ?z ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?z)
                (PARTICULAR ?t)
                (P ?w0 ?x ?y ?t)
                (P ?w0 ?y ?z ?t))
           (P ?w0 ?x ?z ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A14 ==
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
").
% ==============================================
% ==============================================
:-load_kif(";;; A15 == [see comment on (D19)==
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
").
% ==============================================
% Links With Other Primitives
% ==============================================
:-load_kif(";;; A16 ==
(forall (?w0 ?x ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?t)
                (ED ?w0 ?x)
                (PRE ?w0 ?x ?t))
           (P ?w0 ?x ?x ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A17 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (P ?w0 ?x ?y ?t))
           (and (PRE ?w0 ?x ?t) (PRE ?w0 ?y ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A18 ==
(forall (?w0 ?x ?y ?t ?u)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (PARTICULAR ?u)
                (P ?w0 ?x ?y ?t)
                (P ?w0 ?u ?t))
           (P ?w0 ?x ?y ?u)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A19 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (PED ?w0 ?x)
                (P ?w0 ?x ?y ?t))
           (incl.S.t ?w0 ?x ?y ?t)))
").
% ==============================================
% Constitution Argument restrictions
% ==============================================
:-load_kif(";;; A20 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t))
           (and (or (ED ?w0 ?x) (PD ?w0 ?x))
                (or (ED ?w0 ?y) (PD ?w0 ?y))
                (T ?w0 ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A21 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t))
           (<=> (PED ?w0 ?x) (PED ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A22 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t))
           (<=> (NPED ?w0 ?x) (NPED ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A23 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t))
           (<=> (PD ?w0 ?x) (PD ?w0 ?y))))
").
% ==============================================
% Ground Axioms
% ==============================================
:-load_kif(";;; A24 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t))
           (not (K ?w0 ?y ?x ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A25 ==
(forall (?w0 ?x ?y ?z ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?z)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t)
                (K ?w0 ?y ?z ?t))
           (K ?w0 ?x ?z ?t)))
").
% ==============================================
% Links with other Primitives
% ==============================================
:-load_kif(";;; A26 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (K ?w0 ?x ?y ?t))
           (and (PRE ?w0 ?x ?t) (PRE ?w0 ?y ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A27 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t))
           (<=> (K ?w0 ?x ?y ?t)
              (forall (?u)
                    (=> (and (PARTICULAR ?u) (P ?w0 ?u ?t))
                        (K ?w0 ?x ?y ?u))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A28 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (PED ?w0 ?x)
                (K ?w0 ?x ?y ?t))
           (~.S.t ?w0 ?x ?y ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A29 ==
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
").
% ==============================================
% Links between Categories
% ==============================================
:-load_kif(";;; A30 ==
(forall (?w0) (=> (WORLD ?w0) (GK ?w0 NAPO M)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A31 ==
(forall (?w0) (=> (WORLD ?w0) (GK ?w0 APO NAPO)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A32 ==
(forall (?w0) (=> (WORLD ?w0) (GK ?w0 SC SAG)))
").
% ==============================================
% Participation Argument restrictions
% ==============================================
:-load_kif(";;; A33 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (PC ?w0 ?x ?y ?t))
           (and (ED ?w0 ?x) (PD ?w0 ?y) (T ?w0 ?t))))
").
% ==============================================
% Existential Axioms
% ==============================================
:-load_kif(";;; a34 ==
(forall (?w0 ?x ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?t)
                (PD ?w0 ?x)
                (PRE ?w0 ?x ?t))
           (exists (?y)
                (and (PARTICULAR ?y) (PC ?w0 ?y ?x ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a35 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0) (PARTICULAR ?x) (ED ?w0 ?x))
           (exists (?y ?t)
                (and (PARTICULAR ?y) (PARTICULAR ?t) (PC ?w0 ?x ?y ?t)))))
").
% ==============================================
% Links with other Primitives
% ==============================================
:-load_kif(";;; a36 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (PC ?w0 ?x ?y ?t))
           (and (PRE ?w0 ?x ?t) (PRE ?w0 ?y ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a37 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t))
           (<=> (PC ?w0 ?x ?y ?t)
              (forall (?u)
                    (=> (and (PARTICULAR ?u) (P ?w0 ?u ?t))
                        (PC ?w0 ?x ?y ?u))))))
").
% ==============================================
% Quality Argument restrictions:
% ==============================================
:-load_kif(";;; a38 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (qt ?w0 ?x ?y))
           (and (Q ?w0 ?x)
                (or (Q ?w0 ?y) (ED ?w0 ?y) (PD ?w0 ?y)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a39 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (qt ?w0 ?x ?y))
           (<=> (TQ ?w0 ?x)
                (or (TQ ?w0 ?y) (PD ?w0 ?y)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a40 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (qt ?w0 ?x ?y))
           (<=> (PQ ?w0 ?x)
                (or (PQ ?w0 ?y) (PED ?w0 ?y)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a41 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (qt ?w0 ?x ?y))
           (<=> (AQ ?w0 ?x)
                (or (AQ ?w0 ?y) (NPED ?w0 ?y)))))
").
% ==============================================
% Ground Axioms:
% ==============================================
:-load_kif(";;; a42 ==
(forall (?w0 ?x ?y ?z)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?z)
                (qt ?w0 ?x ?y)
                (qt ?w0 ?y ?z))
           (qt ?w0 ?x ?z)))
").
% ==============================================
% ==============================================
:-load_kif(";;; a43 ==
(forall (?w0 ?x ?y ?z)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?z)
                (qt ?w0 ?x ?y)
                (qt ?w0 ?x ?z))
           (=?y ?z)))
").
% ==============================================
% ==============================================
:-load_kif(";;; a44 ==
(forall (?w0 ?f ?x ?y ?z)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?z)
                (qtf ?w0 ?f ?x ?y)
                (qtf ?w0 ?f ?z ?y))
           (=?x ?z)))
").
% ==============================================
% ==============================================
:-load_kif(";;; a45 ==
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
").
% ==============================================
% Existential Axioms:
% ==============================================
:-load_kif(";;; a46 ==
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
                             (=?z ?y)))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a47 ==
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
                             (=?z ?y)))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a48 ==
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
                             (=?z ?y)))))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a49 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0) (PARTICULAR ?x) (PD ?w0 ?x))
           (exists (?y)
                (and (PARTICULAR ?y) (qtf ?w0 TL ?y ?x)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a50 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0) (PARTICULAR ?x) (PED ?w0 ?x))
           (exists (?y)
                (and (PARTICULAR ?y) (qtf ?w0 SL ?y ?x)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; a51 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0) (PARTICULAR ?x) (NPED ?w0 ?x))
           (exists (?f ?y)
                (and (PARTICULAR ?y)
                     (UNIVERSAL ?f)
                     (SBL ?w0 AQ ?f)
                     (qtf ?w0 ?f ?y ?x)))))
").
% ==============================================
% Quale Immediate Quale Argument restrictions:
% ==============================================
:-load_kif(";;; A52 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (ql ?w0 ?x ?y))
           (and (TR ?w0 ?x) (TQ ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A53 ==
(forall (?w0 ?x ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (ql ?w0 ?x ?y)
                (TL ?w0 ?y))
           (T ?w0 ?x)))
").
% ==============================================
% Basic Axioms:
% ==============================================
:-load_kif(";;; A54 ==
(forall (?w0 ?x ?x1 ?y)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?x1)
                (PARTICULAR ?y)
                (ql ?w0 ?x ?y)
                (ql ?w0 ?x1 ?y))
           (=?x ?x1)))
").
% ==============================================
% Existential Axioms:
% ==============================================
:-load_kif(";;; A55 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (TQ ?w0 ?x))
           (exists (?y)
                (and (PARTICULAR ?y) (ql ?w0 ?y ?x)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A56 ==
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
").
% ==============================================
% ==============================================
:-load_kif(";;; A57 ==
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
").
% ==============================================
% Temporary Quale Argument restrictions:
% ==============================================
:-load_kif(";;; A58 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (ql ?w0 ?x ?y ?t))
           (and (or (PR ?w0 ?x) (AR ?w0 ?x))
                (or (PQ ?w0 ?y) (AQ ?w0 ?y))
                (T ?w0 ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A59 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (ql ?w0 ?x ?y ?t))
           (<=> (PR ?w0 ?x) (PQ ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A60 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (ql ?w0 ?x ?y ?t))
           (<=> (AR ?w0 ?x) (AQ ?w0 ?y))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A61 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (ql ?w0 ?x ?y ?t)
                (SL ?w0 ?y))
           (S ?w0 ?x)))
").
% ==============================================
% Existential Axioms:
% ==============================================
:-load_kif(";;; A62 ==
(forall (?w0 ?x)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (or (PQ ?w0 ?x) (AQ ?w0 ?x))
                (PRE ?w0 ?x ?t))
           (exists (?y)
                (and (PARTICULAR ?y) (ql ?w0 ?y ?x ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A63 ==
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
").
% ==============================================
% ==============================================
:-load_kif(";;; A64 ==
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
").
% ==============================================
% Link with Parthood and extension:
% ==============================================
:-load_kif(";;; A65 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (ql ?w0 ?x ?y ?t))
           (PRE ?w0 ?y ?t)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A66 ==
(forall (?w0 ?x ?y ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t))
           (<=> (ql ?w0 ?x ?y ?t)
              (forall (?u)
                        (=> (and (PARTICULAR ?u) (P ?w0 ?u ?t))
                            (ql ?w0 ?x ?y ?u))))))
").
% ==============================================
% Dependence and Spatial Dependence Links between categories
% ==============================================
:-load_kif(";;; A67 ==
(forall (?w0) (=> (WORLD ?w0) (MSD ?w0 TQ PD)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A68 ==
(forall (?w0) (=> (WORLD ?w0) (MSD.S ?w0 PQ PED)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A69 ==
(forall (?w0) (=> (WORLD ?w0) (MSD ?w0 AQ NPED)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A70 ==
(forall (?w0) (=> (WORLD ?w0) (OGD ?w0 F NAPO)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A71 ==
(forall (?w0) (=> (WORLD ?w0) (OSD ?w0 MOB APO)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A72 ==
(forall (?w0) (=> (WORLD ?w0) (OGD ?w0 SAG APO)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A73 ==
(forall (?w0) (=> (WORLD ?w0) (OGD ?w0 NASO SC)))
").
% ==============================================
% ==============================================
:-load_kif(";;; A74 ==
(forall (?w0) (=> (WORLD ?w0) (OD ?w0 NPED PED)))
").
% ==============================================
% Characterization of Categories Perdurant Conditions on Perdurant's Leaves
% ==============================================
:-load_kif(";;; A75 ==
(forall (?w0 ?f)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (PSBL ?w0 ACH ?f))
           (and (NEP.S ?w0 ?f) (CM~ ?w0 ?f) (AT ?w0 ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A76 ==
(forall (?w0 ?f)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (PSBL ?w0 ACC ?f))
           (and (NEP.S ?w0 ?f) (CM~ ?w0 ?f) (AT~ ?w0 ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A77 ==
(forall (?w0 ?f)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (PSBL ?w0 ST ?f))
           (and (NEP.S ?w0 ?f) (CM ?w0 ?f) (HOM ?w0 ?f))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A78 ==
(forall (?w0 ?f)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (PSBL ?w0 PRO ?f))
           (and (NEP.S ?w0 ?f) (CM ?w0 ?f) (HOM~ ?w0 ?f))))
").
% ==============================================
% Existential Axioms
% ==============================================
:-load_kif(";;; A79 ==
  (forall (?w0)
       (=> (WORLD ?w0)
           (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 ACH ?f)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A80 ==
  (forall (?w0)
       (=> (WORLD ?w0)
           (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 ACC ?f)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A81 ==
  (forall (?w0)
       (=> (WORLD ?w0)
           (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 ST ?f)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; A82 ==
  (forall (?w0)
       (=> (WORLD ?w0)
           (exists (?f) (and (UNIVERSAL ?f) (PSBL ?w0 PRO ?f)))))
").
% ==============================================
%  =========================================THEOREMS General Properties
% ==============================================
:-load_kif(";;; T1 ==
  (forall (?w0 ?x ?t)
       (=> (and (WORLD ?w0) (PARTICULAR ?x) (PARTICULAR ?t))
           (not (K ?w0 ?x ?x ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; T2 ==
  (forall (?w0 ?f ?g)
       (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (SK ?w0 ?f ?g))
           (SD ?w0 ?f ?g)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T3 ==
  (forall (?w0 ?f ?g)
       (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (GK ?w0 ?f ?g))
           (GD ?w0 ?f ?g)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T4 ==
  (forall (?w0 ?f ?g ?h)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (UNIVERSAL ?g)
                (UNIVERSAL ?h)
                (SK ?w0 ?f ?g)
                (SK ?w0 ?g ?h)
                (DJ ?w0 ?f ?h))
           (SK ?w0 ?f ?h)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T5 ==
  (forall (?w0 ?f ?g ?h)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (UNIVERSAL ?g)
                (UNIVERSAL ?h)
                (GK ?w0 ?f ?g)
                (GK ?w0 ?g ?h)
                (DJ ?w0 ?f ?h))
           (GK ?w0 ?f ?h)))
").
% ==============================================
% Ground Properties
% ==============================================
:-load_kif(";;; T6 ==
  (forall (?w0 ?x ?t)
       (=> (and (WORLD ?w0) (PARTICULAR ?x) (PARTICULAR ?t))
           (not (PC ?w0 ?x ?x ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; T7 ==
  (forall (?w0 ?x ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?y)
                (PARTICULAR ?t)
                (PC ?w0 ?x ?y ?t))
           (not (PC ?w0 ?y ?x ?t))))
").
% ==============================================
% ==============================================
:-load_kif(";;; T8 ==
  (forall (?w0 ?x)
       (=> (and (WORLD ?w0) (PARTICULAR ?x))
           (not (qt ?w0 ?x ?x))))
").
% ==============================================
% General properties
% ==============================================
:-load_kif(";;; T9 ==
  (forall (?w0 ?f ?g ?h)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (UNIVERSAL ?g)
                (UNIVERSAL ?h)
                (SD ?w0 ?f ?g)
                (SD ?w0 ?g ?h)
                (DJ ?w0 ?f ?h))
           (SD ?w0 ?f ?h)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T10 ==
  (forall (?w0 ?f ?g ?h)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (UNIVERSAL ?g)
                (UNIVERSAL ?h)
                (GD ?w0 ?f ?g)
                (GD ?w0 ?g ?h)
                (DJ ?w0 ?f ?h))
           (GD ?w0 ?f ?h)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T11 ==
  (forall (?w0 ?f ?g ?h)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (UNIVERSAL ?g)
                (UNIVERSAL ?h)
                (SD ?w0 ?f ?g)
                (GD ?w0 ?g ?h)
                (DJ ?w0 ?f ?h))
           (GD ?w0 ?f ?h)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T12 ==
  (forall (?w0 ?f ?g ?h)
       (=> (and (WORLD ?w0)
                (UNIVERSAL ?f)
                (UNIVERSAL ?g)
                (UNIVERSAL ?h)
                (GD ?w0 ?f ?g)
                (SD ?w0 ?g ?h)
                (DJ ?w0 ?f ?h))
           (GD ?w0 ?f ?h)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T13 ==
  (forall (?w0 ?f ?g)
       (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (SD.S ?w0 ?f ?g))
           (SD ?w0 ?f ?g)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T14 ==
  (forall (?w0 ?f ?g)
       (=> (and (WORLD ?w0) (UNIVERSAL ?f) (UNIVERSAL ?g) (GD.S ?w0 ?f ?g))
           (GD ?w0 ?f ?g)))
").
% ==============================================
% Being Present
% ==============================================
:-load_kif(";;; T15 ==
  (forall (?w0 ?x)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (or (ED ?w0 ?x) (PD ?w0 ?x) (Q ?w0 ?x)))
           (exists (?t)
                (and (PARTICULAR ?t) (PRE ?w0 ?x ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; T16 ==
  (forall (?w0 ?x ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?t)
                (or (PED ?w0 ?x) (PQ ?w0 ?x))
                (PRE ?w0 ?x ?t))
           (exists (?s)
                (and (PARTICULAR ?s) (PRE ?w0 ?s ?x ?t)))))
").
% ==============================================
% ==============================================
:-load_kif(";;; T17 ==
  (forall (?w0 ?x ?t ?t1)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?t)
                (PARTICULAR ?t1)
                (PRE ?w0 ?x ?t)
                (P ?w0 ?t1 ?t))
           (PRE ?w0 ?x ?t1)))
").
% ==============================================
% ==============================================
:-load_kif(";;; T18 ==
  (forall (?w0 ?x ?s ?t)
       (=> (and (WORLD ?w0)
                (PARTICULAR ?x)
                (PARTICULAR ?s)
                (PARTICULAR ?t)
                (PRE ?w0 ?s ?x ?t))
           (PRE ?w0 ?x ?t)))
").
% ==============================================
% ==============================================
