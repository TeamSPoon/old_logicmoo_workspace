%------------------------------------------------------------------------------
% File     : CSR075+2 : TPTP v6.0.0. Bugfixed v5.4.0.
% Domain   : Commonsense Reasoning
% Problem  : Class subsumption, skolemization
% Version  : Especial > Augmented > Especial.
% English  :

% Refs     : [NP01]  Niles & Pease (2001), Towards A Standard Upper Ontology
%          : [Sie07] Siegel (2007), Email to G. Sutcliffe
% Source   : [Sie07]
% Names    : TQG1

% Status   : Theorem
% Rating   : 0.93 v6.0.0, 0.91 v5.5.0, 0.93 v5.4.0
% Syntax   : Number of formulae    : 37710 (32454 unit)
%            Number of atoms       : 59118 (1481 equality)
%            Maximal formula depth :   26 (   2 average)
%            Number of connectives : 22312 ( 904   ~; 114   |;10143   &)
%                                         ( 139 <=>;11012  =>;   0  <=;   0 <~>)
%                                         (   0  ~|;   0  ~&)
%            Number of predicates  :    8 (   0 propositional; 1-8 arity)
%            Number of functors    : 5082 (5082 constant; 0-8 arity)
%            Number of variables   : 12890 (  12 sgn;11437   !;1453   ?)
%            Maximal term depth    :    5 (   1 average)
% SPC      : FOF_THM_RFO_SEQ

% Comments : This version includes the cache axioms.
% Bugfixes : v3.4.1 - Bugfixes in CSR003+*.ax
%          : v3.4.2 - Bugfixes in CSR003+1.ax
%          : v3.5.0 - Bugfixes in CSR003+1.ax
%          : v4.0.0 - Bugfixes in CSR003 axiom files.
%          : v4.0.1 - Bugfixes in CSR003 axiom files.
%          : v4.1.0 - Bugfixes in CSR003 axiom files.
%          : v5.3.0 - Bugfixes in CSR003 axiom files.
%          : v5.4.0 - Bugfixes in CSR003 axiom files.
%------------------------------------------------------------------------------
%----Include axioms from SUMO_MILO
include('Axioms/CSR003+1.ax').
%----Include cache axioms for SUMO+MILO
include('Axioms/CSR003+4.ax').
%------------------------------------------------------------------------------
fof(local_1,axiom,(
    s__instance(s__Org1_1,s__Organization) )).

fof(prove_from_SUMO_MILO,conjecture,(
    ? [V_MEMBER] : s__member(V_MEMBER,s__Org1_1) )).

%------------------------------------------------------------------------------
