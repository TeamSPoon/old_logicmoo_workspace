%============================
% Header
%============================

% Grammar for parsing and constructing the semantics of "take a key"

use unicity with (gf = subj) dims(syn)
use unicity with (gf = obj) dims(syn)
use color with () dims(syn)
use rank with () dims (syn)
use unicity with (rank=1) dims (syn)
use unicity with (rank=2) dims (syn)
use unicity with (rank=3) dims (syn)
use unicity with (rank=4) dims (syn)
use unicity with (rank=5) dims (syn)
use unicity with (rank=6) dims (syn)
use unicity with (rank=7) dims (syn)
use unicity with (rank=8) dims (syn)

type SEM = {you,event}
type MARK = {subst,anchor,coanchor,nadj,foot,lex,flex}
type NAME = {SubjNode,IObjNode,Anchor,SentCompl,ComplNode,Prep,Particle,Noun,CleftIt,Conjunction,Anchor,
     	    Anchor2,NCoanchor,Adjective,Determiner}
type CAT = {s,np,vp,v,n,prep,pp,pl,ap,a,adv,advp,by,c,to,det,of,Pro,for,poss}
type GF = {subj,obj,compl,iobj,ppiobj}
type PERSON = [1..3]
type NUMBER = {sing,plur}
type MODE={ind,inf,sbjnct,ger,base,ppart,nom,prep,imp}
type RMODE={ind,inf,sbjnct,ger,base,ppart,nom,prep,imp}
type TRACE = {}
type CONTROL = {}
type PHON = {e}
type CASE = {nom,acc,gen,none}
type COLOR = {red,white,black}
type RANK  = [1..8]
type FUNC={subj,obj,iobj,gen,loc,obl,byagent}
type TRUTH = {yes,no}
type TENSE = {pres,past}
type AUX = {do,have}
type PUNCT-STRUCT = {comma,dash,colon,scolon,nil}
type ASSIGN-CASE = {nom,acc,none}
type ASSIGN-COMP = {that,whether,if,for,ecm,inf_nil,ind_nil,ppart_nil,none}
type AGR = {agr-3rdsing,agr-pers,agr-num,agr-gen}	
type GENDER = {masc,fem,neut}
type NOCOMP-MODE = {inf,ger, ppart}
type COMP = {that,if,whether,for,inf_nil,ind_nil,nil}
type MAINV = {base,ger,ind,inf,imp,nom,ppart,prep,sbjunt}
type CONJ = {and,or,but,comma,scolon,to,disc,nil}
type PUNCT-STRUCT = {comma,dash,colon,scolon,nil}

type ATOMIC=[
	num : NUMBER,
 	pers : PERSON,
 	refl : bool,      
 	loc : bool,
 	control-num : NUMBER,
	control-pers : PERSON,
	compar : bool,
	equiv : bool,
	pass : bool,
	progress : bool,
	perf : bool,
	tense: TENSE,
	aux: AUX,
	det : bool,
	gerund : bool,
	loc : bool,
	inv : bool,
	extracted : bool,
	agr-3rdsing : bool,
	rel-clause : bool,
	agr-num : NUMBER,
	agr-pers : PERSON,
	agr-gen : GENDER,
	const : bool,
	quan : bool,
	card : bool,
	decrease : bool,
	pron : bool,
	refl : bool,
	definite : bool
]


property rank : RANK
property name : NAME
property mark : MARK
property gf : GF
property color : COLOR

feature phon : PHON
feature cat : CAT
feature trace : TRACE
feature case : CASE
feature wh : TRUTH
feature mode : MODE
feature rmode : RMODE
feature punct-struct : PUNCT-STRUCT
feature asign-case : ASSIGN-CASE
feature assign-comp : ASSIGN-COMP
feature agr : AGR
feature nocomp-mode : NOCOMP-MODE
feature comp : COMP
feature conj : CONJ
feature mainv : MAINV
feature top : ATOMIC
feature bot : ATOMIC
feature control : CONTROL
feature punct-struct : PUNCT-STRUCT

% %%%%%%%%%%%%%%%%%
% % Here the semantic classes used by the macro maker for Geni must be declared
% % as well as the semantic predicates used in the semantic representation language
% %%

extern rel theta1 theta2 

semantics binaryRel nounSem PROSem binaryState basicProperty

%============================
% Tree Fragments
%============================

%-------
%Verbs
%-------

class  VerbalTop
export ?Sr ?VP ?V  
declare ?Sr ?VP ?V  ?fP ?fN ?fAC ?fACP ?fM ?fP1 ?fN1 ?fAC1 ?fACP1 ?fM1 ?fPass ?fTense ?fTense1 ?fPass1 ?fMV1
{<syn>	{ 
	%node ?Sr(color=black)[cat=@{s,np},bot=[inv= -, comp = nil, extracted = -,pass=?fPass,
	node ?Sr(color=black)[cat=@{s,np},bot=[inv= -, extracted = -,pass=?fPass,
								   %pers=?fP,num=?fN,mode=?fM,tense=?fTense,
	     			               pers=?fP,num=?fN,mode=?fM1,tense=?fTense,
					       assign-case=?fAC,assign-comp=?fACP]]
				{
				node ?VP (color=black)[cat=@{vp,n},
				    %top=[pass=?fPass,pers=?fP,num=?fN,mode=?fM,tense=?fTense,assign-case=?fAC,assign-comp=?fACP],
		     		top=[pass=?fPass,pers=?fP,num=?fN,mode=@{ind,imp},tense=?fTense,assign-case=?fAC,assign-comp=?fACP],
		     		bot=[mainv=?fMV1,pers=?fP1,num=?fN1,mode=?fM1,tense=?fTense1,pass=?fPass1,assign-case=?fAC1,
				assign-comp=?fACP1,compar = -]]{
            	    						node ?V(color=black)[cat=v,
		     						top=[mainv=?fMV1,pers=?fP1,num=?fN1,mode=?fM1,
								tense=?fTense1,pass=?fPass1,assign-case=?fAC1,
		     						%assign-comp=?fACP1,punct-struct=nil]]
		     						assign-comp=?fACP1]]
								}
				}
        }
}

class  VerbalNonInverted
import VerbalTop[]
{<syn> { 
       	 node ?V(mark=anchor)
        }
}

class  active
import VerbalNonInverted[]
{<syn> {
	node ?V[top=[pass = - ]]
	}
}

class  verbless
import VerbalTop[]
declare ?A
{<syn> { 
        node ?V[top=[phon=e,mode=nom]]
        }
} 


%----------
% Features
%----------

class  SubjectAgreement
export ?Subj ?xagr ?S ?fAC
declare ?Subj ?xagr ?S ?fP ?fN ?fP1 ?fN1 ?fAC ?fAC1 ?fWH1 
{<syn>{ 
	node ?S[bot=[assign-case=?fAC,pers=?fP,num=?fN,wh=?fWH1]];
	     node ?Subj (gf=subj)[top =[case=?fAC,pers=?fP,num=?fN,wh =?fWH1]];
	     node ?xagr [top =[assign-case=?fAC,pers=?fP,num=?fN],bot=[assign-case=?fAC1,pers=?fP1,num=?fN1]]			
	}
}

class  VerbCaseAssigner
export ?verbph ?verb ?NP2 ?NP1 
declare ?verbph ?verb ?NP2 ?NP1 ?fAC ?fAC1
{<syn> {
	node ?verbph [bot=[assign-case=?fAC1]]{
	    node ?verb[top=[assign-case=?fAC1]]
	    ,,,node ?NP2[top=[case=acc]]
	    ,,,node ?NP1[top=[case=acc]]
					}
	}
}

%-------------------
%PredicateArguments
%-------------------

class  VerbalArgument
export ?XP ?VP
declare ?XP ?VP     
{<syn> {
        node ?XP(color=white)[cat = @{s,np,n}]{
            node ?VP(color=white)[cat = @{vp,a,n}]
                                        }               
        }
}

class  CanXAnchor
import CanComplement[]
export ?xanch  ?fWH
declare ?xanch  ?fWH 
{<syn> { 
        node ?compl(color=black)[bot=[wh=?fWH]]{
             node ?xanch(color=black,mark=anchor)[top=[wh=?fWH]]
                                                };   
        ?V >> ?compl 
        }
}

class  CanAdjAnchor
import CanXAnchor[]
declare ?A ?fCmp ?B
{<syn>{ 
        node ?compl [cat=ap,bot=[compar=?fCmp]]{
                  node ?xanch(rank=2)[cat=a,top=[compar=?fCmp]]
                                                }
        }; 
        propertySem[];
	A = anchorProj1[]; xanch = A.xAnchor;
	B = anchorProj1[]; compl = B.xAnchor
}


%-------------------------------------------------

class  SubjectArgumentTop
import VerbalArgument[]
       SubjectAgreement[]
export  ?fCtrl ?fPrg ?fPerf ?fMV
declare ?fCtrl ?fPrg ?fPerf ?fMV
{<syn> {
	node ?XP[cat=s,bot=[control=?fCtrl,progress= ?fPrg,perf=?fPerf,mainv=?fMV]]{
	    node ?Subj [top=[control=?fCtrl]]
	    node ?VP [cat=vp,top=[progress= ?fPrg,perf=?fPerf,mainv=?fMV]]
	    	      }
	}; ?XP = ?S ; ?VP = ?xagr	 	 
}

class  SubjectArgument
import SubjectArgumentTop[]
{<syn> { 
       	 node ?Subj(color=red)[top=[wh = -]]
       }
}

class  ImperativeSubject
import SubjectArgument[] 
declare ?X
{<syn> { 
       	 node ?XP [top=[inv = -], bot=[mode=imp]]{
	    node ?Subj(mark=nadj)[cat=np,top=[wh = -,pers=2,num=@{sing,plur}]]{
	 	node ?X(color=red,mark=flex)[phon=e]}
	    node ?VP[top=[tense=pres],top=[mode=imp]]}
	} ; ?fAC = nom
}

class  CanonicalSubject
import SubjectArgument[]
{<syn> { 
       	 node ?Subj(mark=subst)[cat=np]
	}; ?fAC = nom   
}


% %-------------------------------------------------

class  CanComplement
import VerbalArgument[]
export ?V ?compl
declare ?V ?compl
{<syn> {
  	node ?VP[cat=vp]{
                node ?V(color=white)[cat=v]
                ,,,node ?compl[top=[wh = -]]
                }
        }
}

class  CanObject
import CanComplement[]
       VerbCaseAssigner[] 
{<syn> {
	node ?compl(color=red,mark=subst,gf=obj,rank=2)[cat=np]
        }; ?VP = ?verbph ; ?V = ?verb ; ?compl = ?NP1
}


%-----------------
%Lexical classes:
%-----------------

class  noun 
import anchorProj1[]
declare ?NP ?N ?fWH ?fDef ?fC ?fN ?fP ?fG 
{<syn>{node ?NP (color=black)  
		 [cat=np,
		 bot=[definite= ?fDef,wh=?fWH,pers=?fP,num=?fN,case=?fC]] 
		{node ?N (color=black,mark=anchor)
			 [cat=n,top=[wh=?fWH,definite=?fDef,pers=?fP,num=?fN]]
		}
	};
	nounSem[]; N = xAnchor %LB
}

class  betaAn
import betaXn[]
declare ?A ?B
{<syn>{
	node ?modifier(color=black,mark=anchor)[cat=a]
	};
	propertySem[];
	A = anchorProj1[]; modifier = A.xAnchor;
	B = anchorProj1[]; Nf = B.xAnchor	
}

class  betaXn
export ?Nr ?modifier ?Nf ?fC ?fP ?fN ?fWH ?fAC ?fDef
declare ?Nr ?modifier ?Nf ?fC ?fP ?fN ?fWH ?fAC ?fDef
{<syn>{
	node ?Nr(color=black)[cat=n,bot=[case=?fC,pers=?fP,num=?fN,wh=?fWH,assign-comp=?fAC,definite=?fDef]]{
	    node ?modifier(color=black,mark=anchor)[top=[wh=?fWH]]
	    node ?Nf(color=black,mark=foot)[cat=n,top=[case=?fC,pers=?fP,num=?fN,assign-comp=?fAC,definite=?fDef]]
												}
	}
}

class  betaDn
export ?NPr ?NPf ?Det 
declare ?NPr ?NPf ?Det ?fDef ?fWH ?fC ?fP ?fN ?fDef1 ?fWH1 ?L ?I ?A ?B
{<syn>{node ?NPr(color=black)
		[cat=np,
		 bot=[definite=?fDef,wh=?fWH,case=?fC,pers=?fP,num=?fN]]
		{node ?Det(color=black,mark=anchor)
			  [cat=det,	
			   top=[definite=?fDef,wh=?fWH,num=?fN],
			   bot=[definite=?fDef1,wh=?fWH1]]
      	     	 node ?NPf(color=black,mark=foot)
			  [cat=np,top=[rel-clause = -,case=?fC,pers=?fP,num=?fN]]
		}
       };
	propertySem[]; 
	A = anchorProj1[]; Det = A.xAnchor;
	B = anchorProj1[]; NPf = B.xAnchor
}

class betaVv
declare ?VPr ?VPf ?V ?fC ?fM ?fTense ?fP ?fN ?fAC ?fRM ?fPass ?fPass1 ?A
{<syn>{
        node ?VPr(color=black)[cat=vp,bot=[assign-case=?fC,mode=ind,tense=?fTense,pers=?fP,num=?fN,assign-comp=?fAC]]{
             node ?V(color=black,mark=anchor)[cat=v,
                        top=[assign-case=?fC,mode=ind,tense=?fTense,pers=?fP,num=?fN,assign-comp=?fAC],bot=[rmode=?fRM]]
             node ?VPf(color=black,mark=foot)[cat=vp,bot=[mode=nom]]
                                                                        }
        };
        A = anchorProj1[]; VPf = A.xAnchor
}


%==================================
% Syntactic Functions
%==================================

class Subject[]
{     RealisedSubject[]
      | NonRealisedSubject[]
}

class RealisedSubject[]
import SubjectSem[]
declare ?A
{ <syn>{ 
	A=CanonicalSubject[]
%	|A=CliticSubject[]
%	|A=whSubject[]
%	|A=RelativeSubject[]
% 	|A=CleftSubject[]
% 	|A=InvertedNominalSubject[]
	};
	xSubject = A.Subj %LB
}
	
class NonRealisedSubject[]
import PROSem[]
declare ?A
{ <syn>{ 
%  	A=InfinitiveSubject[]; xSubject = A.xS
%   |
   	A=ImperativeSubject[]; xSubject = A.Subj %LB
%  	|A=InterrogInvSubject[]; xSubject = A.xVN 
	}
}	

class  Object
import ObjectSem[] %LB 
declare ?A %LB
{<syn>{
%   CanObject[]
	A=CanObject[]; xObject = A.NP1 %LB
% 	| WhObject[] 
% 	| RelativeOvertObject[] 
% 	| RelativeCovertObject[] 
% 	| DeterminerGerundObject[]
}
}

class AdjComplementAnchor
{
        CanAdjAnchor[] 
%        | WhAnchorAdjective[]
}


%==================================
% Diathesis
%==================================

class  Dian0A1
import 
	SubjectArg0Linking[]
{
        Subject[] ; verbless[] ; AdjComplementAnchor[] 
} 

class  dian0Vn1active
import 
	SubjectArg1Linking[] %LB 
	ObjectArg2Linking[] %LB
{
        active[] ; Subject[] ; Object[]
%	| betaVn[]*=[vmode = ppart] 
%	| alphaAV[]
}

%====================================
% Verb Family: n0Vn1
%====================================
 
 class  Dian0Vn1
 {
         dian0Vn1active[] 
%          | dian0Vn1shortpassive[] 
%          | dian0Vn1passive[] 
 }

%  class  Reln0Vn1
%  {
%          Dian0Vn1[] ; RelativeAdjunct[]
%  }

 class  n0Vn1rel
 {
	 binaryRel[]; %LB
         Dian0Vn1[] %| Reln0Vn1[] 
 }
 
  class  n0Vn1state
 {
	 binaryState[]; %LB
         Dian0Vn1[] %| Reln0Vn1[] 
 }
 
%====================================
% Verb Family: n0A1
%====================================
 
% The apple is red
class  n0A1
{
        Dian0A1[] 
}



%=======================================
% Semantics 
%=======================================

class SubjectSem
      import ArgumentNodeSemantics[]
      export xSubject
      declare ?xSubject
      { <syn>{ node xSubject}*=[subjectI = I, subjectL = L]; 
      	       	    xSubject = x}

% LB 21-01-2009
% The class PROSem is a strange mixture between syntax and semantics
% I'm not sure this is the correct way to generate the implicit 
% subjects of imperatives as predicates in the semantics. 
% It results in L:you(X) where A:agent(E X) is the case
% It works for my purposes, might be worth discussing with Claire
class PROSem
      export xSubject
      declare ?xSubject ?I ?E ?L1
      {<syn>{ node xSubject[top=[idx=you]]}*=[subjectI = I];
 	   <sem>{
		L1:you(E) 
	      }
	      *=[label1 = L1, arg1=E]
}

class ArgumentNodeSemantics
      export x I L
      declare ?x ?I ?L ?S
      { <syn>{ node x[top=[idx=I,label = L,scopeL=S]]}*=[label0=S]}

class ObjectSem
      import ArgumentNodeSemantics[]
      export xObject
      declare ?xObject
      { <syn>{ node xObject}*=[objectI = I, objectL = L]; 
      	       	    xObject = x}

% Syntax/Semantic linking

class TopLevelClass
export ?Sr
declare ?Sr
{	<syn>{node Sr}}

class SubjectArg1Linking[]
import 
	TopLevelClass[]
declare ?I  ?E ?L
{	<syn>{
                node Sr
	}*=[vbI = E,arg0 = E,subjectI = I,arg1 = I,subjectL=L,arg1L=L]
}

class ObjectArg2Linking[]
import 
	TopLevelClass[]
declare ?I ?L  
{	<syn>{
                node Sr
	}*=[objectI = I,arg2 = I,objectL=L,arg2L=L]
}

class SubjectArg0Linking[]
import 
	TopLevelClass[]
declare ?I  ?L
{	<syn>{
                node Sr
	}*=[subjectI = I,arg0 = I,subjectL=L,label0=L]
}

% -------------------------------------------------------
% Semantic schemas
% -------------------------------------------------------

%% Semantic classes

class basicProperty
export E L0 Rel
declare ?Rel ?E ?L0
      {
	<sem>{
		L0:Rel(E) 
	      }
	      *=[label0 = L0, rel =Rel,arg0=E]
	  }

class binaryRel
declare  ?L0 ?Rel ?E ?X ?Theta1 ?Y ?Theta2 ?LA1 ?TopL ?LA2
      {
	<sem>{
		 L0:Rel(event,E);L0:Theta1(E,X);L0:Theta2(E,Y) %;TopL:and(L0,LA1,LA2)
	      }
	      *=[label0 = L0, rel =Rel,arg0=E,arg1=X,theta1 =Theta1,arg2=Y,theta2 =Theta2 %,arg1L=LA1,arg2L=LA2,topL=TopL
		] 
	  }
	  
class binaryState
export L0 Rel X Y
declare ?Rel ?X ?L0  ?Y 
      {
	<sem>{
		L0:Rel(X,Y)
	      }
	      *=[label0 = L0, rel =Rel,arg1=X,arg2=Y]
	  }


class nounSem
declare
        ?N ?I ?L
{
	<syn>{
                node N(color=white)[cat=@{n},top=[idx=I,label = L],bot=[idx=I,label = L]]
	};
	basicProperty[]*=[arg0=I,label0 = L]
}

%LB Class added to define the semantics of basic properties. 
class propertySem
declare
        ?N ?I ?L
{
	<syn>{
                node N(color=white)[
                					  cat=@{det,a},
                					  top=[idx=I,label = L],bot=[idx=I,label = L]
                					 ]
	};
	basicProperty[]*=[arg0=I,label0 = L]
}

class anchorProj1[]
export xN xAnchor
declare ?xN ?xAnchor ?X ?Y ?LX ?LY
{        <syn>{
                node xN(color=white) [bot=[idx = X, label = LX],top=[idx = Y, label = LY]]
                        { node xAnchor [top=[idx = X, label = LX]]}
        }
}


%=======================================
% Valuations
%=======================================

value n0Vn1rel
value n0Vn1state 
value noun
value betaDn
value betaAn
value betaVv
value n0A1