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

type SEM = {you,event,chau,hola}
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
type PHON = {e,COPULA}
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

semantics binaryRel nounSem PROSem binaryState basicProperty unaryRel

%============================
% Tree Fragments
%============================

%-------
%Verbs
%-------

class  VerbalTop
import
	anchorProj2[]
	VerbSem[]
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
        };
        V = xAnchor;
		V = xVerbSem
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
        node ?V[top=[phon=COPULA]]
        }
} 

% Conjunction
class  betax1CONJx2
export ?Xr ?X1 ?X2 ?Conj
declare ?Xr ?X1 ?X2 ?Conj ?Aux ?Aux1 ?Aux2
{<syn>{ 
        node ?Xr(color=black){
             node ?X1(color=black,mark=foot)
             node ?Conj(color=black,mark=anchor)[cat=conj]
             node ?X2(color=black,mark=subst)}
        };
        propertySem[];
%		Aux = anchorProj1[]; Conj = Aux.xAnchor;
        Aux1 = footProj1[]; X1 = Aux1.xSemFoot; 
        Aux2 = footProj1[]; X2 = Aux2.xSemFoot
}

class  betan1CONJn2
import betax1CONJx2[]
declare ?fC ?fWH ?fDef
{<syn>{
        node ?Xr[cat=np,bot=[case=?fC,wh=?fWH,definite=?fDef]];
            node ?X1[cat=np,top=[case=?fC,wh=?fWH,definite=?fDef]];
            node ?X2[cat=np,top=[case=?fC,wh=?fWH,definite=?fDef]]
         }; ?fC = @{nom,acc}
}

class  betas1CONJs2
import betax1CONJx2[]
declare ?fM ?fWH
{<syn>{ 
        node ?Xr[cat=s,bot=[mode=?fM,wh=?fWH]];
             node ?X1[cat=s,top=[mode=?fM,wh=?fWH]];
             node ?X2 [cat=s,top=[mode=?fM,wh=?fWH]]
        }; ?fM = @{ind,inf,ger,imp}
}

class  betaa1CONJa2
import betax1CONJx2[]
declare ?fC ?fWH ?fDef ?Aux1 ?Aux2 ?X1 ?X2
{<syn>{
        node ?Xr[cat=ap,bot=[case=?fC,wh=?fWH]];
            node ?X1[cat=ap,top=[case=?fC,wh=?fWH]];
            node ?X2[cat=ap,top=[case=?fC,wh=?fWH]]
         }
}

class adjective 
declare ?AP ?Aux ?fWH ?A
{<syn>{ 
        node ?AP (color=black) [cat=ap,bot=[wh=?fWH]] { 
          node ?A (color=black,mark=anchor)[cat=a,top=[wh=?fWH]] 
                                                         } 
     };
	propertySem[];
	Aux = anchorProj1[]; A = Aux.xAnchor
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

class  betaxPn
import PrepCaseAssigner[]
	   AdjunctSem[]
	   AdjunctArg2Linking[]
	   PrepSem[]
	   VerbArg1Linking[]
export ?xroot ?xfoot ?xprepph ?xprep ?xnoun 
declare ?xroot ?xfoot ?xprepph ?xprep ?xnoun ?A ?B ?C
{<syn>{ 
        node xroot(color=black) {
                node xfoot(color=black,mark=foot)
                node xprepph(color=black)[cat=pp]{
                        node xprep (color=red,mark=anchor)[cat=prep]
                        node xnoun (color=black,mark=subst)[cat=np]
                                                }
                                } 
        };xprepph = ?PP ; xprep = ?P ; xnoun = ?NP;
        binaryState[]; xAdjunct = xnoun; xPrep = xprep;
	    A = footProj1[]; xfoot = A.xSemFoot ;
        B = anchorProj1[]; xprepph = B.xAnchor;
        C = footProj1[]; xprep = C.xSemFoot
}

class  betavPn
import betaxPn[]
declare ?fP ?fN ?fM ?fTense ?fC ?fAC ?fPass
{ <syn>{ 
         node xroot[cat=vp,bot=[pers=?fP,num=?fN,mode=?fM,tense=?fTense,assign-case=?fC,assign-comp=?fAC,pass=?fPass]]{
                node xfoot[cat=vp,top=[pers=?fP,num=?fN,mode=?fM,tense=?fTense,assign-case=?fC,assign-comp=?fAC,pass=?fPass]]
                node xprepph
                                                        }
        }; xprepph = ?PP ; xprep = ?P ; xnoun = ?NP     
}
 
class  betanPn
import betaxPn[]
declare ?fP ?fN ?fG ?fC ?fAC ?fDef ?fWH
{<syn>{ 
        node xroot[cat=np,bot=[pers=?fP,num=?fN,case=?fC,assign-comp=?fAC,wh=?fWH,definite=?fDef]]{
                node xfoot[cat=np,top=[pers=?fP,num=?fN,case=?fC,assign-comp=?fAC,wh=?fWH,definite=?fDef],
                                  bot=[case=@{nom,acc}]]
                node xprepph(color=red)
                                                                }
        }; xprepph = ?PP ; xprep = ?P ; xnoun = ?NP     
}


class  PPAnchorArgument
import PPAnchorTop[]
       PrepCaseAssigner[]
export ?xNP
declare ?xNP ?A ?B
{<syn> {
        node ?xNP (mark=subst,color=black)[cat=np];
        ?xprep >> ?xNP ; 
        ?compl = ?PP ; ?xprep = ?P ; ?xNP = ?NP
        };
        A = anchorProj1[]; ?NP = A.xAnchor;
        B = anchorProj1[]; ?PP = B.xAnchor
}

class  PPAnchorArgumentNPCan
import PPAnchorArgument[]
{<syn> {
        node ?xprep (mark=anchor)
       }
}

class  PPAnchorTop
import CanComplement[]
export ?xprep
declare ?xprep
{<syn> {
        node ?compl(color=black) [cat=pp]{
             node ?xprep(color=black)[cat=prep]
                                        }
        }
} 

class  PrepCaseAssigner
export ?PP ?P  ?NP
declare ?PP ?P ?fX ?NP  ?fY
{<syn> {
        node ?PP [bot=[assign-case=?fX, wh = ?fY]]{
            node ?P [top=[assign-case=?fX]]
            node ?NP[top=[case=?fX, wh = ?fY]]
                                                }
        }; ?fX = acc
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

class PPAnchorArgumentNP
import ObjectSem[]
declare ?A
{
        A= PPAnchorArgumentNPCan[]; xObject = A.xNP
}

%==================================
% Diathesis
%==================================

class  Dian0V
{
        dian0Vactive[] 
}


class  Dian0Pn1
import
	SubjectArg1Linking[]	
	ObjectArg2Linking[]
{
        RealisedSubject[] ; verbless[] ; PPAnchorArgumentNP[]
}

class  Dian0A1
import 
	SubjectArg0Linking[]
{
        RealisedSubject[] ; verbless[] ; AdjComplementAnchor[] 
} 

class  dian0Vactive
import 
	SubjectArg1Linking[]
{
        active[] ; Subject[]  
}

class  dian0Vn1active
import 
	SubjectArg1Linking[] %LB 
	ObjectArg2Linking[] %LB
{
        active[] ; Subject[] ; Object[]
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
% Verb Family: n0V
%====================================
 
 class  n0V
 import
{
	    unaryRel[];
        Dian0V[]
}

 
%====================================
% Verb Family: n0A1
%====================================
 
% The apple is red
class  n0A1
{
        Dian0A1[] 
}

%====================================
% Verb Family: n0Pn1
%====================================

% The apple is in the chest
% You are in the couch
class  n0Pn1
{
		binaryState[];
        Dian0Pn1[] 
}



%=======================================
% Semantics 
%=======================================

class VerbSem
      export xVerbSem
      declare ?xVerbSem ?I ?L
      { <syn>{ node xVerbSem[top=[idx=I,label = L]]}*=[vbI = I, topL = L,label0=L]}

class SubjectSem
      import ArgumentNodeSemantics[]
      export xSubject
      declare ?xSubject
      { <syn>{ node xSubject}*=[subjectI = I, subjectL = L]; 
      	       	    xSubject = x}

class AdjunctSem
      export xAdjunct
      declare ?xAdjunct ?I ?L
      { <syn>{ node xAdjunct[top=[idx=I,label = L]]}*=[adjunctI = I, adjunctL = L]}    	    

class PrepSem
      export xPrep
      declare ?xPrep ?I ?L
      { <syn>{ node xPrep[top=[idx=I,label = L]]}*=[prepI = I, prepL = L]}      	    

% LB 21-01-2009
% The class PROSem is a strange mixture between syntax and semantics
% I'm not sure this is the correct way to generate the implicit 
% subjects of imperatives as predicates in the semantics. 
% It results in L:you(X) where A:agent(E X) is the case
% It works for my purposes, might be worth discussing with Claire
class PROSem
      export xSubject
      declare ?xSubject ?I ?L
      {<syn>{ node xSubject[top=[idx=I,label=L]]}*=[subjectI = I];
 	   <sem>{
		L:you(I) 
	      }
	      *=[label1 = L, arg1=I]
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

class AdjunctArg2Linking[]
export ?Sr
declare ?I ?Sr ?L
{	<syn>{
                node Sr
	}*=[adjunctI = I,arg2 = I]
}

class VerbArg1Linking[]
export ?Sr
declare ?Sr ?I ?L
{	<syn>{
                node Sr
	}*=[prepI= I, arg1=I,prepL = L, label0 = L]
}

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

class unaryRel
declare ?L0 ?Rel ?E ?X ?Theta1  ?LA1 ?TopL
      {
	<sem>{
		L0:Rel(event,E); L0:Theta1(E,X) %;TopL:and(L0,LA1)
	      }
	      *=[label0 = L0, rel =Rel,arg0=E,arg1=X,theta1 =Theta1 %,arg1L=LA1,topL=TopL
	      ] 
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
                					  cat=@{det,a,conj},
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

class anchorProj2[]
import anchorProj1[]
export xNbar
declare ?xNbar ?X ?Y ?LX ?LY
{        <syn>{
                node xNbar(color=white)[bot=[idx = X, label = LX],top=[idx = Y, label = LY]]
                        { node xN(color=white) [top=[idx = X, label = LX]]}
        }
}

class footProj1[]
export xP1 xSemFoot
declare ?xP1 ?xSemFoot ?X ?Y ?LX ?LY
{        <syn>{
                node xP1(color=white) [bot=[idx = X, label = LX],top=[idx = Y, label = LY]]
                        { node xSemFoot [top=[idx = X, label = LX]]}
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
value betavPn
value n0A1
value n0Pn1
value n0V
value betan1CONJn2
value betas1CONJs2
value betaa1CONJa2
value adjective