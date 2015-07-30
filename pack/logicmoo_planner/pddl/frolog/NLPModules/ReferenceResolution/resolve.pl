/*************************************************************************

    File: resolve.pl
    Copyright (C) 2007 

    Programmer: Alejandra Lorenzo

    This file is part of Frolog, version 0.1 (October 2007).

    Frolog is free software; you can redistribute it and-or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frolog is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frolog; if not, write to the Free Software Foundation, Inc., 
    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

*************************************************************************/

:- module(resolve,[resolveResolutionMain/4]).  
:- use_module('KBInterfaces/RacerInterface/racer',[concept_instances/4]).
:- use_module('Tools/handleExceptions',[handle/2,handle/3]).

resolveResolutionMain(Concept,V,I,DM):- catch(resolveResolution(Concept,V,I,DM),Error,handle(resolution,Error)).

/*SArg is def(arg) or indef(arg)*/
/*This is a special case for resolving you(X)*/
resolveResolution((def(X),[you(X)]),X,[myself],DM). 

/*This is the general case*/
resolveResolution((SArg,Predicates),Arg,ListIndividuals,DM) :-
				racer:playerKB(Link, GameABoxName),
				SArg=..[Spec,Arg], /*transform the predicate or relat. into a list, so we can work with it and its args*/
				formatProperties(Predicates,Arg,Str,Predicates),
				string_concat('(and visible ', Str,StrAux0),
				string_concat(StrAux0,')',StrAux),
				concept_instances(Link,GameABoxName,StrAux,ListIndividuals)/*,
				!,
				check_def(Spec,ListIndividuals,Arg,Predicates,Individuals,DM)*/.

/*if the predicate contains the arg.Arg, we process it and then continue with rest of the list*/
formatProperties(LPs,Arg,St,[P1|R]):- P1=..L, /*transform the predicate or relat. into a list, so we can work with it and its args*/
		              member(Arg,L),
			      procPred(LPs,Arg,P1,StNew1),
		              formatProperties(LPs,Arg,StNew2,R),
			      string_concat(StNew1,StNew2,St).
			      
formatProperties(LPs,Arg,St,[P1|R]):- P1=..L, /*transform the predicate or relat. into a list, so we can work with it and its args*/
		              not( member(Arg,L)),
		              formatProperties(LPs,Arg,St,R).

formatProperties(LPs,Arg,'',[]):-!.           

/*process the current predicate, it is processed in a diferent way depending on its arity*/
procPred(LPs,Arg,P,St):- delete(LPs,P,LPs1),   /*first we eliminate the predicate from the list of predicates*/
		         P=..L,
		         (length(L,2)-> procUnary(L,St);procBinary(LPs1,Arg,L,St)).

/*To process Unary predicates*/
procUnary([ArgName|Arg1],St):- 
	           string_to_atom(Staux,ArgName),
		   string_concat(Staux,' ',St),!.

procUnary([ArgName|Arg1],'').

/*to process Binary predicates*/
procBinary(LPs,Argument,[ArgName,Arg1,Arg2],St):- 
	                     (Argument = Arg2-> procArg(LPs,Arg1,[ArgName,Arg1,Arg2],St);procArg(LPs,Arg2,[ArgName,Arg1,Arg2],St)).

/*Obtains the name of the binary predicate (f.i.  "some hasdetail"),
  call format properties to obtain the name of the unary predicate for the other argument (f.i.worm)
  concatenate everything.*/
procArg(LPs,Argument,[Rel|Args],StNew):-
	                       string_to_atom(S1,Rel),
			       string_concat('(some ',S1,S2),
	                       formatProperties(LPs,Argument,S3,LPs),
			       string_concat(S3,')',S4),
			       string_concat(S2,' ',S5),
			       string_concat(S5,S4,StNew).

%LB 27-Jan-2009
%I will implement error and DM outside Prolog

% /*check_def(+Specification,+List_of_individuals,+Argument,+Predicates,IndividualsReturned,DiscourseModel)*/
% 
% /* if the list of individuals is empty, it is because it should have obtained the individual from the 
% discourse model, but the discourse model is empty, it must report error ("I don't understand what you mean by `it'.").*/
% check_def(def,VisibleIndividuals,_,[],Ind,DM):- getIt(DM,Ind,VisibleIndividuals),
% 						length(Ind,N),
% 						N>0,
% 						!.
% 
% check_def(def,VisibleIndividuals,_,[],Ind,DM):- !, 
% 						throw(no_ref_for_pronoun(_)).
% /*if the list of individuals returned contains only nil, it is because the query to racer returned nil, so
%  (the specification is def or indef),it must report error.*/
% check_def(def,[nil],Arg,Predicates,[],_):- obtainConcept(Arg,Predicates,Concept),!,
%                                   throw(no_ref_for(Concept)). /*You can't see any Concept*/
%                                   
% check_def(indef,[nil],Arg,Predicates,[],_):- obtainConcept(Arg,Predicates,Concept),!, 
%                                   throw(no_ref_for(Concept)). /*You can't see any Concept*/
%                                   
% /*if the list contains one individual and the specification is def, return the individual*/
% check_def(def,[Ind],Arg,Predicates,[Ind],_):-  !.
% /*if the list contains more than one indiv. and the specification is indef, return the first element.*/
% check_def(indef,[Ind|Tail],Arg,Predicates,[Ind],_):-!.
% 
% /*if the list contains more than one individual, and the specification is def, 
% then it should check in the discourse model to see if the one of the las elements is there, 
% then if one of the returned elements is in the discourse model, it should get the last one.*/
% check_def(def,ListInd,Arg,Predicates,[Ind],DM):- length(ListInd,N),
% 					       N>1,
% 					       valida_discourse(ListInd,DM,Ind),
% 					       !.	
% 
% /*if the list contains more than one individual, and the specification is def, it must report error*/
% check_def(def,ListInd,Arg,Predicates,[],DM):- length(ListInd,N),
% 					N>1,
% 					obtainConcept(Arg,Predicates,Concept), !,
%                                         throw(ref_ambiguous(Concept)). /*There is more than one Concept*/
%                                         
% /*getIt(DiscourseModel,IndividualReturned,VisibleIndividuals)*/
% /*it get the top individual in the discourse model */
% getIt([],[],VisibleIndividuals):-!.
% getIt([Top|Rest],[Top],VisibleIndividuals):-member(Top,VisibleIndividuals).                                   
%                                         
% 
% /*valida_discourse(Lista-de-individuos,Discourse-Model,Individual)*/
% valida_discourse(ListInd,[],nil):- !,fail.
% valida_discourse(ListInd,[Ind|Rest],Ind):- member(Ind,ListInd),!.
% valida_discourse(ListInd,[Top|Rest],Ind):- /*not( member (Top,ListInd)),*/
% 					  valida_discourse(ListInd,Rest,Ind).
% 
% 
% /*to obtain the concep that raised an error.*/
% obtainConcept(Arg,[],Concept):-!.
% obtainConcept(Arg,[P1|R],Concept):- P1=..[Concept,Arg],!.
% obtainConcept(Arg,[P1|R],Concept):- obtainConcept(Arg,R,Concept).
% 
%                       
%  
