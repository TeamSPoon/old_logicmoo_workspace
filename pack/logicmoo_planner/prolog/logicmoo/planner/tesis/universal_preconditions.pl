:- module(universal_preconditions,_,[dcg]).

:- use_module(library(lists)).

:- data obj/1.

% This module translates PDDL universal preconditions to 
% conjuntion of positive literals.

% Syntax

% In: (forall (?b) (on ?b ?z)
% Out: on(object1 z?), on(object2 z?)


%Input: [(,?,b,),(,at,?,b,)] Objects = ['trabajo', 'casa']

% up(['(','?','b',')','(','at','?','b',')'],O). con 1 objecto = ok
% con 2+ objectos = OK.


%up(['(','?','b',')','(','at','?','c',')'],O) retorna at(c). 

% up(['(','?','b',')','(','at','?','b',')','(','clear','?','b',')' ],O).


% Si b <> c replica el mismo predicado

% up(['(','?','b',')','(','at','?','c',')'],O) retorna at(c) y at(c) si ha
% mas de un objeto.

 
up(Input,Output):-

	asserta_fact(obj([table])),

	universal_pre(Output,Input,[]).


universal_pre(Conj_Clauses) --> 

	['('],

	list_parameters(Parameter_UP),!,

	[')'],

	{retract_fact(obj(Objects))},

	% Predicates

	predicates_UP(Conj_Clauses, Parameter_UP,Objects).



predicates_UP(_,_,[]) --> [].


predicates_UP([],_,_) --> [].


predicates_UP(Conj_Clauses, Parameter_UP, Objects) --> 

	['('], 

	name(Name_Predicate_UP),!,

	list_parameters(Param_Predicate_UP),

	[')'],

	{instanciate(Instances_UP, Parameter_UP, Name_Predicate_UP, Param_Predicate_UP, Objects)},!,

	predicates_UP(Predicates_UP_more,Parameter_UP,Objects),!,

	{append(Instances_UP, Predicates_UP_more,Conj_Clauses)}.




instanciate(Instances_UP, Parameter_UP, Name_Predicate_UP, Param_Predicates_UP, [Object|Objects_tail]):-

	instanciate_pred(Instance, Parameter_UP, Param_Predicates_UP, Object),

	Name_P =..Name_Predicate_UP,

	InstanceP =..[Name_P|Instance],

	instanciate(Instances_UP_more, Parameter_UP, Name_Predicate_UP, Param_Predicates_UP, Objects_tail),

	append(Instances_UP_more,[InstanceP],Instances_UP).



instanciate([],_,_,_,[]).




instanciate_pred(Instance, Parameter_UP, [Param_UP|Param_UP_tail], Object):-

	Parameter_UP == [Param_UP],

	append([Object], Param_UP_tail, Instance).


instanciate_pred(Instance, Parameter_UP, [Param_UP|Param_UP_tail], Object):-

	Parameter_UP \== [Param_UP],

	instanciate_pred(Instance_more,Parameter_UP, Param_UP_tail,Object),

	append([Param_UP],Instance_more,Instance).


instanciate_pred([],_,[],_).

	


	
%Paramaters
list_parameters(A_Parameters) --> 

	((['?'], 

	\+ keywords(Name_Parameter),

	name(Name_Parameter)) |

        (\+ keywords(Name_Parameter),

	name(Name_Parameter))),

	list_parameters(M_Parameters),!, 

	{append(Name_Parameter,M_Parameters,A_Parameters)}.


list_parameters([]) --> [].


keywords(_Key) --> ['define'] | 
	           ['domain'] |
                   ['requirements'] |
                   ['predicates'] |
                   ['constants'] |
                   ['action'] | 
                   ['parameters'] | 
                   ['precondition'] | 
                   ['effect'] | 
                   ['and'] | 
                   ['not'] |
                   ['='] | ['('] | [')'].


%NAMES see parenthesis and numbers
name([C]) --> [C].
name([C|D]) --> [C],name(D),{((C >= 65,C =< 90) ; (C >= 97 ,C =< 122))}.

