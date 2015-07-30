:- module(pddl_domain,_,[dcg]). % the module imports the dcg library package. Also it could indicate use_package(dcg).

% Libraries and packages.

:- use_module(readFile2,[read_file/2]). % This module reads the pddl input file and it returns a list of strings.
:- use_module(read_write,_). % This module includes predicates for read and writing a file.
:- use_module(module_lists,_).
:- use_module(library(lists)).
:- use_module(library(write),[write_list1/1, numbervars/3]).
:- use_module(library(atom2term)).
:- use_module(library(iso_byte_char)).
:- use_module(library(random)).


parse_domain(Input,Objs,Preconditions,Achieves,Deletes):-
	pddl2stripsdomain(Objs,Preconditions,Achieves,Deletes,Input,[]),!. %%%%% Expansion


%parse_domain(_,_,_,_,_):- message(error, 'El dominio PDDL actual no puede ser traducido').


% Read from PDDL domain file.
%parse_domain(Input, Objs, Preconditions, Achieves, Deletes):-
%	read_file(Input, Lexemas), % Return list of lexemas from the .pddl.
%	pddl2stripsdomain(Objs,Preconditions, Achieves, Deletes, Lexemas,[]). %%%%% Files


%%% Variable Dictionary (Name,VAR)

varof(Name,Dict,Y,Dict2):-
	member(v(Name,Y),Dict),
	Dict2 = Dict.

varof(Name,Dict,Y,[v(Name,Y)|Dict]).
	

%KEYWORDS

key(['domain','define','requirements','predicates','constants',
'action','parameters','precondition','effect','and','not','=',
'(',')','forall','when','or']).

keywords(Name):-
	key(LIST),
	member(Name,LIST).


%DOMAIN  % constants are optional ones
% actions also could be optional.
pddl2stripsdomain(Objs,Preconditions, Achieves, Deletes) --> 
        ['(', 'define', '(', 'domain'], name(_Name_Domain),!, [')'], 
	((requirements_def(Reqs), {_Requirements =..[requirements|Reqs]}) | []),!, 
	((predicates_def(Dict,Preds), {_Predicates =..[predicates|Preds]}) | []),!, 
	((constants_def(Consts), {_Constants =..[constants|Consts]}) | []),!,
	(actions_def(Dict,Objs,Preconditions, Achieves, Deletes) | []),!,
        [')'].


% % % % % % % % % % % % % %REQUIREMENTS:% % % % % % % % % % % % % % % %  
%                                                                     %
% The requirements are features in which the language is factored.    %
% Define the requirements: STRIPS, equality, universal-preconditions, % 
% conditional-effects and disyuntive-preconditions.                   %
%                                                                     %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
 

requirements_def(Requirements) --> ['(',':','requirements'], list_req(Requirements),!, [')'].


list_req(Requirements) --> 
	% Basic STRIPS-style adds and deletes
	([':'], ['strips'], list_req(M_Requirements), {append(['strips'],M_Requirements,Requirements)}) 
        | 
        % Support = as built-in predicate
        ([':'], ['equality'], list_req(M_Requirements), {append(['equality'],M_Requirements,Requirements)}) 
        |
        % Allow forall in goal descriptions
        ([':'], ['universal-preconditions'], list_req(M_Requirements),{append(['universal-preconditions'],M_Requirements,Requirements)}) 
        |
        % Allow when in action effects
        ([':'], ['conditional-effects'], list_req(M_Requirements),{append(['conditional-effects'],M_Requirements,Requirements)}) 
        |
        % Allow or in goal descriptions
        ([':'], ['disjuntive-preconditions'], list_req(M_Requirements),{append(['disjuntive-preconditions'],M_Requirements,Requirements)}).
        % add + PPDL requirements here


list_req([]) --> [].



% % % % % % % % % % % % % %PREDICATES:% % % % % % % % % % % % % % % % % 
%                                                                     %
% These list contain all the predicates used in the domain definition %
% The predicates definition is (<predicates_name> <parameters>)       % 
%                                                                     %
%                                                                     %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


predicates_def(Dict,Predicates) --> 
	['(',':','predicates'], 
	list_pred(Dict,Predicates),!,
	[')'].


list_pred(Dict,Predicates) --> 
	['('], 
        name(Name_P),
	list_parameters(Dict,Param_Predicate),!,
        [')'], 
	list_pred(Dict,M_Predicates),
	{N =..Name_P,  Predicate =..[N|Param_Predicate], append([Predicate],M_Predicates,Predicates)}.


list_pred(Dict,Predicates) --> 
	['('], 
	name(Name_P),!,
	[')'],
	list_pred(Dict,M_Predicates),
	{append(Name_P,M_Predicates,Predicates)}.


list_pred(_,[]) --> [].



% % % % % % % % % % % % % %CONSTANTS :% % % % % % % % % % % % % % % % % 
%                                                                     %
%                                                                     %
%       ToDo: Allow more than only one constant                       % 
%                                                                     %
%                                                                     %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


constants_def(Constants) --> 
	['(', ':', 'constants'], 
	list_of_constants(Constants),
	[')'].


list_of_constants(Constants) -->
	name(Name_Constant),!, 
	list_of_constants(M_Constants),!,
	{append(Name_Constant,M_Constants,Constants)}.


list_of_constants([]) --> [].


% % % % % % % % % % % % % % ACTIONS:% % % % % % % % % % % % % % % % % %
%                                                                     %
% (parameters,preconditions,effects)                                  %
% - The parameters are a list of atomic formulae.                     %          
% - The preconditions are lists of predicates included in the list    %
% of predicates defined in the domain.                                %
% - The effects are lists of predicates included in the list of       %
% predicates defined in the domain. The lists of effects are divided  %
% in two lists of predicates.                                         %                         
% - The first list is named "achieves" list. It is includes           %
% the primitive relations (included in the list of predicates) no     % 
% longer true after executing the action.                             %
% - The second list is named "delete" list. It is includes the        % 
% primitive relations made true by the action.                        %
% - All the variables in the actions must appear in the list of       %
% parameter of the action.                                            % 
%                                                                     %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %

% if effect includes not(a()) then verify that precondition includes a() and add not(a()) to Deletes list.

% if effect includes a() then add a() to the Achieves list.

actions_def(Dict,Objs,Preconditions,Achieves,Deletes) --> 
	['(',':','action'], 
	name(Name),!, 
	[':', 'parameters'], 
	['('], list_parameters(Dict,Param),!, [')'],
	{nth(1, Name, Name_Ac),  Ac =..[Name_Ac|Param]},
	[':', 'precondition'], 
	['('], list_preconditions(Dict,Objs,P),{Pre =..[preconditions|[Ac,P]]},!, [')'], 
	[':', 'effect'], 
	['('], list_effects(Dict,A, D, P),{Ach =..[achieves|[Ac,A]], Del =..[deletes|[Ac,D]]},!, 
	[')'], [')'],
	actions_def(Dict,Objs,PRE2,ACH2,DEL2),!,
	{  ( (instances(P,_),append([Pre],PRE2,Preconditions),append([Ach],ACH2,Achieves),append([Del],DEL2,Deletes));
	     (generateInstances(Pre,Ach,Del,PRE1,ACH1,DEL1),append(PRE1,PRE2,Preconditions), append(ACH1,ACH2,Achieves), append(DEL1,DEL2,Deletes)) )}.


actions_def(_,_,[],[],[]) --> [].


%ACTIONS - Paramaters
list_parameters(Dict,A_Parameters) --> 
	((['?'],name(Name_Parameter)) | (name(Name_Parameter))),
	{varof(Name_Parameter,Dict,X,Dict2)},
	list_parameters(Dict2,M_Parameters),!,
        {append([X],M_Parameters,A_Parameters)}.


list_parameters(_,[]) --> [].



%ACTIONS - Preconditions
list_preconditions(Dict,_,Preconditions) --> 
	name(Name_Precondition),!,
	list_parameters(Dict,Param_Precondition),!, 
	{N =..Name_Precondition, Precondition =..[N|Param_Precondition],Preconditions = [Precondition]}.


list_preconditions(Dict,_,Preconditions) --> 
	['not'], 
	['('], 
	((name(Name_Precondition_NOT),!,
	list_parameters(Dict,Param_Precondition_NOT), [')'],!,
	{N =..Name_Precondition_NOT, P =..[N|Param_Precondition_NOT],Precondition =..['\\+'|[P]],Preconditions = [Precondition]})
	| 
        (['='], 
	list_parameters(Dict,Param_Precondition_EQ),!, 
	[')'], 
	{Precondition =..['=='|Param_Precondition_EQ],Preconditions = [Precondition]})).


list_preconditions(Dict,_,Preconditions) --> 
	['='], 
	['('], 
	list_parameters(Dict,Param_Precondition_EQ),!,
	[')'],
	{Precondition =..['=='|Param_Precondition_EQ],Preconditions = [Precondition]}.


% UNIVERSAL PRECONDITIONS

% if forall -> universal_preconditions(and_list).
% and(and_list) 

list_preconditions(Dict,Objs,Preconditions) -->
	['forall'],
	universal_pre(Dict,Objs,Preconditions),!.



% DISJUNTIVE PRECONDITIONS

% a v b

list_preconditions(Dict,Objs,Preconditions) -->
	['or'],
	disjuntive_pre(Dict,Objs,P),!,
	{PRECOND =..['or'|P], append([PRECOND],[],Preconditions)}.



list_preconditions(Dict,Objs,Preconditions) --> 
	['and'], 
	list_preconditions_AND(Dict,Objs,Preconditions),!. % Simple and precondition 


list_preconditions(_,_,[]) --> [].


list_preconditions_AND(Dict,Objs,FORALL) --> 
	list_preconditions_FORALL(Dict,Objs,FA),!,
	list_preconditions_AND(Dict,Objs,AND2),!, 
	{append(FA,AND2,FORALL)}.


list_preconditions_AND(Dict,Objs,OR) --> 
	list_preconditions_OR(Dict,Objs,DP),!,
	list_preconditions_AND(Dict,Objs,AND2),!, 
	{D =..['or'|DP], append([D],AND2,OR)}.


list_preconditions_AND(Dict,Objs,AND_Preconditions) --> 
	['('], 
	list_preconditions_AND_NOT(Dict,Objs,AND_NOT_P),!,
	[')'],
	list_preconditions_AND(Dict,Objs,M_AND_Preconditions),!, 
	{append(AND_NOT_P,M_AND_Preconditions,AND_Preconditions)}.


list_preconditions_AND(_,_,[]) --> [].


list_preconditions_FORALL(Dict,Objs,FORALL) -->
	['('],
	['forall'],
	universal_pre(Dict,Objs,FORALL),!,
	[')'].


list_preconditions_OR(Dict,Objs,OR) -->
	['('],
	['or'],
	disjuntive_pre(Dict,Objs,OR),!,
	[')'].


list_preconditions_AND_NOT(Dict,Objs,AND_NOT_Preconditions) --> 
	name(Name_Precondition_NOT),!,
	list_parameters(Dict,AND_NOT_Param),!,
	list_preconditions_AND(Dict,Objs,M_AND_NOT_Preconditions),!, 
	{N =..Name_Precondition_NOT, Precondition =..[N|AND_NOT_Param],append([Precondition],M_AND_NOT_Preconditions, AND_NOT_Preconditions)}.


list_preconditions_AND_NOT(Dict,Objs,AND_NOT_Preconditions) --> 
	['not'], 
	['('], 
	((name(Name_AND_NOT),!,
	list_parameters(Dict,Param_AND_NOT),!, [')'],
	list_preconditions_AND(Dict,Objs,AND_Preconditions),
	{N =..Name_AND_NOT, NN =..[N|Param_AND_NOT], Precondition =..['\\+'|[NN]],append([Precondition],AND_Preconditions,AND_NOT_Preconditions)})
	| 
        (['='], 
	list_parameters(Dict,Param_Precondition_EQ),!, 
	[')'],
	list_preconditions_AND(Dict,Objs,AND_Preconditions),!,
	{Precondition =..['\\=='|Param_Precondition_EQ],append([Precondition],AND_Preconditions,AND_NOT_Preconditions)})).


list_preconditions_AND_NOT(Dict,Objs,AND_NOT_Preconditions) --> 
	['='], 
	list_parameters(Dict,Param_AND_NOT),!,
	list_preconditions_AND(Dict,Objs,AND_Preconditions),!,
	{Precondition =..['=='|Param_AND_NOT], append([Precondition],AND_Preconditions, AND_NOT_Preconditions)}.


list_preconditions_AND_NOT(_,_,[]) --> [].



%ACTIONS - Effects
list_effects(Dict,Achieves, _Deletes, _Preconditions) --> 
	name(Name_Effect),!,
	list_parameters(Dict,Param_Effect),!,
	{N =..Name_Effect,Effect =..[N|[Param_Effect]],append([Effect],[],Achieves)}.


list_effects(Dict,Achieves, _Deletes, _Preconditions) --> 
	['='], 
	['('], 
	list_parameters(Dict,Param_Effect_EQ),!, 
	[')'],
	{Effect =..['=='|Param_Effect_EQ], append([Effect],[],Achieves)}.


list_effects(Dict,_Achieves, Deletes, _Preconditions) --> 
	['not'], 
	['('], 
	name(Name_Effect_NOT),!, %Negative effect
	list_parameters(Dict,Param_Effect_NOT),!, 
	[')'], 
	{N =..Name_Effect_NOT,NN =..[N|Param_Effect_NOT],Deletes = [NN]}.
	 

list_effects(Dict,Achieves, Deletes, Preconditions) --> 
	['and'], 
	list_effects_AND(Dict,Achieves, Deletes, Preconditions),!.


list_effects(_,[],[],_) --> [].


list_effects_AND(Dict,Achieves,Deletes,Preconditions) --> 
	['('], 
	list_effects_AND_NOT(Dict,Achieves_1, Deletes_1, Preconditions),!,
	[')'],
	list_effects_AND(Dict,Achieves_2, Deletes_2, Preconditions),!, 
	{append(Achieves_1, Achieves_2, Achieves),append(Deletes_1, Deletes_2, Deletes)}.


list_effects_AND(_,[],[],_) --> [].


list_effects_AND_NOT(Dict,Achieves, _Deletes, Preconditions) --> 
	name(Name_Effect_NOT),!,
	list_parameters(Dict,AND_NOT_Param),!,
	list_effects_AND(Dict,Achieves_2, _Deletes_2, Preconditions),!, 
	{N =..Name_Effect_NOT,Effect =..[N|AND_NOT_Param],append([Effect],Achieves_2, Achieves)}.


list_effects_AND_NOT(Dict,_Achieves, Deletes, Preconditions) --> 
	['not'], 
	['('], 
	((name(Name_Effect_NOT),!,
	  list_parameters(Dict,AND_NOT_Param),!,
	[')'],
	list_effects_AND(Dict,Achieves_1, Deletes_1, Preconditions),!,
	{N =..Name_Effect_NOT, NN =..[N|AND_NOT_Param], append([NN],Deletes_1,Deletes)})
	| 
        (['='], 
	list_parameters(Dict,Param_Effect_EQ),!, 
	[')'],
	list_effects_AND(Dict,Achieves_1, Deletes_1, Preconditions),!,
	{Effect =..['\\=='|Param_Effect_EQ], append([Effect],Deletes_1,Deletes)})).


list_effects_AND_NOT(Dict,Achieves, _Deletes, Preconditions) --> 
	['='], 
	list_parameters(Dict,AND_NOT_Param),!, % Compose precondition with equality.
	list_effects_AND(Dict,Achieves_1, _Deletes_1, Preconditions),!,
	{Effect =..['=='|AND_NOT_Param],append([Effect],Achieves_1, Achieves)}.


list_effects_AND_NOT(_,[],[],_) --> [].



%NAMES see parenthesis and numbers
name([C]) --> [C],!,{\+ keywords(C)}.
name([C|D]) --> [C],name(D),{((C >= 65,C =< 90) ; (C >= 97 ,C =< 122))}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Precondiciones Universales

% This module translates PDDL universal preconditions to 
% conjuntion of positive literals.

universal_pre(Dict,Objs,Conj_Clauses) --> 
	['('],
	list_parameters(Dict,Parameter_UP),!,
	[')'],
	predicates_UP(Dict,Conj_Clauses, Parameter_UP,Objs).


predicates_UP(Dict,Conj_Clauses, Parameter_UP,Objs) --> 
	['('], 
	name(Name_Predicate_UP),!,
	list_parameters(Dict,Param_Predicate_UP),
	[')'],
	{instanciate(Instances_UP, Parameter_UP, Name_Predicate_UP, Param_Predicate_UP,Objs)},!,
	predicates_UP(Dict,Predicates_UP_more,Parameter_UP,Objs),!,
	{append(Instances_UP,Predicates_UP_more,Conj_Clauses)}.


predicates_UP(_,_,_,[]) --> [].


predicates_UP(_,[],_,_) --> [].


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Disjuntive preconditions

%disjuntive_pre(Dict,_,DNF) -->
%	['('],
%	name(D),!,
%	list_parameters(Dict,DP),
%	[')'],
%	disjuntive_pre(Dict,_,DNF2),!,
%	{DN =..D, D1 =..[DN|DP], append([D1],DNF2,DNF)}.

disjuntive_pre(Dict,_,DNF) -->
	['('],
	list_preconditions(Dict,_,D),
	[')'],
	disjuntive_pre(Dict,_,DNF2),!,
	{append(D,DNF2,DNF)}.


%disjuntive_pre(Dict,_,DNF) -->
%	list_preconditions(Dict,_,D),
%	disjuntive_pre(Dict,_,DNF2),!,
%	{append(D,DNF2,DNF)}.


disjuntive_pre(_,_,[]) --> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


instances([PRED|TD],_):-   % Es disyuntiva o efectos condicionales? i.e No HAAY QUE INSTANCIAR
	PRED =..[C|_],
	C \== 'or',
	instances(TD,_),!.

instances([],_):- !.



generateInstances(P,A,D,POUT,AOUT,DOUT):-
	instancesOR(P,CAND,COR,A,D,POUT,AOUT,DOUT),
	instancesAction(P,[],CAND,COR,A,D,POUT,AOUT,DOUT). % COR = [or(),or()], CAND = [pre(),..]


generateInstances([],_,_,_,_,_):- !.


generateInstances(Pre,Ach,Del,Pre,Ach,Del):- !.


instancesOR(preconditions(NAMEA,[PRED|TD]),[PRED|TCAND],COR,A,D,POUT,AOUT,DOUT):-
	PRED =..[O1|_OR],
	O1 \== 'or',
	instancesOR(preconditions(NAMEA,TD),TCAND,COR,A,D,POUT,AOUT,DOUT).


instancesOR(preconditions(NAMEA,[PRED|TP]),CAND,[PRED|TCOR],A,D,POUT,AOUT,DOUT):-
	PRED =..['or'|_OR],
	instancesOR(preconditions(NAMEA,TP),CAND,TCOR,A,D,POUT,AOUT,DOUT).
	

instancesOR(preconditions(_NAMEA,[]),_CAND,_COR,_A,_D,_POUT,_AOUT,_DOUT). %:- instancesAction(NAMEA,CAND,COR,A,D,POUT,AOUT,DOUT).


instancesAction(preconditions(NAMEA,_),DictNames,[],[DIS|[]],ACHIEVES,DELETES,PRE,ACH,DEL):- % Solo una disyunción
	DIS =..['or'|DISJ],                                                                               % DISJ = [d(),d2()..]
	instancesAction1(NAMEA,DictNames,[],DISJ,ACHIEVES,DELETES,PRE,ACH,DEL).


instancesAction(preconditions(NAMEA,_),DictNames,CAND,[DIS|[]],ACHIEVES,DELETES,PRE,ACH,DEL):- % Disyunción + predicado
	DIS =..['or'|DISJ],                                                                               % DISJ = [d(),d2()..]
	instancesAction2(NAMEA,DictNames,CAND,DISJ,ACHIEVES,DELETES,PRE,ACH,DEL). 


%instancesAction(preconditions(NAMEA,_),[],[DIS|DIST],ACHIEVES,DELETES,PRE,ACH,DEL):-     % Mas disyunciones
	


%instancesAction(preconditions(NAMEA,_),CAND,[DIS|DIST],ACHIEVES,DELETES,PRE,ACH,DEL).      % Mas disyunciones + conjunt


%instancesAction(NAMEA,[],[OR1|OR2],achieves(NAMEA,APRED),deletes(NAMEA,DPRED),[PRE|TPRE],[ACH|TACH],[DEL|TDEL]):-
%	PRE = preconditions(NAMEA,[OR1]),
%	ACH = achieves(NAMEA,APRED),
%	DEL = deletes(NAMEA,DPRED),
	%nuevo nombre para accion
%	NAMEA =..[Name|PARAM],
%	atom_codes(AN,Name),
%	genNAME(AN,NEWNAME),
%	instancesAction(NAMEA,[],OR2,achieves(NAMEA,APRED),deletes(NAMEA,DPRED),TPRE,TACH,TDEL).


instancesAction1(NAMEA,DictNames,[],[DISJ1|DISJ2],achieves(NAMEA,APRED),deletes(NAMEA,DPRED),[PRE|TPRE],[ACH|TACH],[DEL|TDEL]):-
	PRE = preconditions(NAMEA,[DISJ1]),
	ACH = achieves(NAMEA,APRED),
	DEL = deletes(NAMEA,DPRED),
	NAMEA =..[N|P],
	getName(DictNames,DictNames2,N,NEWNAME),
	NEWNAME2 =..[NEWNAME|P],
	instancesAction1(NEWNAME2,DictNames2,[],DISJ2,achieves(NEWNAME2,APRED),deletes(NEWNAME2,DPRED),TPRE,TACH,TDEL).


instancesAction1(_,_,_,[],_,_,[],[],[]).


instancesAction2(NAMEA,DictNames,CAND,[DISJ1|DISJ2],achieves(NAMEA,APRED),deletes(NAMEA,DPRED),[PRE|TPRE],[ACH|TACH],[DEL|TDEL]):-
	PRE = preconditions(NAMEA,[DISJ1|CAND]),
	ACH = achieves(NAMEA,APRED),
	DEL = deletes(NAMEA,DPRED),
	NAMEA =..[N|P],
	getName(DictNames,DictNames2,N,NEWNAME),
	NEWNAME2 =..[NEWNAME|P],
	instancesAction2(NEWNAME2,DictNames2,CAND,DISJ2,achieves(NEWNAME2,APRED),deletes(NEWNAME2,DPRED),TPRE,TACH,TDEL).


instancesAction2(_,_,_,[],_,_,[],[],[]).

%genNAME([C|CT],[C|CT],NEWNAME):- C \== 95, genNAME(CT,[C|CT],NEWNAME).
%genNAME([C|CT],[C|CT],NEWNAME):- C == 95, genNAME(CT,[C|CT],NEWNAME).


getName(DictNames,[NEW|DictNames],Name,NEW):-
	generate(1,100,Name,NEW),
	\+ member(NEW,DictNames).


getName(DictNames,L,Name,N):-
	getName(DictNames,L,Name,N).


generate(N1,N2,OLD,NEWNAME):-     % Generates a random number between 1-100
	random(N1,N2,Number),
	number_codes(Number,S),
	atom_codes(OLD,NN),
	append(NN,S,L),
	atom_codes(NEWNAME,L).	
	
	
	

	