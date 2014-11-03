:- module(pddl_problem,_,[dcg]). %  imports the dcg library package. Also it could indicate use_package(dcg).

% Libraries and packages.

:- use_module(readFile2,_). % This module reads the pddl input file and it returns a list of strings.
:- use_module(read_write,_). % This module includes predicates for read and writing a file.
:- use_module(module_lists,_).
:- use_module(library(lists)).
:- use_module(library(write),[write_list1/1, numbervars/3]).
:- use_module(library(atom2term)).
:- use_module(library(iso_byte_char)).


parse_problem(Input, Objects, Goals, Init):-
	pddl2stripsproblem(Objects,Goals,Init,Input,[]),!.

%parse_problem(_,_,_,_):- message(error, 'El problema PPDL actual no puede ser traducido').

%parse_problem(Input, Objects, Goals, Init):-
%	read_file(Input, Lexemas), % Return list of lexemas from the .pddl.
%	pddl2stripsproblem(Objects, Goals, Init, Lexemas,[]). %%%%% File


%KEYWORDS
key(['domain','define','requirements','predicates','constants',
'action','parameters','precondition','effect','and','not','=',
'(',')','forall','when','or','problem','objects','init','goal','length','serial','parallel']).

keywords(Name):-
	key(LIST),
	member(Name,LIST).


%DOMAIN
pddl2stripsproblem(Objects, Goals, Init) --> 
        ['(', 'define', '(', 'problem'], name(_Name_Problem),!, [')'],
	['(', ':', 'domain'], name(_Name_Domain), !, [')'], 
	((objects_def(Objs), {Objects =..[objects|Objs]}) | []),!,  
	((goals_def(Goal,_,_), {Goals =..[goal|Goal]}) | []),!, 
	((init_def(Ini), {Init =..[init|Ini]}) | []),!, 
        [')'].



% % % % % % % % % % % % % %OBJECTS:% % % % % % % % % % % % % % % % % %  
%                                                                     %
% The requirements are features in which the language is factored.    %
% Define the requirements: STRIPS, equality, universal-preconditions, % 
% conditional-effects and disyuntive-preconditions.                   %
%                                                                     %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %
 

objects_def(Objects) --> ['(',':','objects'], list_obj(Objects),!, [')'].


list_obj(Objects) --> 
	name(Name_Obj), 
	list_obj(M_Objects),!, 
	{append(Name_Obj,M_Objects,Objects)}.


list_obj([]) --> [].


%Goal
goals_def(Goals,_,_) --> ['(',':','goal'], ['('], list_goals(Goals,_,_),!,[')'], [')'].


list_goals(Goals, _Parameters, _Predicates) --> 
	name(Name_Goal),!,
	list_parameters(Param_Goal),!, 
	{N =..Name_Goal, G =..[N|Param_Goal], Goals = [G]}.


list_goals(Goals, _Parameters, _Predicates) --> 
	['and'], 
	list_goals_AND(Goals, _Parameters, _Predicates),!.


list_goals([],_,_) --> [].


list_goals_AND(AND_Goal, _Parameters, _Predicates) --> 
	['('], 
	list_goals_AND_1(AND_Goal_1, _Parameters, _Predicates),!, 
	[')'], 
	list_goals_AND(M_AND_Goal, _Parameters, _Predicates),!, 
	{append(AND_Goal_1, M_AND_Goal, AND_Goal)}.


list_goals_AND([],_,_) --> [].


list_goals_AND_1(AND_Goal_1, _Parameters, _Predicates) --> 
	name(Name_Goal),!,
	list_parameters(AND_Goal_Param),!,
	list_goals_AND(M_AND_Goals, _Parameters, _Predicates),!, 
	{N =..Name_Goal, G =..[N|AND_Goal_Param], append([G],M_AND_Goals, AND_Goal_1)}.


list_goals_AND_1([],_,_) --> [].



% % % % % % % % % % % % % %Init: % % % % % % % % % % % % % % % % % % % 
%                                                                     %
% These list contain all the predicates used in the domain definition %
% The predicates definition is (<predicates_name> <parameters>)       % 
%                                                                     %
%                                                                     %
% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % %


init_def(Init) --> 
	['(',':','init'], 
	list_init(Init),!,
	[')'].


list_init(Init) --> 
	['('], 
	name(Name_I),
	list_parameters(Param_Init),!, [')'],
	list_init(M_Init),
	{N =..Name_I,  I =..[N|Param_Init], append([I],M_Init,Init)}.


list_init([]) --> [].


%Paramaters
list_parameters(Parameters) -->  
	name(Name_Parameter), 
	list_parameters(M_Parameters),!, 
	{append(Name_Parameter,M_Parameters,Parameters)}.


list_parameters([]) --> [].



%NAMES see parenthesis and numbers

name([C]) --> [C],!,{\+ keywords(C)}.

name([C|D]) --> [C],name(D),{((C >= 65,C =< 90) ; (C >= 97 ,C =< 122))}.



