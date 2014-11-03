:- module(module_lists,_,_).

% Libraries and packages.

:- use_module(read_write,_). % This module includes predicates for read and writing a file.
:- use_module(library(lists)).
:- use_module(library(write),[write_list1/1, numbervars/3]).



% is_predicate(Predicate, List_of_Predicates) verifies if the current predicate is an instance of the defined predicates in the domain.

% is_predicate((Element_name, List_Parameters), Action_Parameters, List_Predicates):-

%	parameters_belong(List_Parameters, Action_Parameters),!,

%	contains1(List_Predicates, (Element_name, List_Parameters)),!.


is_predicate((Element_name, List_Parameters), Action_Parameters, List_Predicates):-

	parameters_belong(List_Parameters, Action_Parameters),!,

	contains((Element_name, List_Parameters), List_Predicates),!.


% contains

contains((Element_name, List_Parameters), [(Element_name1, List_Parameters1)|_]):-

	equal_predicate((Element_name, List_Parameters), (Element_name1, List_Parameters1)),!.


contains((Element_name, List_Parameters), [(Element_name1, List_Parameters1)|Tail]):-

	\+ equal_predicate((Element_name, List_Parameters), (Element_name1, List_Parameters1)),

	contains((Element_name, List_Parameters), Tail),!.



% equal_predicate(Predicate1, Predicate2) predicates are the same when both predicate name and the amount of parameters are equals.

equal_predicate((Predicate1_Name, Predicate1_Parameters), (Predicate2_Name, Predicate2_Parameters)):-

	equal_lists(Predicate1_Name, Predicate2_Name),

	length(Predicate1_Parameters, Predicate1_Length), 

	length(Predicate2_Parameters, Predicate2_Length),

	Predicate1_Length == Predicate2_Length.



% is_parameter(Parameter, List_of_Parameters)

parameters_belong(List_Parameters1, List_Parameters2):-

	sublist(List_Parameters1, List_Parameters2),!.



% is_constant(Constant, List_Constants)

is_constant(Constant, List_Constants):-

	contains1(List_Constants, Constant).


% is_predicate(([pred1], [a]), [a,b], [([pred2], [b]), ([pred1], [a])]).
