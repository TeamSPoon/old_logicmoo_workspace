

:-ensure_loaded(library(jpl)).
:- include('d:/dev/oaa2_inc.pl').
:-jpl_call(class([jplan,top],['OclEd']),'swiRunsGipo',[{_}],Result),!,Result=='@'(true).
:-interactor.
