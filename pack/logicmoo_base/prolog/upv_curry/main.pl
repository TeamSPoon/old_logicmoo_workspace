/*%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

                            UPV-Curry Interpreter

  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%*/

% ---------------------------------------------------------------------------
% Global Control -----------------------------------------------------
% ---------------------------------------------------------------------------

:- use_module(library(system)), use_module(library(lists)).
:- compile(['common.pl','parser.pl','typecheck.pl','gendtree.pl',
            'write2.pl','eval.pl','menu.pl']).
:- dynamic 
     prelude/3. %Parser,TypeChecker,DefTree

%---Save all information about loaded program in the functor "prelude"
save_state :-
     % Info from Parser
        findall(type(X,Y),type(X,Y),               TypesParser),
        findall(function(X,Y),function(X,Y),       FuncsParser),
        findall(constructor(X,Y),constructor(X,Y), ConsParser),
        findall(infix(X,Y,Z),infix(X,Y,Z),         InfixParser),
        findall(builtin(X),builtin(X),             NoRedefParser),

     % Info from Type Checker
        findall(annotation(X,Y),annotation(X,Y),       AnnCheckType),
        findall(function(X,Y,Z),function(X,Y,Z),       FuncsCheckType),
        findall(constructor(X,Y,Z),constructor(X,Y,Z), ConsCheckType),

     % Info from Def Trees
        findall(dt(X,Y,Z),dt(X,Y,Z),                 DTDefTree),

      retractall(prelude(_,_,_)),
      assert(
       prelude(
        [TypesParser,FuncsParser,ConsParser,InfixParser,NoRedefParser],
        [AnnCheckType,FuncsCheckType,ConsCheckType],
        [DTDefTree]
      )).

%---Retrieve information about prelude for parser 
% (called in "program")
prelude_parser :-
      prelude(InfoParser,_,_),
      assertAll(InfoParser).

% Retrieve information about prelude for type checker 
% (called in "checkType")
prelude_typechecker :-
      prelude(_,InfoTypeChecker,_),
      assertAll(InfoTypeChecker).

% Retrieve information about prelude for deftree generation 
% (called in "deftrees")
prelude_deftree :-
      prelude(_,_,InfoDefTree),
      assertAll(InfoDefTree).

assertAll([]).
assertAll([X|Xs]) :- assertAll1(X),assertAll(Xs).

assertAll1([]).
assertAll1([X|Xs]) :- assert(X),assertAll1(Xs).

%---Load Prelude File
load_prelude :-
   write('Loading prelude...'),nl,
   name(':load "prelude"',Command), %--needed to allow "
   on_exception(_,
     ( command_read(Command),! ;halt ),
     (write('Internal error processing prelude file'),nl,halt) ),
   nl.

:- 
   nl,nl,
   write('***********UPV-Curry interpreter (Version 14 Apr 2000)*******************'),
   nl,nl,
   % Save state with basic info
   save_state,
   % Load Prelude
   load_prelude,
   % Save state with prelude info
   save_state,
   % Help
   command_help(":help "),
   % Interacting Prompt
   !,main.
