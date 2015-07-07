%%% -*- Mode: Prolog; Module: spaceout; -*-

:- module(spaceout, [space_out/3]).

:- meta_predicate space_out(:,?,?).

space_out(Goal, Space, Flag) :-
   integer(Space),
   Space > 0,
   %% If very large we just treat it as infinite
   %% Space =< 16'7fffffff, % must fit in SP_INTEGER (signed long)
   !,
   Margin = 1000,               % for space used by space_out/3 (412 on x86 Win&Linux)
   Space1 is max(Space+Margin, max(1000, Margin)),
   statistics(global_stack, [Used|_]),
   SpaceOutAbs is Used+Space1,

   prolog:'$get_space_out'(OldSpaceOutAbs,_),
   space_out1(Goal, SpaceOutAbs, OldSpaceOutAbs, Flag0),
   Flag = Flag0.
space_out(Goal, Space, Flag) :-
   Culprit = space_out(Goal, Space, Flag),
   prolog:illarg(domain(integer,>(0)), Culprit, 2).


space_out1(Goal, SpaceOutAbs, OldSpaceOutAbs, Flag) :-
   OldSpaceOutAbs > 0, OldSpaceOutAbs < SpaceOutAbs, !, % Surrounding limit is tighter
   call(Goal),                  % no need to do any special processing
   Flag = success.
space_out1(Goal, SpaceOutAbs, OldSpaceOutAbs, Flag) :-
   (

     %% [PM] 3.11.1 SPRM 7549 internal_catch is needed in order to
     %% catch also the internal exception terms used to implement
     %% abort/0, halt/0 etc.
     %% We could probably use call_cleanup to detect internal
     %% exceptions but it is a little tricky.
     prolog:internal_catch(
                           %% Always exits with space_out disabled
                           spaceout:space_out_call(Goal, SpaceOutAbs, Det, ExitKind),
                           Excp,
                           %% Re-raise all exceptions except a pending_space_out exception
                           %% in which case ExitKind==space_out, Det==true
                           spaceout:space_out1_cleanup(Excp, OldSpaceOutAbs, Det, ExitKind)),

     %% Outer space out restored outside of on_exception
     set_space_out(OldSpaceOutAbs)
   ;                            % Goal failed
     %% Outer space out restored outside of on_exception
     set_space_out(OldSpaceOutAbs),
     fail
   ),

   ( Det == true ->             % Goal determinate exit (or exception)
       !                        % remove the above "Goal failed" choice point
   ; true
   ),
   Flag = ExitKind.


space_out1_cleanup(Excp, _OldSpaceOutAbs, Det, ExitKind) :-
   %% handle exception
   is_our_memory_exception(Excp), !,
   Det = true,
   ExitKind = space_out.
space_out1_cleanup(Excp, OldSpaceOutAbs, _Det, _ExitKind) :-
   set_space_out(OldSpaceOutAbs),
   raise_exception(Excp).
                    

%% Always leaves space out disabled.
space_out_call(Goal, SpaceOutAbs, Det, ExitKind) :-
   set_space_out(SpaceOutAbs),
   (
     call_cleanup(call(Goal),
                  Determinate = true
                 )
   ; % this choice point is removed below if Goal exits determinately

     %% Here if Goal left a choice point that was backtracked into but
     %% that did ultimately fail.
     set_space_out(0), % non-det Goal fails or inital call fails
     fail
   ),
   %% Goal exited with success
   set_space_out(0),
   ( Determinate == true ->
       !,                       % remove the above set_space_out choice point
       Det = true               % not the same as Det=Determinate
   ; otherwise ->               % nondet exit
       (
         true % nondet exit from Goal
       ;
         %% about to retry Goal
         set_space_out(SpaceOutAbs),
         fail
       ),
       Det = false
   ),
   ExitKind = success.

%% $set_space_out will fail if SpaceOutAbs does not fit in an SP_INTEGER.
%% On the other hand this means that the space-out limit is in
%% practice infinite so may as well turn off space_out processing.
set_space_out(SpaceOutAbs) :-
   prolog:'$set_space_out'(SpaceOutAbs), !,
   true.
set_space_out(_SpaceOutAbs) :-
   %% infinite space out, turn it off.
   prolog:'$set_space_out'(0).


%% Handle differences in exception format between ISO and SICStus execution modes.
is_our_memory_exception(Excp) :- var(Excp), !, fail.
is_our_memory_exception(resource_error(_,memory)) :-
   prolog_flag(language, sicstus), % should we ignore language mode?
   %% If pending_space_out is not on then the memory exception is a
   %% real out-of-memory exception which we should not catch.
   prolog:'$get_space_out'(_, on).
is_our_memory_exception(error(Excp,_)) :-
   prolog_flag(language, iso),  % should we ignore language mode?
   nonvar(Excp),
   Excp = resource_error(_,memory),
   prolog:'$get_space_out'(_, on). % was on 
