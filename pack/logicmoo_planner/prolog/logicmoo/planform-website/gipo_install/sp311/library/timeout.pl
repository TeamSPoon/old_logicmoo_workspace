/* Copyright (C) 1995, Swedish Institute of Computer Science. */

%   File       : timeout.pl
%   Author     : Mats Carlsson
%   Purpose    : Meta-calls with time-out

%% [PM] 3.9.2
%% Rewritten to use absolute instead of relative expiry times.
%% This avoids accumulating errors from inaccurate OS timers
%% (SPRM 3630, inaccurate linux getitimer)
%% Using absolute times also simplifies the code and avoids ugly
%% $smash/2 (a.k.a setarg).



:- module(timeout, [time_out/3]).

:- public time_out/4, time_out_rec/4.

:- meta_predicate time_out(:,?,?).

%% [PM] 3.9.2 no need to meta this.
%% :- meta_predicate time_out(:,?,?,?). % [PM] 3.9.2 why was this meta?

time_out(Goal, Time, Flag) :-
   %% format(user_error, '~NDBG: This version of timeout.pl contains debug code~n', []),flush_output(user_error),
   
   integer(Time),
   Time > 0, Time < 16'7fffffff, !, % [PM] we no longer need to limit Time to a fixnum. We could limit to =< LONG_MAX.
   '$stop_timer_a'(ContExpires),


   %% [PM] 3.10.0b1 Moved this adjustment to timeout.c since some
   %%      platforms use other TimerResolution than 10ms (Tru64 1/60s)
   %%
   %% [PM] 3.9.2 Note that for N less than or equal to the OS timer
   %% resolution the following will sometimes timeout
   %%   time_out(time_out(true,1000,_),N).
   %%
   %% This is because the inner time_out goal will get charged with
   %% either no time at all or 10ms depending on when the OS time
   %% interrupt happens (assuming a timer resolution of 10ms).
   %%
   %% The symptom is an error reported for test1 in Suite/timeout.pl
   %%
   %% The workaround is to do (10ms on x86 Linux (x86-linux-glibc2.2))
   %% TimerResolution = 10, Limit is max(Time, TimerResolution+1),
   Limit = Time,

   %% 3.8: catch abort etc. too!
   prolog:internal_catch(timeout:time_out(Goal, Limit, ContExpires, Flag0),
                         Excp,
                         timeout:time_out_rec(Excp, ContExpires, Flag0)),
   Flag = Flag0.
time_out(Goal, Time, Flag) :-
   prolog:illarg(domain(integer,between(1,16'7ffffffe)),
                 time_out(Goal,Time,Flag), 2).


% This is like a Byrd box with two assignable variables:
% Limit - resource limit for this goal (a relative time).
% ContExpires - absolute time at which ancestor goal should time out..
time_out(Goal, Limit, ContExpires, success) :-
   %% The time limit applies to CPU per solution.
   %% To get total CPU we should replace Limit with an absolute time
   %% and change start_timer appropriately.
   prolog:'CHOICE IDIOM'(Chpt),
   (                            % choice point 1
       start_timer(ContExpires, Limit), % set timer to fire at time min(ContExpires, <<CPU now>>+Limit)
       Goal
   ;   time_out_exit(ContExpires), % set timer to fire when ancestor goal expires
       fail
   ),
   (                            % choice point 2
       time_out_exit(ContExpires)
   ;
       '$stop_timer_a'(_ContExpires1),

%%      %% DBG: remove in final code
%%      ( ContExpires == _ContExpires1 -> % expected
%%          true
%%      ;
%%          format(user_error, '~NERROR: ContExpires!=_ContExpires1 (~q!=~q) in timeout.pl~n', [ContExpires, _ContExpires1]), flush_output(user_error),
%%          true
%%      ),

       start_timer(ContExpires, Limit),
       fail
   ),
   %% Cut if only chp 1 and 2 are live (so goal has exited without leaving choice points).
   %% Would be possible to use call_cleanup(Goal, Determinate=true), ..., ( Determinate==true -> !; true )?
   prolog:'$clean_up'(Chpt, 2).

%% Handle exception from Goal. In particular the time_out exception.
%% The timer is running (unless the exception is time_out) and needs
%% to be shut off and reset to/stopped expiry of ancestor goal
%%
%% [PM] 3.9.2 NOTE: If we get here due to an ordinary exception and
%% then a time_out gets delivered we will fail to catch the time_out
%% and incorrectly pass it on to the user code. This bug has always
%% been there and I cannot think of any way to completely avoid this
%% possibility (short of support in the runtime system).
time_out_rec(Excp, ContExpires, Flag) :-
   %% time_out_exit is slight overkill if Excp==time_out was raised by
   %% the signal handler (since, in that case the timer is off).
   %% But consider what happens if user does raise_exception(time_out)
   %% with a running timer.

%%  %% DBG: remove in final code
%%  ( '$timer_now'(Now),
%%      format(user_error, '~NDBG: ~q, timer_now(~w)~n in timeout.pl~n', [time_out_rec(Excp, ContExpires, Flag), Now]), flush_output(user_error)
%%  ),

   time_out_exit(ContExpires),
   time_out_rec1(Excp, ContExpires, Flag).

time_out_rec1(time_out, ContExpires, Flag) :-
   '$timer_now'(Now),
   has_not_expired(ContExpires, Now), % Ancestor has not expired so the time_out was for our goal
   !,
   Flag = time_out,
   start_timer(ContExpires, off). % restart timeout for ancestor goal (or stop timer if ContExpires is 'off')
time_out_rec1(Excp, _ContExpires, _) :- % some other exception or a time_out that applies to ancestor goal time_out/3
%%  %% DBG: remove in final code
%%  format(user_error, '~NDBG: Passing on exception (~q) ContExpires==~q, from time_out_rec1 in timeout.pl this is expected~n', [Excp, _ContExpires]), flush_output(user_error),

   raise_exception(Excp).


%% Stop timer for this goal and reinstate the expiry time for the ancestor goal
time_out_exit(ContExpires) :-
   '$stop_timer_a'(_LimitExpires), % 
   start_timer(ContExpires, off).

%% start_timer(ContExpires, Limit).
%% set timer to fire at time min(ContExpires, <<CPU now>>+Limit)
%% Either ContExpires or Limit or both can be the atom 'off'.
start_timer(ContExpires, Limit) :-
   '$start_timer_a'(ContExpires, Limit,Err),

%%  %% DBG: remove in final code
%%  format(user_error, '~NDBG: ~q: ~q~n in timeout.pl~n', [start_timer(ContExpires, Limit), '$start_timer_a'(ContExpires, Limit,Err)]), flush_output(user_error),

   ( Err == 0 ->
       true
   ; Err == 1 ->                % ContExpires has already happened
       raise_exception(time_out) % imminent anyway
   ;                            % some other error
       %% This will not happen except for internal errors, currently a
       %% system_error is raised from timeout.c

%%      %% DBG: remove in final code
%%      format(user_error, '~NERROR: Err==~q~n in timeout.pl~n', [Err]), flush_output(user_error),

       raise_exception(time_out) % fake out.
   ).

%% limit_min(off, Y, Y) :- !.
%% limit_min(Y, off, Y) :- !.
%% limit_min(X, Y, Z) :- Z is min(X,Y).

%% limit_gt(off, _) :- !.
%% limit_gt(X, Y) :- X > Y.

%% limit_difference(off, _, off) :- !.
%% limit_difference(X, off, X) :- !.
%% limit_difference(X, Y, Z) :- Z is X-Y.

%% has_not_expired(ContExpires, Now)
%% Succeed if ContExpires has not yet occurred. This includes the case
%% when it will never occur (being 'off')


has_not_expired(off, _) :- !.
has_not_expired(X, Y) :- X > Y.

%% has_not_expired(ContExpires, Now) :-
%%    %% DBG: remove in final code
%%    format(user_error, '~NDBG: ~q', [has_not_expired(ContExpires, Now)]),
%%    ( has_not_expired1(ContExpires, Now) ->
%%        format(user_error, 'true~n', []), flush_output(user_error)
%%    ;
%%        format(user_error, 'false~n', []), flush_output(user_error),
%%        fail
%%    ).


:- dynamic foreign/2, foreign_resource/2.

%% % [PM] 3.8.7 will raise system_error if an error occurred (Solaris: EACCESS from setitimer)
%% foreign(to_start_timer, '$start_timer'(+term,[-integer])).
%% % [PM] 3.8.7 will silently return 'off' if an error occurred.
%% foreign(to_stop_timer, '$stop_timer'([-term])).

% [PM] 3.9.2
foreign(to_start_timer_a, '$start_timer_a'(+term, +term, [-integer])).
foreign(to_stop_timer_a, '$stop_timer_a'([-term])).
foreign(to_timer_now, '$timer_now'([-term])).


foreign_resource(timeout, [
	init(to_init),
	deinit(to_deinit),
	%% [PM] pre 3.9.2: to_start_timer,to_stop_timer
        to_start_timer_a, to_stop_timer_a, to_timer_now
                          ]).

:- load_foreign_resource(library(system(timeout))).

