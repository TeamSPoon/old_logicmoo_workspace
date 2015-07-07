%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% FILE: Delivery/delivery.pl
%
% Golog code for a delivery robot
%
% This file contains system independent predicates
% For more information on Golog and some of its variants, see:
%     http://www.cs.toronto.edu/~cogrobo/
%
% WRITTEN BY: Maurice Pagnucco and Hector J. Levesque
% REVISED: June 15, 2000 
% TESTED: ECLiPSe 4.2.2 under RedHat Linux 6.2
%         ECLiPSe 5.0 under RedHat Linux 6.2
%         SWI Prolog 3.3.6 under RedHat Linux 6.2
%         LPA DOS-Prolog 3.83 under DOS on HP200LX
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%                             June 15, 2000
%
% This software was developed by the Cognitive Robotics Group under the
% direction of Hector Levesque and Ray Reiter.
% 
%        Do not distribute without permission.
%        Include this notice in any copy made.
% 
% 
%         Copyright (c) 2000 by The University of Toronto,
%                        Toronto, Ontario, Canada.
% 
%                          All Rights Reserved
% 
% Permission to use, copy, and modify, this software and its
% documentation for non-commercial research purpose is hereby granted
% without fee, provided that the above copyright notice appears in all
% copies and that both the copyright notice and this permission notice
% appear in supporting documentation, and that the name of The University
% of Toronto not be used in advertising or publicity pertaining to
% distribution of the software without specific, written prior
% permission.  The University of Toronto makes no representations about
% the suitability of this software for any purpose.  It is provided "as
% is" without express or implied warranty.
% 
% THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
% SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
% FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
% SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
% RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
% CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
% CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% This is a Golog delivery robot. It assumes clipping actions to start
% and stop motion behaviour, and exogenous requests for deliveries.
% There are no sensing actions (other than the exogenous ones).
%
% This file defines the following predicates describing the interface
% to the outside world:
% -- prim_fluent(?Fluent): for each primitive fluent
% -- prim_action(?Action): for each primitive action
% -- exog_action(?Action): for each exogenous action
% -- senses(+Action, +Fluent): for each sensing action
% -- poss(+Action, +Condition): when Condition, Action is executable
% -- initially(+Fluent, +Value): Fluent has Value in S0, the initial situation
% -- causes_val(+Action, +Fluent, +Value, +Cond): when Cond holds, doing
%      Action causes Fluent to have Value
% -- proc(+Name, +Program): Golog complex action. It consists of a program
%      Name and a Program to be executed
% -- execute(+Action, +History, -SensingResult): do the Action, return
%      the SensingResult. The History is passed for diagnostic or other
%      use. The result is ignored unless action is a sensing action
% -- exog_occurs(-ActionList): return a list of exog_actions
%      that have occurred since the last time it was called
%      The predicate always succeeds returning [] when there are none
% -- initialize: run at start of programs
% -- finalize: run at end of programs
%
% The following predicates are assumed to be provided by the implementation
% independent Prolog/RCX communication predicates in legorcx.pl:
% -- initializeRcx: prepare the RCX for reading
% -- finalizeRcx: finished with RCX for reading and writing
% -- sendRcxActionNumber(+Num, -Result): send the action number Num
%      to the RCX, and return the value Result from the RCX
% -- receiveRcxActionNumber(-Actions): receive 0 or 1 action numbers in
%      list Actions from RCX. Fail if no reply from RCX
%
% The following predicates are assumed to be defined in the main
% program file (as they may contain Prolog specific code):
% -- initializeExog: perform any initialization of other sources of
%      exogenous actions that is required
% -- finalizeExog: things to do for other sources of exogenous actions
%      at end of program
% -- checkOtherExog(-ExogList): check whether a request has been
%      entered via keyboard
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DECLARATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% senses(+Action, +Fluent): Action senses the value of Fluent
senses(_, _) :-
    fail.                                % No sensing actions

% prim_action(?Action): Action is a primitive action and it can be executed
%     on the RCX
prim_action(turnaround).                 % Change direction
prim_action(signal_arrival).             % Announce arrival at waystation
prim_action(start_to_next_station).      % Start motion to next waystation

% exog_action(?Action): Actions is an exogenous action and its occurrence
%     can be reported to Golog
exog_action(arrive_at_station).          % Arrive successfully and stop
exog_action(stop_abnormally).            % Stop because confused
exog_action(push_go_button).             % Button on robot is pushed
exog_action(request_delivery(_, _)).     % Call robot to deliver package
exog_action(cancel_request(_, _)).       % Tell robot to never mind

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Fluents
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% prim_fluent(?Fluent): Fluent is a primitive fluent
prim_fluent(location).              % The last waystation the robot occupied
prim_fluent(direction).             % Aiming 1 (up) or -1 (down)
prim_fluent(motion).                % Moving, stopped, or suspended (lost)
prim_fluent(delivery_requested(From)) :-     % Delivery request is pending
    way_station(From).                       % From is 0 or way station number
prim_fluent(holding_delivery_for(Loc)) :-    % Pickup done, drop off to go
    way_station(Loc).

% way_station(?N): N is a way station in this example. It is an
%     auxiliary fluent
way_station(N) :-
    N = 1; N = 2; N = 3; N = 4; N = 5; N = 6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Causal laws
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% causes_val(+Action, +Fluent, +Value, +Cond): When Cond holds, doing
%     Action causes Fluent to have Value
causes_val(turnaround, direction, D, D is (-1) * direction).
causes_val(arrive_at_station, location, N, N is location + direction).
causes_val(start_to_next_station, motion, moving, true).
causes_val(arrive_at_station, motion, stopped, true).
causes_val(stop_abnormally, motion, suspended, true).
causes_val(request_delivery(N, M), delivery_requested(N), M, true).
causes_val(cancel_request(N, _), delivery_requested(N), 0, true).
causes_val(push_go_button, delivery_requested(N), 0, location = N).
causes_val(push_go_button, holding_delivery_for(M), 0, location = M).
causes_val(push_go_button, holding_delivery_for(M), 1,
    delivery_requested(location) = M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Preconditions of prim actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% poss(+Action, +Condition): When Condition is true, Action is possible
poss(turnaround, motion = stopped).
poss(start_to_next_station, 
    and(motion=stopped, 
        or(and(direction = 1, location < 6), 
            and(direction = (-1), location > 1)))).
poss(signal_arrival, true).

poss(arrive_at_station, motion = moving).
poss(stop_abnormally, motion = moving).
poss(request_delivery(_, _), true).
poss(cancel_request(N, M), delivery_requested(N) = M).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Initial state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initially(+Fluent, +Value): Fluent has Value at S0, the initial situation
initially(location, 3).
initially(direction, 1). 
initially(motion, stopped).
initially(delivery_requested(_), 0).        % No pending requests
initially(holding_delivery_for(X), 1) :-
    X = 4 ; X = 6.                          % Pending dropoff
initially(holding_delivery_for(X), 0) :-
    X = 1; X = 2; X = 3; X = 5.             % No other pending dropoffs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Golog PROGRAM
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of complex conditions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% proc(+Name, +Program): A user-defined procedure called Name with
%     Program body
proc(stop_requested(N),
    or(some(m, and(delivery_requested(N) = m, way_station(m))), holding_delivery_for(N) = 1)).

proc(next_location_to_serve(N), stop_requested(N)).  % Random choice


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definitions of complex actions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(head_to_next_station(D),          % Head to next waystation up or down 
     if(direction = D, start_to_next_station, turnaround)).

proc(recover_position, ?(reset(location, direction))). 

% reset(Location, Direction): auxiliary predicate
reset(Location,Direction) :-
    write('**** I got lost heading from waystation '), write(Location),
    (Direction = 1 -> write(' going up.') ; write(' going down.')), nl,
    write('**** Please position me between waystations in the correct '),
    write('direction, and type any key when ready: '),
    get(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  Main Routine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

proc(control, prioritized_interrupts(
    [interrupt(motion = suspended, [recover_position, start_to_next_station]),
    interrupt(motion = moving, wait),
    interrupt(stop_requested(location), [signal_arrival,wait]),
    interrupt(n, next_location_to_serve(n), 
        [if(location < n, head_to_next_station(1), head_to_next_station(-1))]),
    interrupt(location > 1, head_to_next_station(-1)),
    interrupt(true, wait)
])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HOOKS TO legorcx.pl CODE
%  This section provides predicates for executing actions from the IndiGolog
%  interpreter on the RCX and returning exogenous actions detected by
%  the RCX to the interpreter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% initialize: Perform any application dependent initialization
initialize :-
    initializeRcx,
    initializeExog.

% finalize: Application dependent wrap-up
finalize :-
    finalizeRcx,
    finalizeExog.

% execute(+Action, +History, -SensingResult): Execute Action on RCX returning
%     SensingResult. Current action History is supplied for debugging
%     purposes in case something goes wrong
execute(Action, _, SensingResult) :-
    write('Executing action: '), write(Action), nl,
    actionNum(Action, N),
    sendRcxActionNumber(N, SensingNumber),
    translateSensorValue(Action, SensingNumber, SensingResult),
    write('    Sensing result: '), write(SensingResult), nl, nl.

% If previous clause fails, call user-defined debug routine.
% The debug routine can reattempt the action returning SensingResult
execute(Action, History, SensingResult) :-
    debugRcx(Action, History, SensingResult).

% exog_occurs(-ExogList): Check for occurrence of exogenous actions and
%     return them in list ExogList. Exogenous actions can occur at
%     various sources: here we check the RCX and the keyboard
exog_occurs(ExogList) :-
    checkRcxExog(RcxExogList),
    (RcxExogList == [] -> true;
        (write('    Rcx exogenous action: '), write(RcxExogList), nl, nl)),
    checkOtherExog(OtherExogList),
    (OtherExogList == [] -> true;
        (write('    Other exogenous action: '), write(OtherExogList), nl, nl)),
    append(RcxExogList, OtherExogList, ExogList).

% checkRcxExog(-RcxExogList): Check for occurrence of exogenous actions
%     at RCX. At present RCX can only report one exogenous action at a time
checkRcxExog([Action]) :-
    receiveRcxActionNumber([N]),
    actionNum(Action, N),
    write('Exogenous action: *'), write(Action), write('* has occurred'), nl, !.

checkRcxExog([]).    % No exogenous action from RCX

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Use the following for testing independent of legorcx.pl.
% Uncomment to run.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%%execute(Action, _, SensingResult) :-
%%    write('Executing action: '), write(Action), nl,
%%    (senses(Action, _) ->
%%        (write('[Enter Sensing value, terminate with "."]: '),
%%             read(SensingResult));
%%        nl),
%%    write('    Sensing result: '), write(SensingResult), nl, nl.
%%
%%% If previous clause fails, call user-defined debug routine
%%execute(Action, History, SensingResult) :-
%%    debugRcx(Action, History, SensingResult).
%%
%%% It is probably better to do this by only checking for exogenous actions
%%%    from the keyboard but, if you want to go through the process slowly,
%%%    do it this way!
%%
%%exog_occurs(ExogList) :-
%%    write('Enter list (possibly empty) of exogenous actions: '),
%%    read(TempExogList),
%%    ((TempExogList = [_|_] ; TempExogList == []) ->
%%        (ExogList=TempExogList,
%%            write('    Exogenous actions: *'), write(ExogList),
%%            write('* have occurred'), nl, nl);
%%        (write('ERROR: You must enter a list (possibly empty) of actions'),nl,
%%            exog_occurs(ExogList))
%%    ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Action/message mappings - numbers must correspond to NQC code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% actionNum(?Action, ?ActionNumber): Returns ActionNumber associated
%     with Action and vice versa. ActionNumber can be sent to the RCX
%     for execution of Action. It can be returned from the RCX to
%     report the occurrence of exogenous Action
actionNum(turnaround, 0).
actionNum(signal_arrival, 1). 
actionNum(start_to_next_station, 2).
actionNum(arrive_at_station, 3).
actionNum(stop_abnormally, 4).
actionNum(push_go_button, 5).
actionNum(recover_position, 6).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Translation of sensor values from RCX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% translateSensorValue(+Action, +SensorValue, SensingResult): Translate
%     the value SensorValue returned by the RCX sensor into a legal
%     SensingResult under Action

translateSensorValue(_, SensorValue, SensorValue).  % Just leave it for now

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEBUG ROUTINES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% debugRcx(+Action, +History, -SensingResult): The sendRcxActionNumber/2
%     predicate failed (RCX panicked or there was a problem with the
%     communication). This predicate attempts to provide some basic debug
%     and error recovery.
%     If you can't be bothered, uncomment the following clause and
%     the program will abort on failure to execute an action
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%debugRcx(_, _, _) :-
%%    write('ERROR: A problem was encountered while trying to execute action'),
%%    nl,
%%    abort.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
debugRcx(Action, History, SensingResult) :-
    write('** There is a problem with the RCX. It may need to be reset.'), nl,
    errorRecoveryData(History),
    errorRecoveryProc,
    execute(Action, History, SensingResult). % Try action again

% errorRecoveryData(+History): Extract values of primitive fluents at
%     the point where Hist actions are performed.
errorRecoveryData(History) :-
    write('    Actions performed so far: '),
    write(History), nl,
    bagof(U, prim_fluent(U), FluentList),
    printFluentValues(FluentList, History).

% printFluentValues(+FluentList, +History): Print value of primitive fluents
%     at the point where History actions have been performed
printFluentValues([], _) :-
    write('-----------------------------------------------'),
    nl.

printFluentValues([Hf | FluentList], History) :-
    write('    Primitive fluent '),
    write(Hf),
    write(' has value '),
    has_val(Hf, Hv, History),
    write(Hv), nl,
    printFluentValues(FluentList, History).

% errorRecoveryProc: What to do in case of error. In this case, ask the user
%     to reposition the RCX so that last action can be re-attempted
errorRecoveryProc:-
    write('If you wish to abort, enter "a".'), nl,
    write('If you wish to continue execution, place RCX in a position'), nl,
    write('consistent with these values and hit any other key.'), nl,
    get0(Val),
    get0(_),                     % Clear carriage return
    (Val == 65; Val == 97) ->    % 65 is ASCII 'A', 97 is ASCII 'a'
         abort;
         true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EOF: Delivery/delivery.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
