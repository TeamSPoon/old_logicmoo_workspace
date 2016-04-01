:- module(main_perf, [run_perf_test/3], eclipse_language).

:- use_module(gdl_parser).
:- use_module(game_description).
:- use_module(match_info).

:- use_module(time_sync).
:- lib(timeout).
:- lib(random).

run_perf_test(GameFile, OutputFile, SecondsToRun) :-
        load_rules_from_file(GameFile),
        shelf_create(agg/2, 0, StatCounter),
        current_time(Now),
        Deadline is Now+SecondsToRun,
        set_deadline(Deadline),
        time_to_deadline(TimeToDeadline),
        timeout(
          rollouts_loop(StatCounter),
          TimeToDeadline,
          writeln("test done")
        ),
        current_time(EndTime),
        TimePassed is EndTime - Now,
        write_stats_to_file(OutputFile, StatCounter, TimePassed).

write_stats_to_file(OutputFile, StatCounter, TimePassed) :-
        open(OutputFile, 'write', Stream),
        Milliseconds is TimePassed * 1000,
        printf(Stream, "millisecondsTaken = %.0f\n", [Milliseconds]),
        NumRollouts is shelf_get(StatCounter, 1),
        printf(Stream, "numRollouts = %d\n", [NumRollouts]),
        NumStateChanges is shelf_get(StatCounter, 2),
        printf(Stream, "numStateChanges = %d\n", [NumStateChanges]),
        printf(Stream, "version = 1.1\n", []),
        close(Stream).

rollouts_loop(StatCounter) :-
        run_one_rollout(StatCounter),
        rollouts_loop(StatCounter).

run_one_rollout(StatCounter) :-
	initial_state(InitialState),
	set_current_state(InitialState),
	continue_rollout(0, StatCounter).

% Now need to figure out Prolog control structures
% Need one move per role
continue_rollout(StateChangesSoFar, StatCounter) :-
        roles(Roles),
        get_current_state(State),
        select_moves(Roles, Moves, State),
	state_update(State, Moves, NewState),
        set_current_state(NewState),
        NewStateChanges is StateChangesSoFar + 1,
        (terminal(NewState)
         -> record_stats(NewStateChanges, StatCounter)
         ; continue_rollout(NewStateChanges, StatCounter)).

record_stats(StateChanges, StatCounter) :-
        shelf_inc(StatCounter, 1), % num rollouts
        OldStatesTotal is shelf_get(StatCounter, 2),
        NewStatesTotal is OldStatesTotal + StateChanges,
        shelf_set(StatCounter, 2, NewStatesTotal).

select_moves([], [], _).
select_moves([Role|_], [MoveForRole|_], State) :-
        get_random_move(Role, MoveForRole, State).

:- mode get_random_move(++, -, ++).
get_random_move(Role, Move, Z) :-
        setof(M, legal(Role, M, Z), Ms),
	MoveVector=..[[]|Ms],
	I is (random mod length(Ms))+1,
	arg(I, MoveVector, Move).
