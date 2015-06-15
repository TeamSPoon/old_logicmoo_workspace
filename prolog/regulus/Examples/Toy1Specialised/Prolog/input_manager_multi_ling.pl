
:- module(input_manager,
	[lf_to_dialogue_move/3]
    ).


%======================================================================

:- use_module(library(lists)).

%======================================================================

% INPUT MANAGEMENT: LF TO DIALOGUE MOVE

lf_to_dialogue_move(LF, _InState, DialogueMove) :-
	get_do_or_query(LF, DoOrQuery),
	get_device_type(LF, Device),
	get_location(LF, Location),
	get_onoff(LF, OnOff),
	% We only want a specific intensity in a command - for a question, we prefer to leave it vague.
	get_onoff(LF, OnOff),
	(   DoOrQuery = command ->
	    get_intensity(LF, Intensity) ;
	    true
	),
	DialogueMove = [DoOrQuery, device(Device, Location, OnOff, Intensity)],
	!.
lf_to_dialogue_move(_LF, _InState, _DialogueMove) :-
	format('~N~nUnable to convert to dialogue move~n', []),
	fail.

get_do_or_query(LF, DoOrQuery) :-
	member([utterance_type, YNQOrImp], LF),
	translate_ynq_or_imp(YNQOrImp, DoOrQuery),
	!.

translate_ynq_or_imp(ynq, query).
translate_ynq_or_imp(query, query).

translate_ynq_or_imp(imp, command).
translate_ynq_or_imp(command, command).

get_device_type(LF, Device) :-
	member([device, Device0], LF),
	translate_french_constant_if_necessary(Device0, Device),
	!.

% Location is optional
get_location(LF, Location) :-
	member([location, Location0], LF),
	translate_french_constant_if_necessary(Location0, Location),
	!.
get_location(_LF, _Location).

get_onoff(LF, OnOff) :-
	(   member([prep, OnOff], LF) ;
	    member([adj, OnOff], LF)
	),
	is_onoff_constant(OnOff),
	!.
get_onoff(LF, OnOff) :-
	member([action, dim], LF),
	OnOff = on,
	!.
get_onoff(LF, OnOff) :-
	member([action, allumer], LF),
	OnOff = on,
	!.
get_onoff(LF, OnOff) :-
	member([onoff, allum�], LF),
	OnOff = on,
	!.
get_onoff(LF, OnOff) :-
	member([action, �teindre], LF),
	OnOff = off,
	!.
get_onoff(LF, OnOff) :-
	member([onoff, �teint], LF),
	OnOff = off,
	!.
get_onoff(LF, OnOff) :-
	member([action, baisser], LF),
	OnOff = on,
	!.

is_onoff_constant(on).
is_onoff_constant(off).

get_intensity(LF, Intensity) :-
	(   member([prep, on], LF) ;
	    member([adj, on], LF) ;
	    member([onoff, allum�], LF)
	),
	Intensity = 100,
	!.
get_intensity(LF, Intensity) :-
	(   member([prep, off], LF) ;
	    member([adj, off], LF) ;
	    member([onoff, �teint], LF)
	),
	Intensity = 0,
	!.
get_intensity(LF, Intensity) :-
	member([action, dim], LF),
	Intensity = 50,
	!.
get_intensity(LF, Intensity) :-
	member([action, allumer], LF),
	Intensity = 100,
	!.
get_intensity(LF, Intensity) :-
	member([action, �teindre], LF),
	Intensity = 0,
	!.
get_intensity(LF, Intensity) :-
	member([action, baisser], LF),
	Intensity = 50,
	!.
	
translate_french_constant_if_necessary(Const0, Const) :-
	french_constant(Const0, Const),
	!.
translate_french_constant_if_necessary(Const, Const).

french_constant(lampe, light).
french_constant(ventilateur, fan).
french_constant(cuisine, kitchen).
french_constant(salon, living_room).

