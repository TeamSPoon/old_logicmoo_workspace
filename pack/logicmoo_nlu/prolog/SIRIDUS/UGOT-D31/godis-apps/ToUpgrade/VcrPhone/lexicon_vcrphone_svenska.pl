:- module( lexicon_vcrphone_svenska, [output_form/2, input_form/2,
				      yn_answer/1]).
:- use_module( library(vcr_languages) ).
:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_svenska) ).
:- use_module( library(telephone_names) ).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

output_form( ask(X^(task(X))), "Vad kan jag hj�lpa dig med?" ).

output_form( ask(task(T)), Str ):-
	output_form(task(T), StrT ),
	append( "Vill du ", StrT, Str0 ),
	append( Str0, "?", Str).

output_form( ask(TaskList), Str ) :-
	TaskList = [task(_)|_],
	altlist2altstr_or( TaskList, AltStr ),
	append( "Vill du ", AltStr, Str0 ),
	append( Str0, "?", Str).

output_form( ask(quit), "Vill du avsluta GoDiS?" ).


output_form( ask(X^new_program_position(X)),
	     "Vilken kanal vill du ha?" ).
output_form( ask(X^program_position_to_store(X)),
	     "Vilken kanal vill du ha?" ).
output_form( ask(X^date_to_store(X)),
	     "Vilket datum?" ).
output_form( ask(X^start_time_to_store(X)),
	     "Hur dags vill du att inspelningen ska b�rja?" ).
output_form( ask(X^stop_time_to_store(X)),
	     "Hur dags vill du att inspelningen ska sluta?" ).
output_form( ask(X^program_number(X)),
	     "Vilket programnummer vill du radera?" ).
output_form( ask(X^new_clock(X)),
	     "Vilken tid?" ).

output_form( ask(X^base_station_language(X)),
	     "Vilket spr�k vill du �ndra basstationen till?" ).
output_form( ask(X^handset_language(X)),
	     "Vilket spr�k vill du �ndra handtelefonen till?" ).
output_form( ask(X^ring_volume(X)),
	     "V�lj en volymniv� mellan 0 och 7." ).
output_form( ask(X^earpiece_volume(X)),
	     "V�lj en volymniv� mellan 0 och 7." ).
output_form( ask(X^signal_type(X)),
	     "V�lj signaltyp: intern, extern, meddelandesignal eller s�ksignal.").
output_form( ask(X^tone_or_melody(X)),
	     "V�lj en ton eller melodi: l�g, medium, h�g, blandad, hoppig, Eine kleine Nachtmusik, Toccata, Elise, Samba eller Blues Rhythm." ).
output_form( ask( X^phonebook_name_to_add(X) ),
	     "Vilket namn vill du l�gga till?" ).
output_form( ask( X^phonebook_number_to_add(X) ),
	     "Vilket nummer vill du l�gga till?" ).
output_form( ask( X^phonebook_entry_to_delete(X) ),
	     "Vem vill du radera? S�g ett namn." ).
output_form( ask( X^phonebook_name_to_find(X) ),
	     "Vilket namn vill du leta efter?" ).

output_form( ask(main_menu), "Vill du byta dom�n?" ).

output_form( ask( call(Number) ), Cs ) :-
	format_to_chars( 'Numret �r ~a. Vill du ringa detta nummer?', [Number], Cs ).

output_form( task(vcr_top), "g� till videon" ).
output_form( task(vcr_change_play_status), "�ndra spelningsl�ge p� videon" ).
output_form( task(vcr_timer_recording), "g� till tidsinst�lld inspelning" ).
output_form( task(vcr_settings), "g� till inst�llningar" ).
output_form( task(vcr_query), "st�lla en fr�ga" ).
output_form( task(vcr_new_program_position), "�ndra kanal" ).
output_form( task(vcr_play), "starta spelning" ).
output_form( task(vcr_stop), "stoppa" ).
output_form( task(vcr_ff), "spola fram" ).
output_form( task(vcr_rew), "spola bak" ).
output_form( task(vcr_pause_still), "pausa" ).
output_form( task(vcr_still_adv), "advance" ). % SE: Vad �r detta?
output_form( task(vcr_rec), "spela in" ).
output_form( task(vcr_add_program), "l�gga till ett program" ).
output_form( task(vcr_add_program1), "l�gga till ett program" ).
output_form( task(vcr_delete_program), "radera ett program" ).
output_form( task(vcr_delete_program1), "radera ett program" ).
output_form( task(vcr_set_clock), "st�lla in klockan" ).
output_form( task(vcr_query_status), "fr�ga om spelningsl�ge" ).
output_form( task(vcr_query_channel), "ta reda p� vilken kanal som �r p�" ).

output_form( task(tp_top), "g� till telefonen" ).
output_form( task(tp_base_station), "g� till basstationen" ).
output_form( task(tp_handset), "g� till handtelefonen" ).
output_form( task(tp_phonebook), "g� till telefonboken" ).
output_form( task(tp_answering_machine), "g� till telefonsvararen" ).
%output_form( task(tp_answering_machine_onoff), "switch it on or off" ).
output_form( task(tp_answering_machine_switch_on), "aktivera den" ).
output_form( task(tp_answering_machine_switch_off), "st�nga av den" ).
output_form( task(tp_base_station_settings), "g� till basstationsinst�llningar" ).
output_form( task(tp_base_station_settings_volume), "�ndra volymen" ).
output_form( task(tp_base_station_settings_autoanswer), "st�nga av eller p� autosvar" ).
output_form( task(tp_base_station_settings_autoanswer_on), "aktivera autosvar" ).
output_form( task(tp_base_station_settings_autoanswer_off), "st�nga av autosvar" ).
output_form( task(tp_base_station_settings_basic), "g� till basinst�llningar" ).
output_form( task(tp_base_station_settings_basic_date), "st�lla in datum" ).
output_form( task(tp_base_station_settings_basic_language),
	     "�ndra basstationens spr�k" ).
output_form( task(tp_handset_warnings), "ordna med ringsignaler och volym" ).
output_form( task(tp_handset_language), "�ndra handtelefonens spr�k" ).
output_form( task(tp_handset_earpiece_volume), "�ndra h�rlurens volym" ).
output_form( task(tp_handset_warnings_volume), "�ndra ringsignalens volym" ).
output_form( task(tp_handset_warnings_signals), "ordna med ringsignalen" ).
output_form( task(tp_phonebook_new_entry), "g�ra till�gg i telefonboken" ).
output_form( task(tp_phonebook_search_entry), "s�ka i telefonboken" ).
output_form( task(tp_phonebook_delete_entry), "radera n�got i telefonboken" ).

output_form( task(main_menu), "byta dom�n" ).

output_form( inform(already_playing), "Videon �r redan i spell�ge" ).
output_form( inform(now_playing), "Videon spelar nu." ).
output_form( inform(already_stopped), "Videon �r redan i stoppl�ge." ).
output_form( inform(now_stopped), "Videon har nu stoppats." ).
output_form( inform(already_ff), "Videon �r redan p� fram�tspolning." ).
output_form( inform(now_ff), "Videon spolar nu fram�t." ).
output_form( inform(already_rewinding), "Videon �r redan p� bak�tspolning." ).
output_form( inform(now_rewinding), "Videon spolar nu bak�t." ).
output_form( inform(already_paused), "Videon �r redan i pausl�ge." ).
output_form( inform(now_paused), "Videon �r nu pausad." ).
output_form( inform(already_recording), "Videon spelar redan in." ).
output_form( inform(now_recording), "Videon spelar nu in." ).
output_form( inform(advanced), "Okej." ).
output_form( inform(not_paused), "Videon �r inte i pausl�ge." ).
output_form( inform(no_available_program_slot), "Det finns ingen tillg�nglig programplats." ).
output_form( inform(clock_was_set), "Klockan har st�llts in." ).
output_form( inform(date_cannot_be_set),
	     "Tyv�rr kan inte datum st�llas in i denna version." ).


output_form( inform(ok_new_program_position(P)), Cs ) :-
	format_to_chars( 'Bytt till kanal ~a.\n', [P], Cs ).

output_form( inform(program_added(Position,Date,Start,Stop)), Cs ) :-
	date_output(Date,DateOutput),
	( atom_chars(Start,[Start1,Start2,Start3,Start4]);
	    ( atom_chars(Start,[Start2,Start3,Start4]), Start1 = 48 ) ) ,
	( atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]);
	    ( atom_chars(Stop,[Stop2,Stop3,Stop4]), Stop1 = 48 ) ),
	format_to_chars( 'Okej, jag spelar in kanal ~a ~a fr�n ~c~c:~c~c till ~c~c:~c~c.',
			 [Position,DateOutput,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], Cs ).

output_form( inform(programs(Programs)), Cs ) :-
	program_listing( Programs, Listing ),
	append( "Vilket programnummer vill du radera?", Listing, Cs ).

output_form( inform(no_program_to_delete), "Det finns inget program att radera." ).
output_form( inform(program_deleted), "Programmet har raderats." ).
output_form( inform(no_such_program), "Ingen program finns sparat i den positionen." ).

output_form( inform(confirm_status(S)), Cs ) :-
	status_output(S,SO),
	format_to_chars( 'Ja, videon �r i l�ge ~a.', [SO], Cs ).

output_form( inform(status_is(S)), Cs ) :-
	status_output(S,SO),
	format_to_chars( 'Nej, videon �r i l�ge ~a.', [SO], Cs ).

output_form( inform(current_program_position(P)), Cs ) :-
	format_to_chars( 'Nuvarande kanal �r ~a.', [P], Cs ).

output_form( inform(answering_machine_already_on),
	     "Telefonsvararen �r redan p�." ).
output_form( inform(answering_machine_now_on),
	     "Telefonsvararen �r nu p�." ).
output_form( inform(answering_machine_already_off),
	     "Telefonsvararen �r redan av." ).
output_form( inform(answering_machine_now_off),
	     "Telefonsvararen �r nu av." ).
output_form( inform(autoanswer_already_on),
	     "Autosvar �r redan aktiverat." ).
output_form( inform(autoanswer_now_on),
	     "Autosvar �r nu aktiverat." ).
output_form( inform(autoanswer_already_off),
	     "Autosvar �r redan av." ).
output_form( inform(autoanswer_now_off),
	     "Autosvar �r nu av." ).
output_form( inform(ring_volume_was_set),
	     "Ringvolymen �r nu inst�lld." ).
output_form( inform(earpiece_volume_was_set),
	     "H�rlurens volym �r nu inst�lld." ).
output_form( inform(new_earpiece_volume(V)), Cs ) :-
	format_to_chars( 'H�rlurens volym �r inst�lld p� ~a.\n', [V], Cs ).
output_form( inform(new_ring_volume(V)), Cs ) :-
	format_to_chars( 'Handtelefonens ringvolym �r nu ~a.\n', [V], Cs ).

output_form( inform(base_station_language_was_set(L)), Cs ) :-
	synset(SynSet, L),
	member([Syn],SynSet),
	capitalize(Syn,LS),
	format_to_chars('Basstationens spr�k �r nu ~s.', [LS], Cs ).
output_form( inform(handset_language_was_set(L)), Cs ) :-
	synset(SynSet, L),
	member([Syn],SynSet),
	capitalize(Syn,LS),
	format_to_chars('Handtelefonens spr�k �r nu ~s.', [LS], Cs ).
output_form( inform(signal_was_set(SignalType,ToneOrMelody)), Cs ) :-
	%signal_type_string(SignalType,SignalTypeCs),
	%tone_or_melody
	move_string( signal_type(SignalType), SignalTypeCs ),
	move_string( tone_or_melody(ToneOrMelody), ToneOrMelodyCs ),
	format_to_chars('~s samtal kommer att anv�nda ~s.',[SignalTypeCs,ToneOrMelodyCs],Cs).

output_form( inform(phonebook_entry_added(Name)), String ) :-
	atom_chars( Name, NameString ),
	append( NameString, " har lagts till telefonboken.", String).
output_form( inform(phonebook_entry_deleted(Name)), String ) :-
	atom_chars( Name, NameString ),
	append( NameString, " har raderats fr�n telefonboken.", String).
output_form( inform(phonebook_entry_exists(Name)), String ) :-
	atom_chars( Name, NameString ),
	append( NameString, " finns redan i telefonboken.", String).
output_form( inform(phonebook_entry_not_found), "Jag kunde inte hitta det namnet." ).
output_form( inform(now_calling(Name)), Cs ) :-
	format_to_chars( 'Jag ringer nu ~a.', [Name], Cs ).


% silent confirmation of queries and change domain
output_form( inform(task(top)), "Okej, toppniv�n." ):-!.
output_form( inform(task(vcr_query)), "" ):-!.
output_form( inform(task(vcr_query_channel)), "" ):-!.
output_form( inform(task(vcr_query_status)), "" ):-!.
output_form( inform(task(main_menu)), "" ):-!.

% confirmation 
output_form( inform(task(T)), Str ):-
	move_string( task(T), TaskStr ),
	append( "Okej, ", TaskStr, Str0 ),
	append( Str0, ".", Str ).
% in case there is no input form, use output form
output_form( inform(task(T)), Str ):-
	output_form( task(T), TaskStr ),
	append( "Okej, ", TaskStr, Str0 ),
	append( Str0, ".", Str ).
%output_form( task(top), "toppniv�n" ).
output_form( inform(task(T)), "" ).

output_form( greet, "V�lkommen till telefonen och videon!" ).
output_form( quit, "Hej d�!" ).
output_form( reqRep(understanding), "Urs�kta?" ).
output_form( reqRep(relevance), "Vad menar du?" ).
output_form( thank, "Tack s� mycket." ).
output_form( confirm, "Okej" ).

% repeating a move

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).

date_output(today,idag) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).

status_output(ff,'fram�tspolning') :- !.
status_output(stopped,'stoppad') :- !.
status_output(rewind,'bak�tspolning') :- !.
status_output(paused,'pausad') :- !.
status_output(playing,'uppspelning') :- !.
status_output(S,S).

altlist2altstr_and( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " och ", Str1, Str ).
altlist2altstr_and( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_and( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

altlist2altstr_or( [D], Str ):-
	alt2altstr( D, Str1 ),
	append( " eller ", Str1, Str ).
altlist2altstr_or( [D|Ds], Str ):-
	alt2altstr( D, Str1 ),
	altlist2altstr_or( Ds, Str2 ),
	append( Str1, ", ", Str3 ),
	append(Str3, Str2, Str ).

alt2altstr( D, Str ):-
	output_form( D, Str ).

alt2altstr( D, Str ):-
	name( D, Str ).

program_listing( [], "" ).

program_listing( [N:(Position,Date,Start,Stop)|Programs], Listing ) :-
	atom_chars(Date,[Date1,Date2,Date3,Date4]),
	( atom_chars(Start,[Start1,Start2,Start3,Start4]);
	    atom_chars(Start,[Start2,Start3,Start4]), Start1=48),
	( atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]);
	    atom_chars(Stop,[Stop2,Stop3,Stop4]), Stop1=48 ),
	%atom_chars(Start,[Start1,Start2,Start3,Start4]),
	%atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'Nummer ~d: kanal ~a, ~c~c/~c~c, fr�n ~c~c:~c~c till ~c~c:~c~c; ',
			 [N,Position,
			  Date1,Date2,Date3,Date4,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], This ),
	program_listing( Programs, Rest ),
	append( This, Rest, Listing ).

capitalize(A,[CC|Cs]) :-
	atom_chars(A,[C|Cs]),
	cap_char(C,CC).

cap_char(A,B) :-
	A >= 0'a,
	A =< 0'z,
	!,
	B is A - 32.

cap_char(A,A).

move_string(Move,String) :-
	input_form( Words, answer(Move) ),
	concat_words( Words, String ).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [english], answer(changeto(english)) ).
input_form( [inglish], answer(changeto(english)) ).

output_form( inform(current_language(svenska)), "Okej, pratar nu svenska. Urs�kta min darriga r�st.").

% VCR

input_form( [video],             answer(task(vcr_top)) ).
input_form( [videon],             answer(task(vcr_top)) ).
input_form( [spelningsl�ge],     answer(task(vcr_change_play_status)) ).
input_form( [tidsinst�lld,inspelning], answer(task(vcr_timer_recording)) ).
input_form( [l�gga,till,ett,program],   answer(task(vcr_add_program)) ).
input_form( [l�gga,till,program],     answer(task(vcr_add_program)) ).
input_form( [l�gg,till,ett,program],   answer(task(vcr_add_program)) ).
input_form( [l�gg,till,program],     answer(task(vcr_add_program)) ).
input_form( [radera,ett,program],  answer(task(vcr_delete_program)) ).
input_form( [radera,program],answer(task(vcr_delete_program)) ).
input_form( [ta,bort,program],answer(task(vcr_delete_program)) ).
input_form( [ta,bort,ett,program],answer(task(vcr_delete_program)) ).
input_form( [st�ll,klocka],           answer(task(vcr_set_clock)) ).
input_form( [st�ll,klockan],           answer(task(vcr_set_clock)) ).
input_form( [s�tt,klocka],           answer(task(vcr_set_clock)) ).
input_form( [s�tt,klockan],           answer(task(vcr_set_clock)) ).
input_form( [�ndra,kanal],  answer(task(vcr_new_program_position)) ).
input_form( [�ndra,program],  answer(task(vcr_new_program_position)) ).
input_form( [byt,kanal],  answer(task(vcr_new_program_position))).% 011217
input_form( [byt,program],  answer(task(vcr_new_program_position))). % 011217
input_form( [�ka,kanal],[answer(task(vcr_new_program_position)),
				answer(incdec(increase))]).
input_form( [minska,kanal],[answer(task(vcr_new_program_position)),
				answer(incdec(decrease))]).
input_form( [n�sta,kanal],    [answer(task(vcr_new_program_position)),
				answer(incdec(increase))]).
input_form( [n�sta,program],    [answer(task(vcr_new_program_position)),
				answer(incdec(increase))]).
input_form( [f�rra,kanalen],[answer(task(vcr_new_program_position)),
				answer(incdec(decrease))]).
input_form( [f�rra,programmet],[answer(task(vcr_new_program_position)),
				answer(incdec(decrease))]).
input_form( [s�rskild,kanal],answer(task(vcr_new_specific_program_position)) ).
input_form( [specifik,kanal],answer(task(vcr_new_specific_program_position)) ).
input_form( [en,viss,kanal],answer(task(vcr_new_specific_program_position)) ).

input_form( [play],            answer(task(vcr_play)) ).
input_form( [spela],            answer(task(vcr_play)) ).

input_form( [stop],            answer(task(vcr_stop)) ).
input_form( [stoppa],            answer(task(vcr_stop)) ).
input_form( [stanna],            answer(task(vcr_stop)) ).
input_form( [stoppl�ge],            answer(task(vcr_stop)) ).

input_form( [forward],         answer(task(vcr_ff)) ).
input_form( [spola,fram�t],         answer(task(vcr_ff)) ).
input_form( [spola, fram],         answer(task(vcr_ff)) ).

input_form( [rewind],          answer(task(vcr_rew)) ).
input_form( [spola,bak�t],          answer(task(vcr_rew)) ).
input_form( [spola,tillbaka],          answer(task(vcr_rew)) ).
input_form( [spola,tillbaks],          answer(task(vcr_rew)) ).
input_form( [spola,bak],          answer(task(vcr_rew)) ).

input_form( [pause],           answer(task(vcr_pause_still)) ).
input_form( [paus],           answer(task(vcr_pause_still)) ).
input_form( [pausa],           answer(task(vcr_pause_still)) ).

input_form( [advance],         answer(task(vcr_still_adv)) ). %% ...

input_form( [spela,in],          answer(task(vcr_rec)) ).
input_form( [inspelning],          answer(task(vcr_rec)) ).

input_form( [idag],           answer(date_to_store(today)) ).
input_form( [spelar],         answer(status(playing)) ).
input_form( [pausad],          answer(status(paused)) ).
input_form( [stoppad],         answer(status(stopped)) ).
input_form( [spolar,tillbaka],       answer(status(rewinding)) ).
input_form( [spolar,fram�t],      answer(status(ff)) ).
input_form( [spelar,in],       answer(status(recording)) ).
input_form( [fr�ga],           answer(task(vcr_query)) ).
input_form( [vilken,kanal],    answer(task(vcr_query_channel)) ).
input_form( [vilket,spelningsl�ge],    answer(task(vcr_query_status)) ).

% "komplexa" fr�gor
input_form( [spolar,videon,fram�t],    [answer(task(vcr_query)), answer(status(ff))] ).
input_form( [spolar,videon,tillbaka],      [answer(task(vcr_query)), answer(status(rewinding))] ).
input_form( [spelar,videon,in],      [answer(task(vcr_query)), answer(status(recording)) ] ).




input_form( [fr�n|L], answer(start_time_to_store(T)) ) :-
	input_form( L, answer(number(T)) ).

input_form( [till|L], answer(stop_time_to_store(T)) ) :-
	input_form( L, answer(number(T)) ).

input_form( [�ndra,till,PrCh|L], answer(new_program_position(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).

input_form( [byt,till,PrCh|L], answer(new_program_position(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).

input_form( [spela,in,PrCh|L], answer(program_position_to_store(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).


input_form( [PrCh|L], answer(program_position(P)) ) :-
	program_word(PrCh),
	input_form( L, answer(number(P)) ).

input_form( [klockan,p�|L], answer(new_clock(C)) ) :-
	input_form( L, answer(number(C)) ).

% Telephone

input_form( [telefon],            answer(task(tp_top)) ).
input_form( [telefonen],            answer(task(tp_top)) ).
input_form( [basstation],         answer(task(tp_base_station)) ).
input_form( [basstationen],         answer(task(tp_base_station)) ).
input_form( [telefonsvarare],    answer(task(tp_answering_machine)) ).
input_form( [telefonsvararen],    answer(task(tp_answering_machine)) ).
input_form( [basstationsinst�llningar],answer(task(tp_base_station_settings)) ).
input_form( [basstationsinst�llningarna],answer(task(tp_base_station_settings)) ).
input_form( [basstationens,inst�llningar],answer(task(tp_base_station_settings)) ).
input_form( [basstationsvolym],  answer(task(tp_base_station_settings_volume)) ).
input_form( [basstationsvolymen],  answer(task(tp_base_station_settings_volume)) ).
input_form( [basstationens,volym],  answer(task(tp_base_station_settings_volume)) ).
input_form( [volym],               answer(task_type(volume)) ).
input_form( [basinst�llning],
	    answer(task(tp_base_station_settings_basic)) ).
input_form( [basinst�llningar],
	    answer(task(tp_base_station_settings_basic)) ).
input_form( [basinst�llningarna],
	    answer(task(tp_base_station_settings_basic)) ).
input_form( [basinst�llningen],
	    answer(task(tp_base_station_settings_basic)) ).
input_form( [datum], answer(task(tp_base_station_settings_basic_date)) ).
input_form( [basstationsspr�k],answer(task(tp_base_station_settings_basic_language)) ).
input_form( [spr�k],             answer(task_type(language)) ).

input_form([W], answer(language(L)) ) :-
        lexsem([W],L),
        vcr_language(L).
%input_form( [L],                    answer(language(L)) ) :-
	%vcr_language(L).
input_form( [autosvar],
	    answer(task(tp_base_station_settings_autoanswer)) ).
input_form( [handtelefon],              answer(task(tp_handset)) ).
input_form( [handtelefonen],              answer(task(tp_handset)) ).
input_form( [handtelefonens,spr�k],     answer(task(tp_handset_language)) ).
input_form( [h�rlurens,volym],      answer(task(tp_handset_earpiece_volume)) ).
input_form( [telefonbok],            answer(task(tp_phonebook)) ).
input_form( [telefonboken],           answer(task(tp_phonebook)) ).
input_form( [s�ka,i,telefonboken], answer(task(tp_phonebook_search_entry)) ).
input_form( [s�ka,i,telefonbok],answer(task(tp_phonebook_search_entry)) ).
input_form( [s�k],           answer(task(tp_phonebook_search_entry)) ).
input_form( [s�k,efter],           answer(task(tp_phonebook_search_entry)) ).
input_form( [s�ka,efter],           answer(task(tp_phonebook_search_entry)) ).
input_form( [vilket, telefonnummer], answer(task(tp_phonebook_search_entry)) ).%011217
input_form( [vilket, nummer], answer(task(tp_phonebook_search_entry)) ).%011217
input_form( [vad, har, Name, f�r, nummer], [answer(task(tp_phonebook_search_entry))|[NameAns]] ):-input_form([Name],NameAns).%011217


input_form( [g�ra,till�gg,i,telefonboken],      answer(task(tp_phonebook_new_entry)) ).
input_form( [till�gg],        answer(task(tp_phonebook_new_entry)) ).
input_form( [l�gg, till],      answer(task(tp_phonebook_new_entry)) ).

input_form( [radera,n�got,i,telefonboken],      answer(task(tp_phonebook_delete_entry)) ).
input_form( [radera,i,telefonboken],         answer(task(tp_phonebook_delete_entry)) ).
input_form( [ta,bort,Name,i,telefonboken], [answer(task(tp_phonebook_delete_entry))|NameAns] ):- input_form([Name],NameAns).
input_form( [ta,bort,Name,fr�n,telefonboken], [answer(task(tp_phonebook_delete_entry))|NameAns] ):- input_form([Name],NameAns).

input_form( [ringsignaler,och,volym], answer(task(tp_handset_warnings)) ).
input_form( [ringsignaler],       answer(task(tp_handset_warnings_signals)) ).
input_form( [ringsignalens,volym],  answer(task(tp_handset_warnings_volume)) ).
input_form( [l�g],                  answer(tone_or_melody(low)) ).
input_form( [medium],               answer(tone_or_melody(medium)) ).
input_form( [h�g],                 answer(tone_or_melody(high)) ).
input_form( [blandad],                answer(tone_or_melody(mixed)) ).
input_form( [hoppig],                answer(tone_or_melody(jumpy)) ).
input_form( [nachtmusik],           answer(tone_or_melody(eine_kleine_nachtmusik)) ).
input_form( [toccata],              answer(tone_or_melody(toccata)) ).
input_form( [elise],                answer(tone_or_melody(elise)) ).
input_form( [samba],                answer(tone_or_melody(samba)) ).
input_form( [blues,rhythm],          answer(tone_or_melody(blues_rythm)) ).
input_form( [intern],             answer(signal_type(internal)) ) .
input_form( [extern],             answer(signal_type(external)) ) .
input_form( [meddelandesignal],              answer(signal_type(message)) ) .
input_form( [s�ksignal],               answer(signal_type(search_signal)) ) .

input_form( [l�gg,till|L], answer(phonebook_name_to_add(N)) ) :-
	input_form( L, answer(name(N)) ).

input_form( [radera|L], answer(phonebook_entry_to_delete(N)) ) :-
	input_form( L, answer(name(N)) ).

input_form( [ring|L], answer(phonebook_name_to_find(N)) ) :-
	%call_word(CallDial),
	input_form( L, answer(name(N)) ).

%input_form( [call],                 answer(call) ).

input_form( [Name], answer(name(Name)) ) :-
	lex_name(Name).

% Common

input_form( [inst�llningar],        answer(task_type(settings)) ).
input_form( [dom�n],          answer(task(main_menu))).
input_form( [st�ng,p�],   answer(action(switch_on)) ).
input_form( [st�ng,av],  answer(action(switch_off)) ).
input_form( [s�tt,p�],    answer(action(switch_on)) ).
input_form( [s�tt,av],   answer(action(switch_off)) ).
input_form( [aktivera],          answer(onoff(on)) ).
input_form( [avaktivera],         answer(onoff(off)) ).
input_form( [�ka],        answer(incdec(increase)) ).
input_form( [minska],        answer(incdec(decrease)) ).

%input_form( S, M ) :-
%	lexsem( S, C ),
%	input_move( C, M ).

% numbers

%input_form(['Num'],answer(number(N))).

input_form( S, answer(number(N)) ) :-
	number_form( S, N ).

% simple stuff

input_form( [hej], greet ).
input_form( [goddag], greet ).
input_form( [hejsan], greet ).
input_form( [hej,d�], quit ).
input_form( [adj�], quit ).
input_form( [sluta], quit ).
input_form( [vad,sa,du], reqRep ).
input_form( [vad,sade,du], reqRep ).
input_form( ['va?'], reqRep ).
input_form( ['vad?'], reqRep ).
input_form( [va], reqRep ).
input_form( [vad], reqRep ).
input_form( [f�rl�t], reqRep ).
input_form( [urs�kta], reqRep ).
input_form( [ja], answer(yes) ).
input_form( [jo], answer(yes) ).
input_form( [japp], answer(yes) ).
input_form( [nej], answer(no) ).
input_form( [n�], answer(no) ).
input_form( [okej], ack ).
input_form( [ok], ack ).
input_form( [aha], ack ).

program_word(program).
program_word(kanal).

%input_form2(_,_,_):-fail.
%digit_word(_,_):-fail.

%call_word(call).
%call_word(dial).

/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

synset( [[video],[videon]], vcr ).
synset( [[telefon],[telefonen]], telephone ).
synset([[svenska]],swedish).
synset([[engelska]],english).
synset([[tyska]],german).
synset([[spanska]],spanish).
synset([[franska]],french).

% yes or no answer

yn_answer(A):-
	A = 'yes';
	A = 'no'.

% from concept to string (pick the first synonym)

concept_string( Concept, String ) :-
	synset( [Words|_], Concept ),
	concat_words( Words, String ).

concat_words( [Word], String ) :-
	atom_chars( Word, String ).

concat_words( [Word|Words], String ) :-
	atom_chars( Word, S1 ),
	concat_words( Words, S2 ),
	append( S1, [ 0' | S2 ], String ).

/*----------------------------------------------------------------------
     - Domain concepts
----------------------------------------------------------------------*/

task(T) :-
	domain_vcrphone:task(T).

%language(L) :-
%	domain_vcrphone:language(L).

sublist( SubList, List ) :-
	append( _, Suffix, List ),
	append( SubList, _, Suffix ).

program_position(P) :-
	to_number(P,N),
	integer(N),
	N >= 1,
	N =< 99.

program_number(P) :-
	to_number(P,N),
	integer(N),
	N >= 1,
	N =< 8.

ring_volume(V) :-
	to_number(V,N),
	integer(N),
	N >= 0,
	N =< 7.

earpiece_volume(V) :-
	to_number(V,N),
	integer(N),
	N >= 0,
	N =< 7.

date(today).

date(D) :-
	number_atom(D),
	atom_chars(D,[D1,D2,D3,D4]),
	number_chars(Day,[D1,D2]),
	Day >= 1,
	Day =< 31,
	number_chars(Month,[D3,D4]),
	Month >= 1,
	Month =< 12.

time(T) :-
	number_atom(T),
	atom_chars(T,[T1,T2,T3,T4]),
	number_chars(Hour,[T1,T2]),
	Hour >= 0,
	Hour =< 23,
	number_chars(Min,[T3,T4]),
	Min >= 0,
	Min =< 59.
time(T) :-
	number_atom(T),
	atom_chars(T,[T2,T3,T4]),
	T1=48,
	number_chars(Hour,[T1,T2]),
	Hour >= 0,
	Hour =< 23,
	number_chars(Min,[T3,T4]),
	Min >= 0,
	Min =< 59.

to_number(Atom,Number) :-
	number_atom(Atom),
	atom_chars(Atom,Cs),
	number_chars(Number,Cs).

number_atom(A) :-
	atomic(A),
	\+ number(A).






