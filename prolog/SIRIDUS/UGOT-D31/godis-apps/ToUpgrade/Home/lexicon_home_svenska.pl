

:- module( lexicon_home_svenska, [output_form/2, input_form/2,
				   input_form2/3, digit_word/2,
				  yn_answer/1]).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

input_form( [english], answer(changeto(english)) ).
input_form( [inglish], answer(changeto(english)) ).
output_form( inform(current_language(svenska)), "Okej, pratar nu svenska. Urs�kta min darriga r�st.").


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

output_form( ask(X^(device(X))), "Vilken sak �r det du pratar om?" ).
output_form( ask(X^(device_type(X))), "Vilken sak �r det du pratar om?" ).
output_form( ask(X^(action(X))), "Vad vill du g�ra?" ).
output_form( ask(X^(device_state(X))), "Vad vill du veta?" ).
output_form( ask(X^(location(X))), "Var?" ).
output_form( ask(main_menu), "Vill du byta dom�n?" ).

output_form( ask(light(X)), Str ) :-
%	atom_chars(X,XS),  % SE �ndrade till concept_string f�r svenska
	concept_string(X,XS),
	append( "Vill du t�nda lyset i ", XS, Str0 ),
	append( Str0, "?", Str ).

output_form( ask(leave_on_light),
	     "Vill du l�ta ljuset vara t�nt i n�got av rummen?" ).

output_form( task(perform_specific_action), "�ndra ljuset i n�got av rummen" ).
output_form( task(make_query), "st�lla en fr�ga").
output_form( task(main_menu), "byta dom�n" ).
output_form( task(leaving), "g� ut" ).

output_form( inform(device_already_switched_on(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r redan t�nt.", [LS], Cs ).
output_form( inform(device_already_switched_off(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r redan sl�ckt.", [LS], Cs ).
output_form( inform(light_already_dimmed(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r redan dimmat.", [LS], Cs ).

output_form( inform(no_lamp_in(_)),
	     "Det finns ingen lampa d�r." ).
output_form( inform(no_dimmer_in(_)),
	     "Det finns ingen dimmer d�r." ).

output_form( inform(is_switched_onoff(OnOff,S,L)), Cs ) :-
	concept_string( L, LS ),
	( ( ( OnOff = on,  S = switched_on ) ;
	    ( OnOff = off, S = switched_off ) ) ->
	    CR = "Ja" ; CR = "Nej" ),
	concept_string( OnOff, OnOffS ),
	format_to_chars( "~s, ljuset i ~s �r ~s.", [CR,LS,OnOffS], Cs ).

output_form( inform(is_dimmed(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r dimmat.", [LS], Cs ).
output_form( inform(is_not_dimmed(_,L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r inte dimmat.", [LS], Cs ).

output_form( inform(bad_action(lamp)),
	     "Man kan inte g�ra s� med en lampa." ).
output_form( inform(bad_action(dimmer)),
	     "Man kan inte g�ra s� med en dimmer." ).

output_form( inform(switched_on_lamp(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r nu t�nt.", [LS], Cs ).
output_form( inform(switched_off_lamp(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r nu sl�ckt.", [LS], Cs ).
output_form( inform(dimmed_light(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Ljuset i ~s �r nu dimmat.", [LS], Cs ).

output_form( inform(woken_up_lit_kitchen),
	     "�ndrat till morgonl�ge. Alla lampor har nu t�nts." ).
output_form( inform(woken_up_no_lit_kitchen),
	     "�ndrat till morgonl�ge. Alla lampor utom den i k�ket har t�nts." ).

output_form( inform(closed_down_except(L)), Cs ) :-
	concept_string( L, LS ),
	format_to_chars( "Alla lampor f�rutom den i ~s har sl�ckts.", [LS], Cs ).
output_form( inform(closed_down_all),
	     "Alla lampor �r nu sl�ckta." ).

% silent confirmation of queries and change domain
output_form( inform(task(make_query)), "" ):-!.
output_form( inform(task(main_menu)), "" ):-!.
output_form( inform(task(top)), "" ):-!.
%output_form( inform(task(top)), "Okej, toppniv�n." ).

%output_form( inform(task(T)), "Okej." ).
output_form( inform(task(T)), Str ):-
	move_string( task(T), TaskStr ),
	append( "Okej, ", TaskStr, Str0 ),
	append( Str0, ".", Str ).
output_form( inform(task(T)), "" ).
%output_form( task(wake_up), "morgonl�ge" ).

move_string(Answer,String) :-
	input_form( Words, answer(Answer) ),
	concat_words( Words, String ).
concat_words( [Word], String ) :-
	atom_chars( Word, String ).
concat_words( [Word|Words], String ) :-
	atom_chars( Word, S1 ),
	concat_words( Words, S2 ),
	append( S1, [ 0' | S2 ], String ).

%output_form( greet, "Welcome to the home device manager!" ).
output_form( greet, "V�lkommen till hemassistenten!" ).
output_form( quit, "Hej d�!" ).
output_form( reqRep(understanding), "Urs�kta?" ).
output_form( reqRep(relevance), "Vad menar du?" ).
output_form( thank, "Tack s� mycket." ).
output_form( confirm, "Okej" ).

% repeating a move

output_form( repeat(Move), Str ):-
	output_form( Move, Str ).


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

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

input_form( [t�nd],      answer(action(switch_on)) ).
input_form( [sl�ck],     answer(action(switch_off)) ).
input_form( [st�ng,p�],   answer(action(switch_on)) ).
input_form( [st�ng,av],  answer(action(switch_off)) ).
input_form( [s�tt,p�],    answer(action(switch_on)) ).
input_form( [s�tt,av],   answer(action(switch_off)) ).
input_form( [dimma,ljuset],answer(action(dim_light)) ).
input_form( [dimma,lampan],       answer(action(dim_light)) ).
input_form( [dimma],answer(action(dim_light)) ).

input_form( [t�nt],    answer(device_state(switched_on)) ).

input_form( [�r,Dev,i,Loc,t�nd],    [answer(task(make_query)), answer(device_state(switched_on))|[DevAns|[LocAns]]] ):- input_form([Dev], DevAns), input_form([Loc], LocAns).
input_form( [�r,Dev,t�nd,i,Loc],    [answer(task(make_query)), answer(device_state(switched_on))|[DevAns|[LocAns]]] ):- input_form([Dev], DevAns), input_form([Loc], LocAns).
input_form( [�r,Dev,t�nd],    [answer(task(make_query)), answer(device_state(switched_on))|[DevAns]] ):- input_form([Dev], DevAns).

input_form( [sl�ckt],   answer(device_state(switched_off)) ).
%input_form( [t�nd],    answer(device_state(switched_on)) ).
input_form( [p�],  answer(device_state(switched_on)) ).
input_form( [av], answer(device_state(switched_off)) ).
input_form( [dimmat],       answer(device_state(dimmed)) ).
input_form( [dimmad],       answer(device_state(dimmed)) ).

input_form( [byt, dom�n],       answer(task(main_menu)) ).
input_form( [�ndra, dom�n],       answer(task(main_menu)) ).
input_form( [�ndra],       answer(task(perform_specific_action)) ).
input_form( [byt],       answer(task(perform_specific_action)) ).
input_form( [vakna],      answer(task(wake_up)) ).
input_form( [god,morgon], answer(task(wake_up)) ).
input_form( [morgonl�ge], answer(task(wake_up)) ).
input_form( [g�r,ut],    answer(task(leaving)) ).
input_form( [g�,ut],    answer(task(leaving)) ).
input_form( [g�r],      answer(task(leaving)) ).
input_form( [g�],      answer(task(leaving)) ).
input_form( [fr�ga],        answer(task(make_query)) ).
input_form( [�r],       answer(task(make_query)) ).
input_form( [l�t,ljuset,vara,p�], answer(leave_on_light) ).
input_form( [l�t,ljuset,vara,t�nt], answer(leave_on_light) ).
input_form( [dom�n],       answer(task(main_menu))).

input_form( S, M ) :-
	lexsem( S, C ),
	input_move( C, M ).

% simple stuff

input_form( [hej], greet ).
input_form( [hejsan], greet ).
input_form( [hej,d�], quit ).
input_form( [adj�], quit ).
input_form( [stopp], quit ).
input_form( [sluta], quit ).
input_form( [vad,sa,du], reqRep ).
input_form( [vad,sade,du], reqRep ).
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

% concepts

input_move( L, answer(location(L)) ) :-
	location( L ).

input_move( T, answer(device_type(T)) ) :-
	device_type( T ).

input_form2(_,_,_):-fail.
digit_word(_,_):-fail.
/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

%synset( [[lamp], [light]], lamp ).
%synset( [[lampa], [lampan], [ljus], [ljuset],[lyse],[lyset]], lamp ).
synset( [[lampa], [lampan]], lamp ).
synset( [[dimmer], [dimmern]], dimmer ).
synset( [[k�ket], [k�k]], kitchen ).
synset( [['teve-rummet'],['teve-rum'],['tv-rummet'],['tv-rum'], [teverummet],[teverum],[tvrummet],[tvrum]], tv_room ).
synset( [[vardagsrummet],[vardagsrum]], living_room ).
synset( [[hallen],[hall]], hall ).
synset( [[hobbyrummet],[hobbyrum]], hobby_room ).
synset( [[arbetsrummet],[arbetsrum]], study ).
synset( [[av]], off).
synset( [[p�]], on).

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

action( X ) :- domain_home:action( X ).
task( T ) :- domain_home:task( T ).
location( L ) :- domain_home:location( L ).
device_type( T ) :- domain_home:device_type( T ).

sublist( SubList, List ) :-
	append( _, Suffix, List ),
	append( SubList, _, Suffix ).
