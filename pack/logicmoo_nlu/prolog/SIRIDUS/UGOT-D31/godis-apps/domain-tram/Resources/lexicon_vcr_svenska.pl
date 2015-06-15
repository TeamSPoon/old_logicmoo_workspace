 
/*************************************************************************

         name: lexicon_vcr_svenska.pl 
      version: 
  description: 
       author: Staffan Larsson, David Hjelm
 
*************************************************************************/

:- module( lexicon_vcr_svenska, [resource_of_type/1,
				 output_form/2, input_form/2,
				 yn_answer/1]).

resource_of_type(lexicon).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.



:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_svenska) ).
%:- ensure_loaded( library(lexicon_general_english) ).

:- ensure_loaded( library( semsort_vcr ) ).


%:- multifile( output_form/2 ).
%:- multifile( input_form/2 ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

% ask-moves

output_form( ask(X^(action(X))), ['Vad kan jag hj�lpa dig med?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Vill du '], StrT, Str0 ),
	append( Str0, ['?'], Str).


output_form( ask(X^new_channel(X)),
	     ['Vilken kanal vill du byta till?'] ).

output_form( ask(X^channel_to_store(X)),
	     ['Vilken kanal vill du spela in?'] ).
output_form( ask(X^date_to_store(X)),
	     ['Vilket datum?'] ).
output_form( ask(X^start_time_to_store(X)),
	     ['Vilken tid ska inspelningen starta?'] ).
output_form( ask(X^stop_time_to_store(X)),
	     ['Vilken tid ska inspelningen sluta?'] ).

output_form( ask(X^program_to_delete(X)),
	     ['Vilket programnummer vill du radera?'] ).

output_form( ask(X^new_clock(X)),
	     ['Vilken tid?'] ).

% action
output_form( action(up), ['g� upp'] ).
output_form( action(change_language), ['byta spr�k'] ).
output_form( action(top), ['toppniv�'] ).
output_form( action(top), ['g� till videon'] ).
output_form( action(vcr_change_play_status), ['�ndra spelningsl�ge'] ).
output_form( action(vcr_timer_recording), ['g� till tidsinst�lld inspelning']).
output_form( action(vcr_settings), ['g� till inst�llningar'] ).
%output_form( action(vcr_query), ['make a query'] ).
output_form( action(vcr_new_channel), ['byta kanal'] ).
output_form( action(vcr_play), ['spela videon'] ).
output_form( action(vcr_stop), ['stoppa'] ).
output_form( action(vcr_ff), ['spola fram'] ).
output_form( action(vcr_rew), ['spola bak�t'] ).
output_form( action(vcr_pause_still), ['pausa'] ).
output_form( action(vcr_still_adv), ['spola fram l�ngsamt'] ).
output_form( action(vcr_rec), ['spela in'] ).
output_form( action(vcr_add_program), ['l�gga till ett program'] ).
output_form( action(vcr_add_program1), ['l�gga till ett program'] ).
output_form( action(vcr_delete_program), ['ta bort ett program'] ).
output_form( action(vcr_delete_program1), ['ta bort ett program'] ).
output_form( action(vcr_set_clock), ['st�lla in klockan'] ).
%output_form( action(vcr_query_status), ['query the current play status'] ).
%output_form( action(vcr_query_channel), ['find out what channel is on'] ).



output_form( icm:acc*neg:action(vcr_play,already), ['Videon spelar redan.'] ).
output_form( confirm(vcr_play), ['Nu spelar videon.'] ).
output_form( icm:acc*neg:action(vcr_stop, already), ['Videon �r redan stoppad'] ).
output_form( confirm(vcr_stop), ['Nu �r videon stoppad.'] ).
output_form( icm:acc*neg:action(vcr_ff, already), ['Videon �r redan p� fram�tspolning.'] ).
output_form( confirm(vcr_ff), ['Nu spolar videon fram.'] ).
output_form( icm:acc*neg:action(vcr_rewind, already), ['Videon �r redan p� bak�tspolning'] ).
output_form( confirm(vcr_rew), ['Nu spolar videon bak�t.'] ).
output_form( icm:acc*neg:action(vcr_pause_still, already), ['Videon �r redan pausad.'] ).
output_form( confirm(vcr_pause_still), ['Nu �r videon pausad.'] ).
output_form( icm:acc*neg:action(vcr_rec, already), ['Videon spelar redan in.'] ).
output_form( confirm(vcr_rec), ['Nu spelar videon in.'] ).

output_form( confirm(vcr_still_adv), ['Videon spelar nu fram l�ngsamt.'] ).%??
output_form( answer(not(vcr_paused)), ['Videon �r inte pausad.'] ).
output_form( icm:acc*neg:action(vcr_add_program,no_available_program_slot), ['There is no available program slot.'] ).
output_form( confirm(vcr_set_clock), ['Klockan har st�llts in.'] ).

output_form( confirm(vcr_change_channel(P)), Cs ) :-
	format_to_chars( 'Ockej. Bytt till kanal ~a.\n', [P], Cs ).
output_form( confirm(vcr_new_channel), ['Nu har kanal bytts'] ). %huff

output_form( confirm(vcr_add_program(Position,Date,Start,Stop)), Cs ) :-
	date_output(Date,DateOutput),
	atom_chars(Start,[Start1,Start2,Start3,Start4]),
	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'Ockej, spelar in kanal ~a ~a fr�n ~c~c:~c~c till ~c~c:~c~c.',
			 [Position,DateOutput,
			  Start1,Start2,Start3,Start4,
			  Stop1,Stop2,Stop3,Stop4], Cs ).

output_form( confirm(vcr_add_program), ['Programmet har lagts till.'] ).

output_form( answer(programs(Programs)), Cs ) :-
	program_listing( Programs, Listing ),
	append( 'Nuvarande program �r\n', Listing, Cs ).

output_form( ask(X^program_to_delete(X)), ['Vilket programnummer vill du radera?'] ).

output_form( icm:acc*neg:action( vcr_delete_program, no_program_to_delete ), ['Det finns inget program att radera.'] ).
output_form( confirm(vcr_delete_program), ['Ockej. Programmet har raderats.'] ).
%output_form( report( 'DeleteProgram', failed(no_such_program)), ['No program is stored in that position.'] ).

output_form( icm:acc*neg:program_to_delete(_), ['Det finns inget program lagrat p� den positionen.'] ).



output_form( icm:acc*neg:new_channel(_), ['Ledsen, Tillg�ngliga kanaler �r 1 till 99.'] ).

output_form( answer(play_status(S)), ['Videon',SO] ) :-
	status_output(S,SO).

output_form( answer(not(play_status(S))), ['Nej, videon' , S0] ):-
	neg_status_output(S,S0).

%output_form( answer(not(play_statusstatus(S))), Cs ) :-
%	status_output(S,SO),
%	format_to_chars( 'The VCR is not ~a.', [SO], Cs ).

%output_form( answer(current_channel(P)), Cs ) :-
%	format_to_chars( 'The current channel is ~a.', [P], Cs ).
output_form( answer(current_channel(P)), ['Nuvarande kanal �r', P ] ).



output_form( greet, ['V�lkommen till videon!'] ).
output_form( quit, ['Hej d�!'] ).


output_form( new_clock(T), [ S1, S2 ] ):-
	input_form( [ S1, S2 ], answer(time(T)) ).


output_form( ask(X^language(X)), ['Vilket spr�k vill du anv�nda?'] ).
output_form( ask([language(_)|_]), ['Vill du anv�nda engelska eller svenska?'] ). % HACK

date_output(today,today) :- !.

date_output(Date,DateOutput) :-
	atom_chars(Date,Chars),
	format_to_chars('~c~c/~c~c',Chars,DateOutputCs),
	atom_chars(DateOutput,DateOutputCs).

status_output(playing, 'spelar').
status_output(stopped, '�r stoppad').
status_output(ff,'spolar fram�t').
status_output(rewinding,'spolar bak�t').
status_output(paused,'�r pausad').
status_output(recording,'spelar in').

neg_status_output(playing, 'spelar inte').
neg_status_output(stopped, '�r inte stoppad').
neg_status_output(ff,'spolar inte fram�t').
neg_status_output(rewinding,'spolar inte bak�t').
neg_status_output(paused,'�r inte pausad').
neg_status_output(recording,'spelar inte in').


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
	atom_chars(Start,[Start1,Start2,Start3,Start4]),
	atom_chars(Stop,[Stop1,Stop2,Stop3,Stop4]),
	format_to_chars( 'Nummer ~d: position ~a, ~c~c/~c~c, fr�n ~c~c:~c~c till ~c~c:~c~c;\n',
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


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/

% generic

input_form( [g�,upp],           request(up) ).
input_form( [upp],           request(up) ).
input_form( [spr�k],        request(change_language) ).
input_form( [engelska], answer(language(english)) ).
input_form( [svenska], answer(language(svenska)) ).
input_form( [inglish], answer(language(english)) ).


% VCR
input_form( [toppniv�],             request(top) ).
input_form( [spelningsl�ge],     request(vcr_change_play_status) ).
input_form( [tidsinst�lld,inspelning], request(vcr_timer_recording) ).
input_form( [l�gg,till,ett,program],   request(vcr_add_program) ).
input_form( [l�gga,till,program],     request(vcr_add_program) ).
input_form( [l�gg,till,program],   request(vcr_add_program) ).
input_form( [l�gga,till,ett,program],     request(vcr_add_program) ).

input_form( [ta,bort,program],  request(vcr_delete_program) ).
input_form( [ta,bort,ett,program],request(vcr_delete_program) ).
input_form( [ta,bort,ett,program],  request(vcr_delete_program) ).
input_form( [ta,bort,program],request(vcr_delete_program) ).
input_form( [radera,program],  request(vcr_delete_program) ).
input_form( [radera,ett,program],request(vcr_delete_program) ).
input_form( [inst�llningar],        request(vcr_settings) ).
input_form( [st�ll,klockan],           request(vcr_set_clock) ).

input_form( [byt,kanal],  request(vcr_new_channel ) ).
input_form( [�ndra,kanal],  request(vcr_new_channel ) ).
input_form( [byt,till],  request(vcr_new_channel ) ).
%input_form( [specific,channel],request(vcr_new_channel ) ).
input_form( [n�sta,program], [ request(vcr_increase_channel) ] ).
input_form( [n�sta,kanal], [ request(vcr_increase_channel) ] ).
input_form( [f�reg�ende,program], [ request(vcr_decrease_channel ) ] ).
input_form( [f�reg�ende,kanal], [ request(vcr_decrease_channel ) ] ).

input_form( [plej],            request(vcr_play) ).
input_form( [spela],            request(vcr_play) ).
input_form( [stopp],            request(vcr_stop) ).
input_form( [stoppa],            request(vcr_stop) ).
input_form( [spola,fram�t],         request(vcr_ff) ).
input_form( [spola,fram],         request(vcr_ff) ).
input_form( [spola,bak�t],         request(vcr_ff) ).
input_form( [spola,bak],         request(vcr_ff) ).
input_form( [paus],           request(vcr_pause_still) ).
input_form( [pausa],           request(vcr_pause_still) ).

input_form( [spola,fram,l�ngsamt],         request(vcr_still_adv) ).
input_form( [spela,in],          request(vcr_rec) ).

input_form( [idag],           answer(date_to_store(today)) ).

input_form( [spelar,videon],         ask( play_status( playing ) ) ).
input_form( [�r,videon,pausad],          ask( play_status( paused ) ) ).
input_form( [�r,videon,stoppad],         ask( play_status( stopped ) ) ).
input_form( [spolar,videon,bak�t],       ask( play_status( rewinding ) ) ).
input_form( [spolar,videon,fram�t],      ask( play_status( ff ) ) ).
input_form( [spelar,videon,in],       ask( play_status(recording) ) ).
input_form( [vad,g�r,videon],       ask( X^play_status(X) ) ).

input_form( [vilken,kanal],    ask(X^current_channel(X)) ).

input_form( [ fr�n | [ S1, S2 ] ], answer( start_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).

input_form( [ till | [ S1, S2 ] ], answer( stop_time_to_store( C ) ) ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).

%input_form( [ switch, to, channel | S ], answer( new_channel( C ) ) ) :-
%	lexsem( S, C ),
%	sem_sort( C, channel ).

% ???
%input_form( [ record, channel | L ], answer( channel_to_store(P)) ) :-
%	lexsem( S, C ),
%sem_sort( C, channel ).

input_form( [ kanal | S ], answer( channel( C ) ) ) :-
	lexsem( S, C ),
%	sem_sort( C, channel ).
	sem_sort( C, number ).



input_form( [ st�ll, klockan, p� | [ S1, S2 ] ], [ request( vcr_set_clock ), answer( new_clock( C ) ) ] ) :-
	input_form( [ S1, S2 ], answer(time(C)) ).


% time: two numbers in sequence

input_form( [S1,S2] , answer( time( C ) ) ) :-
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	append( C1S, C2S, CS ),
	name( C, CS ),
	sem_sort( C, time ).


% numbers

input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).



/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/



% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', �r det korrekt?'], Output ).



output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
%	append(['Do you mean'|Alt0out], AltsOr, Output0 ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).
altlist2alts_or( [Alt], ['eller'|OutputAlt] ):-
	output_form(Alt, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).

output_form( Alt, OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).



% db entries

output_form( answer( db_entry( _PropList, set(NewPropList), P ) ), [''|Output ] ):-
	output_forms( NewPropList, NewPropListOut ),
	output_form( answer( P ), AnswerOut ),
	append( AnswerOut, NewPropListOut, Output ).

%output_form( answer( db_entry( set( List )  ) ), Output ):-
%	output_forms( List, Output ).

output_forms( [], [] ).
output_forms( [ Move | Moves ], Output1 ):-
	output_form( Move, Output ),
	output_forms( Moves, Outputs ),
	append( Output, Outputs, Output1 ).
	


				%
				%output_form( answer( db_entry( PropList, P ) ), Output ):-
%	output_forms( PropList, PropOutput ),
%	output_form( answer( P ), AnsOutput ),
%	append( PropOutput, ['.'.


output_form( answer(notexist(X,Q)), [' Ledsen, det finns inget som matchar din fr�ga om'|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Ledsen, det finns inget som matchar din fr�ga om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['fr�ga om'|Out] ):-
	input_form( Out, ask( Q ) ).


% for asking metaissue clarification question
%output_form( action(Action), ['to '|Out] ):-
%	input_form( Out, request( Action ) ).

% ICM

% contact
output_form( icm:con*neg, ['Hall�?'] ).


% perception
output_form( icm:per*int, ['Urs�kta?'] ).
output_form( icm:per*int, ['Vad sade du?'] ).
output_form( icm:per*neg, ['Urs�kta, Jag h�rde inte vad du sade.'] ).


output_form( icm:per*pos:String, ['Jag tyckte du sade',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du'] ).
output_form( icm:sem*neg, ['F�rl�t, jag f�rst�r inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).


% understanding(pragmatic)
output_form( icm:und*neg, ['Jag f�rst�r inte riktigt.']  ).


output_form( icm:und*pos:usr*issue(A^play_status(A)),
	     ['Du vill veta vad videon g�r']  ).


output_form( icm:und*pos:usr*issue(play_status(Status)),
	     ['Du vill veta om videon',S0]  ):-
	status_output(Status,S0).

output_form( icm:und*pos:usr*issue(A^current_channel(A)),
	     ['Du vill veta vilken kanal som �r p�.'] ).



output_form( icm:und*pos:usr*(not issue(Q)), ['Du fr�gade inte:'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, AnsPDot  ),
	append( ['inte'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, AnsPDot  ):-
	( output_form(P, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).

% special cases; could make use of isa-hierarchy
%output_form( icm:und*pos:usr*channel_to_store(X), IcmPos  ):-
%	output_form( icm:und*pos:usr*channel(X), IcmPos ).
%output_form( icm:und*pos:usr*new_channel(X), IcmPos  ):-
%	output_form( icm:und*pos:usr*channel(X), IcmPos ).

% 020702 SL
output_form( icm:und*pos:usr*PX, IcmPos ):-
	PX =.. [P,X],
	isa( P, P1 ),
	P1X =.. [P1,X],
	output_form( icm:und*pos:usr*P1X, IcmPos ).



output_form( icm:und*int:usr*C, IcmInt  ):-
	output_form( ask(C), IcmInt ).
	%output_form( icm:und*pos:C, IcmPos ),
	%append( IcmPos0,['.'],IcmPos),
	%append( IcmPos0, [', is that correct?'], IcmInt ).

%output_form( icm:und*int:usr*C, IcmInt  ):-
%	input_form( answer(C), IcmInt ).


output_form( icm:und*int:usr*C, Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', �r det korrekt?'], Output ).




% clarification question
output_form( icm:und*int:usr*AltQ, Output):-
	output_form( ask(AltQ), Output).



% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, ['Ockej.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), ['Ledsen, jag kan inte svara p� fr�gor om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
output_form( icm:acc*neg:P, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' �r inte en korrekt parameter.'], Rest ).

% indicate loading a plan (pushed by findPlan)
%output_form( icm:loadplan, ['I need some information.'] ).
output_form( icm:loadplan, ['L�t oss se.'] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, ['G� tillbaks till fr�gan om '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, ['G� tillbaks till '|InputQDot]):-
	( input_form( InputQ, request(A) ); output_form( action(A), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise issue (system-initiated, where question follows immediately after)
output_form( icm:reraise, ['S�,']).

% accommodation
output_form( icm:accommodate:_, ['Visst.']  ).

output_form( icm:reaccommodate:Q, ['G� tillbaks till fr�gan om'|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).



output_form( not C, ['Inte'|S] ):- output_form( C, S ).


/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/


	

	

%input_form( S, answer(C) ):- lexsem( S, C ), sem_sort( C, country ).


% general negation 010419
input_form( [inte|S], answer(not(C))):- input_form(S,answer(C)).



input_form( [ja], answer(yes) ).
input_form( [nej], answer(no) ).


% simple stuff

input_form( [hej], greet ).
input_form( [hej,d�], quit ).
input_form( [sluta], quit ).

% ICM

input_form( [f�rl�t], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).

input_form( [vet, inte], icm:acc*neg:issue ).



/*----------------------------------------------------------------------
     yn_answer( ?YN )
----------------------------------------------------------------------*/

yn_answer(A):-
	A = 'ja';
	A = 'nej'.










/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

synset( [[videon],[video]], vcr ).

%synset( [[DigitWord]], Digit ):- digit_word( DigitWord, DigitString ), name( Digit, DigitString ).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).





