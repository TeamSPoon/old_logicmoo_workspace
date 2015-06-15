/*************************************************************************

         name: lexicon_lamps_svenska.pl 
	 date: 2004-11-23
       author: Andreas Wallentin
 
*************************************************************************/

:- module( lexicon_lamps_svenska, [ output_form/2,
				    input_form/2,
				    yn_answer/1
				  ]).

:- discontiguous output_form/2, input_form/2.
:- multifile synset/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- ensure_loaded( [semsort_lamps] ).


resource_of_type(lexicon).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

output_form( greet,       ['V�lkommen till X10'] ).
output_form( quit,        ['Aj� aj�'] ).

output_form( action(top), ['b�rjan igen'] ).
output_form( ask(X^(action(X))), ['Vad kan jag g�ra f�r dig?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Vill du '], StrT, Str0 ),
	append( Str0, ['?'], Str).

output_form( ask(L^lamp_to_turn_on(L)),   ['Vilken lampa vill du t�nda?'] ).
output_form( ask(L^lamp_to_turn_off(L)),  ['Vilken lampa vill du sl�cka?'] ).
output_form( ask(L^lamp_to_add(L)),       ['Vilken sorts lampa vill du l�gga till?'] ).
output_form( ask(L^lamp_to_remove(L)),    ['Vilken sorts lampa vill du ta bort?'] ).

output_form( action(lamps_turn_on),    ['t�nda lampor'] ).
output_form( action(lamps_turn_off),   ['sl�cka lampor'] ).
output_form( action(add_lamp),         ['l�gga till en lampa'] ).
output_form( action(remove_lamp),      ['ta bort en lampa'] ).
output_form( action(all_lamps_on),     ['t�nda alla lampor'] ).
output_form( action(all_lamps_off),    ['sl�cka alla lampor'] ).

output_form( confirm(lamps_turn_on),   ['Lampan �r t�nd'] ).
output_form( confirm(lamps_turn_off),  ['Lampan �r sl�ckt'] ).
output_form( confirm(add_lamp),        ['Ny lampa  inkopplad'] ).
output_form( confirm(remove_lamp),     ['Lampan �r urkopplad'] ).
output_form( confirm(all_lamps_on),    ['Alla inkopplade lampor �r nu t�nda'] ).
output_form( confirm(all_lamps_off),   ['Alla inkopplade lampor �r sl�ckta'] ).

output_form( answer(available_lamps(Lamps)),  ['Dessa lampor finns:',Lamps] ).
output_form( answer(lamps_on(Lamps)),         ['Dessa �r t�nda:',Lamps] ).
output_form( answer(lamps_off(Lamps)),        ['Dessa �r sl�ckta:',Lamps] ).
output_form( answer(is_lit(Answer)),          [Answer] ).
output_form( answer(is_off(Answer)),          [Answer] ).

output_form( report('Exist',failed(Lamp)),
	     ['Det finns ingen',Lamp,'inkopplad'] ).
output_form( report('Single',failed(Lamp)),
	     ['Det finns redan en',Lamp,'inkopplad'] ).
output_form( report('TurnOn',failed(Lamp)),
	     [Lamp,'�r redan p�'] ).
output_form( report('TurnOff',failed(Lamp)),
	     [Lamp,'�r redan sl�ckt'] ).
output_form( report('AnyLamps',failed),
	     ['Det finns inga lampor inkopplade'] ).



/*
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
*/




%%% used in output_form/2 with ask(set(...))
altlist2alts_or( [Alt], ['eller'|OutputAlt] ):-
	output_form(Alt, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).


% object-level clarification and groundning questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', �r det korrekt?'], Output ).

output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['?'], Output).

output_form( Alt, OutputAlt ):-
	input_form( OutputAlt, answer( Alt ) ).


output_form( answer(notexist(X,Q)), [' Ledsen, det finns inget som matchar din fr�ga om'|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Ledsen, det finns inget som matchar din fr�ga om'|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% for asking metaissue clarification question
output_form( issue(Q), ['fr�ga om'|Out] ):-
	input_form( Out, ask( Q ) ).


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
	     ['Du vill veta vad spelaren g�r']  ).


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
output_form( icm:acc*pos, ['Okej.'] ).

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

input_form( [inte|S], answer(not(C))):- input_form(S,answer(C)).

input_form( [ja], answer(yes) ).
input_form( [nej], answer(no) ).


% simple stuff

input_form( [hej], greet ).
input_form( [hej,d�], quit ).
input_form( [sluta], quit ).
input_form( [avbryt], quita ).

			% ICM

input_form( [f�rl�t], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).

input_form( [vet, inte], icm:acc*neg:issue ).


%%% restarting the dialogue
input_form( [b�rja,om],   request(lamps_restart) ).

%%% requests
input_form( [t�nd,alla],   request(all_lamps_on) ).
input_form( [t�nda,alla],  request(all_lamps_on) ).
input_form( [sl�ck,alla],  request(all_lamps_off) ).
input_form( [sl�cka,alla], request(all_lamps_off) ).
input_form( [On],          request(lamps_turn_on) ):-   lexsem(On,on).
input_form( [Off],         request(lamps_turn_off) ):-  lexsem(Off,off).
input_form( [l�gga,till],  request(add_lamp) ).
input_form( [l�gg,till],   request(add_lamp) ).
input_form( [ta,bort],     request(remove_lamp) ).

%%% answers
input_form( [Lamp0],      answer(lamp(Lamp)) ):-
	lexsem(Lamp0,Lamp),
	sem_sort(Lamp,lamp).


%%% questions
input_form( [�r,X,t�nd],               ask(Lamp^is_lit(Lamp)) ):-
	lexsem(X,Lamp),
	sem_sort(Lamp,lamp).
input_form( [�r,X,p�],                 ask(Lamp^is_lit(Lamp)) ):-
	lexsem(X,Lamp),
	sem_sort(Lamp,lamp).
input_form( [�r,X,sl�ckt],             ask(Lamp^is_off(Lamp)) ):-
	lexsem(X,Lamp),
	sem_sort(Lamp,lamp).
input_form( [�r,X,av],                 ask(Lamp^is_off(Lamp)) ):-
	lexsem(X,Lamp),
	sem_sort(Lamp,lamp).
input_form( [vilka,lampor,finns],      ask(L^available_lamps(L)) ).
input_form( [vilka,lampor,�r,t�nda],   ask(L^lamps_on(L)) ).
input_form( [vilka,�r,t�nda],          ask(L^lamps_on(L)) ).
input_form( [vilka,lampor,�r,sl�ckta], ask(L^lamps_off(L)) ).
input_form( [vilka,�r,sl�ckta],        ask(L^lamps_off(L)) ).

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

synset( [t�nd,t�nda],    on ).
synset( [sl�ck,sl�cka],  off ).

synset( [bordslampa,bordlampa,bordslampan,bordlampan], bordslampa ).
synset( [taklampa,taklampan],                          taklampa ).
synset( [skrivbordslampa,skrivbordslampan],            skrivbordslampa ).
synset( [s�nglampa,s�nglampan],                        s�nglampa ).
synset( [h�rnlampa,h�rnlampan],                        h�rnlampa ).
synset( [sofflampa,sofflampans],                       sofflampa ).
synset( [golvlampa,golvlampan],                        golvlampa ).

