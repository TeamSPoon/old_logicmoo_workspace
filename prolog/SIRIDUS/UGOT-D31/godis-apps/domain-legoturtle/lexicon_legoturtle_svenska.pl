:- module( lexicon_legoturtle_svenska, [output_form/2, input_form/2,
				 yn_answer/1]).

:- discontiguous output_form/2, input_form/2, plan/2, postcond/2.

:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).

:- ensure_loaded( library(lexicon_numbers) ).
:- ensure_loaded( library(digits_legoturtle) ).

:- ensure_loaded( library( semsort_legoturtle ) ).


%:- multifile( output_form/2 ).
%:- multifile( input_form/2 ).

/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/

% ask-moves

output_form( ask(X^(action(X) ) ), ['Vad vill du att jag ska g�ra?'] ).
output_form( ask(action(T) ), Str) :-
	output_form( action(T), StrT ), 
	append( ['Vill du att jag skall'], StrT, Str0 ),
	append( Str0, ['?'], Str).
output_form( ask(X^steps(X) ), ['Hur l�ngt?'] ).
output_form( ask(X^degrees(X) ), ['Hur m�nga grader vill du att jag skall v�nda mig? V�lj en siffra mellan 0 och 360.'] ).
output_form( ask(X^color(X) ), ['Vilken f�rg vill du ha?'] ).

% action

output_form( action(top), ['g� till startl�get'] ).
output_form( action(pen_up), ['ta upp pennan'] ).
output_form( action(pen_down), ['s�tta ner pennan'] ).
output_form( action(background), ['byta bakgrundsf�rg'] ).
output_form( action(pencolor), ['byta f�rg p� pennan'] ).
output_form( action(move), ['flytta p� mig'] ).
output_form( action(forward), ['g� fram�t'] ).
output_form( action(backward), ['g� bak�t'] ).
output_form( action(turn), ['v�nda mig'] ).
output_form( action(right), ['v�nda mig till h�ger'] ).
output_form( action(left), ['v�nda mig till v�nster'] ).
output_form( action(clear), ['sudda ut allt vi ritat'] ).
output_form( action(circle), ['rita en cirkel'] ).
output_form( action(tree), ['rita ett tr�d'] ).
	   
%

output_form( icm:neg:action(forward), 
 	     ['Jag kan inte g� s� l�ngt.'] ).
output_form( icm:neg:action(backward), 
 	     ['Jag kan inte g� s� l�ngt.'] ).

output_form( icm:neg:action(right), ['Ledsen, jag kan inte v�nda mig mer �n  360 grader.'] ).
output_form( icm:neg:action(left), ['Ledsen, jag kan inte v�nda mig mer �n  360 grader.'] ).

output_form( confirm(pen_up), ['Nu kan jag g� utan att rita.'] ).
output_form( confirm(pen_down), ['Nu kan jag b�rja rita.'] ).
output_form( confirm(move), ['Nu kan jag flytta p� mig.'] ).
output_form( confirm(forward), ['Nu har jag g�tt fram�t.'] ).
output_form( confirm(backward), ['Nu har jag g�tt bak�t.'] ).
output_form( confirm(turn), ['Nu kan jag v�nda mig.'] ).
output_form( confirm(left), ['Nu har jag v�nt mig �t v�nster.'] ).
output_form( confirm(right), ['Nu har jag v�nt mig �t h�ger.'] ).
output_form( confirm(pencolor), ['Nu har jag bytt f�rg p� pennan.'] ).
output_form( confirm(background), ['Nu har jag bytt bakgrundsf�rg.'] ).
output_form( confirm(clear), ['Nu har jag suddat ut allt.'] ).
output_form( confirm(circle), ['Nu har jag ritat en cirkel.'] ).
output_form( confirm(tree), ['Nu har jag ritat ett tr�d.'] ).

% H�lsning
output_form( greet, ['Hej, vad kul att du vill rita med mig!'] ).
output_form( quit, ['Hej d�!'] ).

%Hj�lpmeny
output_form( help, ['Vill du att jag skall rita, v�nda mig �t h�ger eller v�nster, rita en cirkel, ett tr�d eller bara g� utan att rita?'] ).

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
% request sequences: if requests (followed by 0-2 answers) are separated by
%"and", reverse list so that first request is put last on stack; this is done by%putting last in the list
% SL 030508

input_form( ReqForms, RevList ) :-
        RevList = [_|_],
        append( ReqForm1, [och | ReqForm2 ], ReqForms ),
        ( AnsList1 = [] ; AnsList1 = [ answer(_) ]; AnsList1 = [answer(_),answer(_)] ),
        input_form( ReqForm1, [ request(A1) | AnsList1 ] ),
        ( AnsList2 = [] ; AnsList2 = [ answer(_) ]; AnsList2 = [answer(_),answer(_)] ),
        input_form( ReqForm2, [ request(A2) | AnsList2 ] ),
        append( [ request(A2) | AnsList2 ],[ request(A1) | AnsList1 ], RevList).

% input form for request followed by sequence of answers
input_form( ReqAnsForm, [ request(A) | AnsList ] ):-
        append( ReqForm, AnsForm, ReqAnsForm),
        input_form( ReqForm, request( A ) ),
        input_form_answers( AnsForm, AnsList ).

% input form for sequence of answers
input_form_answers( [], [] ).
input_form_answers( AnsForms, [answer(Ans)|AnsList] ):-
        append( AnsForm, AnsForms1, AnsForms  ),
        input_form( AnsForm, answer( Ans ) ),
        input_form_answers( AnsForms1, AnsList ).

% generic

input_form( [g�,upp],           request(up) ).
input_form( [upp],           request(up) ).

% speak
input_form( [igen], request(top) ).
input_form( [startl�get], request(top) ).
input_form( [startl�ge], request(top) ).
input_form( [start], request(top) ).
input_form( [starta], request(top) ).
input_form( [b�rja], request(top) ).
input_form( [b�rjan], request(top) ).

input_form( [g�, utan, att, rita], request(pen_up)).
input_form( [utan], request(pen_up) ).
input_form( [ta, upp, pennan], request(pen_up) ).
input_form( [sl�pp, pennan], request(pen_up) ).
input_form( [sluta, rita], request(pen_up) ).
input_form( [inte, rita], request(pen_up) ).
input_form( [rita, inte], request(pen_up) ).
input_form( [sluta, m�la], request(pen_up) ).
input_form( [inte, m�la], request(pen_up) ).
input_form( [m�la, inte], request(pen_up) ).

input_form( [rita], request(pen_down) ).
input_form( [s�tt, ner, pennan], request(pen_down) ).
input_form( [m�la], request(pen_down) ).

input_form( [cirkel], request(circle) ).
input_form( [rita, en, cirkel], request(circle) ).
input_form( [ring], request(circle) ).
input_form( [rita, en, ring], request(circle) ).
input_form( [boll], request(circle) ).
input_form( [rita, en, boll], request(circle) ).


input_form( [tr�d], request(tree) ).
input_form( [ett, tr�d], request(tree) ).
input_form( [rita, ett, tr�d], request(tree) ).

input_form( [bakgrundsf�rg], request(background) ).
input_form( [bakgrundsf�rgen], request(background) ).
input_form( [bakgrund], request(background) ).
input_form( [bakgrunden], request(background) ).
input_form( [sk�rmen], request(background) ).
input_form( [sk�rm], request(background) ).

input_form( [pennan], request(pencolor) ).
input_form( [penna], request(pencolor) ).
input_form( [pennf�rg], request(pencolor) ).
input_form( [krita], request(pencolor) ).
input_form( [kritan], request(pencolor) ).
input_form( [kritf�rg], request(pencolor) ).

input_form( [flytta], request(move) ).
input_form( [g�], request(move) ).
input_form( [g�r], request(move) ).

input_form( [streck], request(move) ).
input_form( [ett, rakt, streck], request(move) ).
input_form( [rita, ett, streck], request(move) ).
input_form( [linje], request(move) ).
input_form( [rita, en, linje], request(move) ).

input_form( [fram�t], request(forward) ).
input_form( [fram], request(forward) ).

input_form( [bak�t], request(backward) ).
input_form( [bak], request(backward) ).

input_form( [v�nda], request(turn) ).
input_form( [v�nd], request(turn) ).
input_form( [sv�nga], request(turn) ).
input_form( [sv�ng], request(turn) ).

input_form( [v�nster], request(left) ).
input_form( [g�, till, v�nster], request(left) ).
input_form( [h�ger], request(right) ).
input_form( [g�, till, h�ger], request(right) ).

input_form( [vanster], request(left) ).
input_form( [hoger], request(right) ).

input_form( [sudda], request(clear) ).
input_form( [rensa], request(clear) ).
input_form( [ta, bort], request(clear) ).
input_form( [blanka], request(clear) ).




input_form( SSteg, answer( steps( C ) ) ) :-
	append( S, [steg], SSteg ),
	lexsem( S, C ),
	sem_sort( C, steps ),
	sem_sort( C, number ).

input_form( SGrader, answer( degrees( C ) ) ) :-
	append( S, [grader], SGrader),
	lexsem( S, C ),
	sem_sort( C, degrees ),
	sem_sort( C, number ).


% numbers

input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).

input_form( S, answer( colors( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, colors ).

/*----------------------------------------------------------------------
     output_form( +Move, -WordList )
     -- Canned output
----------------------------------------------------------------------*/



% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', �r det riktigt?'], Output ).



output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Vill du att jag skall'|Alt0out], AltsOr, Output0 ),
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

output_forms( [], [] ).
 output_forms( [ Move | Moves ], Output1 ):-
 	output_form( Move, Output ),
 	output_forms( Moves, Outputs ),
 	append( Output, Outputs, Output1 ).
	
output_form( answer(notexist(X,Q)), [' Ledsen, jag vet inte vad jag ska svara p� fr�gan: '|InputQDot]):-
	input_form( InputQ, ask(X^Q) ),
	append( InputQ, ['.'], InputQDot ).
output_form( answer(unknown(Q)), ['Ledsen, jag vet inte vad jag ska svara p� fr�gan: '|InputQDot]):-
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
output_form( icm:per*int, ['Vad menar du?'] ).
output_form( icm:per*neg, ['Urs�kta, Jag f�rstod inte vad du skrev.'] ).


output_form( icm:per*pos:String, ['Du skrev:',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du'] ).
output_form( icm:sem*neg, ['F�rl�t, jag f�rst�r inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).


% understanding(pragmatic)
output_form( icm:und*neg, ['Jag f�rst�r inte riktigt.']  ).


output_form( icm:und*pos:usr*(not issue(Q)), ['Du fr�gade inte: '|AnsPDot]  ):-
	input_form( AnsP, ask( Q ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*(not P), AnsNotPDot  ):-
	output_form( icm:und*pos:usr*P, AnsPDot  ),
	append( ['inte'],AnsPDot,AnsNotPDot ).

output_form( icm:und*pos:usr*P, AnsPDot  ):-
	( output_form(P, AnsP);
	    input_form( AnsP, answer(P) ) ),
	append(AnsP,['.'],AnsPDot).


output_form( icm:und*pos:usr*PX, IcmPos ):-
	PX =.. [P,X],
	isa( P, P1 ),
	P1X =.. [P1,X],
	output_form( icm:und*pos:usr*P1X, IcmPos ).



output_form( icm:und*int:usr*C, IcmInt  ):-
	output_form( ask(C), IcmInt ).


output_form( icm:und*int:usr*C, Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', �r det riktigt?'], Output ).


% clarification question
output_form( icm:und*int:usr*AltQ, Output):-
	output_form( ask(AltQ), Output).


% "acceptance"/integration

% icm-Type(-Polarity(-Args))
output_form( icm:acc*pos, ['Ok.'] ).

% reject(issue(Q))
output_form( icm:acc*neg:issue(Q), ['Ledsen, jag kan inte svara p� fr�gor om: '|InputQDot]):-
	input_form( InputQ, ask(Q) ),
	append( InputQ, ['.'], InputQDot ).

% reject proposition P
output_form( icm:acc*neg:P, ['Ledsen, '|Rest]):-
	input_form( InputP, answer(P) ),
	append( InputP, [' kan man inte skriva.'], Rest ).

% indicate loading a plan (pushed by findPlan)
output_form( icm:loadplan, [] ).


% reraise issue explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:Q, ['G� tillbaks till fr�gan om '|InputQDot]):-
	( input_form( InputQ, ask(Q) ); output_form( ask(Q), InputQ ) ),
	append( InputQ, ['.'], InputQDot ).

% reraise action explicitly (feedback on user reraise, or system-initiated)
output_form( icm:reraise:A, ['Nu f�r du v�lja'|InputQDot]):-
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


% general negation 010419
input_form( [inte|S], answer(not(C))):- input_form(S,answer(C)).

input_form( [ja], answer(yes) ).
input_form( [nej], answer(no) ).


% simple stuff

input_form( [hej], greet).

input_form( [hejd�], quit ).
input_form( [hej,svejs], quit ).
input_form( [hej,d�], quit ).
input_form( [sluta], quit ).
input_form( [stopp], quit ).
input_form( [avsluta], quit ).
input_form( [avbryt], quit ).

input_form( [hj�lp], help ).

input_form( [?], help ).
input_form( [vad, kan], help ).

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


synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).

synset([[r�d], [r�tt]], red).
synset([[m�rkr�d], [m�rkr�tt]], darkred).
synset([[vit], [vitt]], white).
synset([[svart]], black).
synset([[gul], [gult]], yellow).
synset([[ljusgul], [ljusgult]], lightyellow).
synset([[orange], [oranget]], orange).
synset([[m�rkorange], [m�rkoranget]], darkorange).
synset([[bl�], [bl�tt]], blue).
synset([[m�rkbl�], [m�rkbl�tt]], darkblue).
synset([[ljusbl�], [ljusbl�tt]], lightblue).
synset([[lila]], purple).
synset([[m�rklila]], darkpurple).
synset([[gr�n], [gr�nt]], green).
synset([[m�rkgr�n], [m�rkgr�nt]], darkgreen).
synset([[ljusgr�n], [ljusgr�nt]], lightgreen).
synset([[brun], [brunt]], brown).
synset([[rosa]], pink).
synset([[sk�r], [sk�rt]], pink).
