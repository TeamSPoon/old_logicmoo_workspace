 
/*************************************************************************

         name: lexicon_svenska_player.pl 
	 date: 2004-10-25
       author: Andreas Wallentin
 
*************************************************************************/

:- module( lexicon_player_svenska, [output_form/2,
				    input_form/2,
				    yn_answer/1]).

:- multifile synset/2.
:- discontiguous output_form/2, input_form/2.

resource_of_type(lexicon).

:- use_module( library(lists), [ member/2, select/3, append/3, is_list/1 ] ).
%%:- use_module( library(charsio), [ format_to_chars/3 ] ).

%% f�r mer variation av output
:- use_module( library(random) ).

%:- use_module( dbase ).
:- ensure_loaded( digits_svenska_player ).
:- ensure_loaded( semsort_player ).
%:- ensure_loaded( groups ).


/*----------------------------------------------------------------------
     output_form( +Move, -String )
     -- Canned output
----------------------------------------------------------------------*/
/*
F�r mer variation i output, slumpas olika fraser fram.
Samma f�r avsluten.
*/
greetings(['Musikspelaren �r klar f�r anv�ndning.','V�lkommen till musikspelaren']).
byes(['Hej d�!','Hoppas att du hade det trevligt','Adj� adj�']).

% getNoXInList(+VilketIOrdning, +Lista, -UtvaltSvar).
getNoNInList(1,[X|_],X).
getNoNInList(Num, [_|Xs], Svar):-
	N is Num-1,
	getNoNInList(N,Xs,Svar).


output_form( action(top), ['toppniv�'] ).

%% Called the first time the program is running
%%
output_form( greet, [Greeting] ):-
	random(1,3,N),
	greetings(List),
	getNoNInList(N,List,Greeting).

output_form( quit, [Ends] ):-
	random(1,4,N),
	byes(List),
	getNoNInList(N,List,Ends).


% ask-moves
output_form( ask(X^(action(X))), ['Vad kan jag g�ra f�r dig?'] ).

output_form( ask(action(T)), Str ):-
	output_form(action(T), StrT ),
	append( ['Vill du '], StrT, Str0 ),
	append( Str0, ['?'], Str).


%% ta reda p� saker fr�n anv�ndaren
output_form( ask(X^playlist(X)),
	     ['Vilken spellista vill du �ppna?'] ).
output_form( ask(X^itemAdd(X)),
	     ['Vilken l�t vill du l�gga till i spellistan?'] ).
output_form( ask(X^itemRem(X)),
	     ['Vilken l�t(indexnummer) vill du ta bort fr�n spellistan?'] ).
output_form( ask(X^groupToAdd(X)),
	     ['Vilken grupp �r du ute efter?'] ).
output_form( ask(X^station(X)),
	     ['Vilken radiostation vill du lyssna p�?'] ).
output_form( ask(X^listenTo(X)),
	     ['Vill du lyssna p� radio eller l�tar?'] ).
output_form( ask(X^artist(X)),
	     ['Vilken artist menar du?'] ).
output_form( ask(X^song(X)),
	     ['Vilken l�t menar du?'] ).
output_form( ask(X^album(X)),
	     ['Vilket album menar du?'] ).
output_form( ask(X^song_artist(X)),
	     ['Vilken artist menar du?'] ).

output_form( ask(X^group(X)),
	     ['Vilken artist menar du?'] ).
output_form( ask(X^item(X)),
	     ['Vilken l�t menar du?'] ).

output_form( ask(X^what_to_play(X)),
	     ['Vilken l�t i spellistan vill du spela?'] ).

output_form( answer(path(Path)),                Ans ):-
	( Path = ''
	->
	    Ans = ['Det finns ingen s�kv�g som matchar s�kkreterierna.']
	;
	    Ans = ['S�kv�gen till l�ten �r:',Path]
	).


output_form( answer(fail(Path^path(Path),no_matches)),                Ans ):-
	Ans = ['S�kv�gen till l�ten �r inte detnna:',Path].

output_form( answer(artists_song(Artist)),      ['F�ljande artist/-er har gjort den:',Artist] ).
output_form( answer(artists_album(Artist)),     Answer ):-
	(
	  Artist = ''
	->
	  Answer = ''
	;
	  (
	    Artist = 'best_of'
	  ->
	    Answer = ['Albumet �r ett samlingsalbum']
	  ;
	    Answer = ['Albumet har gjorts av',Artist]
	  )
	).

output_form( answer(albums_by_artist(Albums)),  Answer ):-
	( Albums = ''
	-> Answer = ['Det finns inga album']
	; Answer = ['F�ljande album finns:',Albums]
	).
output_form( answer(current_song([A,B])),           Answer ):-
	Answer = ['Du lyssnar p�',A,'-',B].

output_form( answer(songs_by_artist(Songs)),    ['De har gjort:',Songs] ).



output_form( issue(_^path(_)),            ['fr�ga v�g'] ).
output_form( action(restart),           ['b�rja om'] ).

output_form( action(handle_player),     ['prata med spelaren'] ).
output_form( action(handle_playlist),   ['�ndra i spellistan'] ).
output_form( action(handle_stations),   ['v�lja radiostationer'] ). 

output_form( action(start),             ['starta spelaren'] ).
output_form( action(start_specific),    ['spela en viss l�t'] ).
output_form( action(stop),              ['stoppa spelaren'] ).
output_form( action(pause),             ['pausa musiken'] ).
output_form( action(resume),            ['�teruppta musiken'] ).
output_form( action(fast_rewind),       ['spola i l�ten'] ).
output_form( action(start_playlist),    ['spela en viss spellista'] ).
output_form( action(fast_forward),             ['spola fram�t'] ).
output_form( action(rewind),                   ['spola bak�t'] ).
output_form( action(next_song),                ['till n�sta'] ).
output_form( action(previous_song),            ['till f�reg�ende'] ).

output_form( action(playlist_add),      ['l�gga till en l�t i spellistan'] ).
output_form( action(playlist_del_specific),      ['ta bort en l�t fr�n spellistan'] ).
output_form( action(playlist_del),      ['ta bort spellistan'] ).
output_form( action(playlist_shuffle),  ['blanda ordningen p� l�tarna'] ).
output_form( action(show_list),                ['visa spellistan'] ).

%%% confirming actions
output_form( confirm(handle_player),    ['klar med att hantera spelaren'] ).
output_form( confirm(handle_playlist),  ['klar med att hantera spellistor'] ).
output_form( confirm(handle_stations),  ['klar med att hantera radiostationer'] ).

output_form( confirm(start),            ['Startar musiken'] ).
output_form( confirm(start_specific),    ['Startar musiken'] ).
output_form( confirm(stop),             ['Musiken �r stoppad'] ).
output_form( confirm(pause),            ['Pausar spelaren'] ).
output_form( confirm(resume),           ['�terupptar musiken'] ).
%output_form( confirm(fast_rewind),      ['soplar �t n�t h�ll'] ).
output_form( confirm(start_playlist),    ['Spelar spellista'] ).
output_form( confirm(fast_forward),     ['Spolar lite fram�t'] ).
output_form( confirm(rewind),           ['Spolar lite bak�t'] ).

output_form( confirm(playlist_add),     ['Spellistan �r ut�kad'] ).
output_form( confirm(playlist_del_specific),     ['Spellistan har reducerats'] ).
output_form( confirm(playlist_del),     ['Spellistan �r borttagen'] ).
output_form( confirm(playlist_shuffle), ['Spellistans ordning har blandats'] ). 
output_form( confirm(show_list),        ['Spellistan visad'] ).

output_form( confirm(vol_up),           ['�kar volymen'] ).
output_form( confirm(vol_down),         ['S�nker volymen'] ).
output_form( confirm(next_song),        ['Till n�sta l�t'] ).
output_form( confirm(previous_song),    ['Till f�reg�ende l�t'] ).

output_form( report('PlaylistAdd', failed(G,S)), Ans ):-
   	make_name(G,Group),
    	make_name(S,Song),
   	Ans = ['Tyv�rr finns inte',Song,'med',Group].

output_form( report('Resume', failed ),
	     ['Spelaren stod p� inte p� paus, s� d�rf�r ingen resume'] ).

%%% output_form( report('Start', failed(Status) ), %%% spelare p� paus
%%%	     ['Spelaren stod p�',Status,'D� m�ste resume k�ras'] ).


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




%%% used in output_form/2 with ask(set(...))
altlist2alts_or( [Alt], ['eller'|OutputAlt] ):-
	output_form(Alt, OutputAlt ).
altlist2alts_or( [Alt|Alts], [','|Output] ):-
	output_form(Alt, OutputAlt ),
	altlist2alts_or(Alts, AltsOr),
	append( OutputAlt, AltsOr, Output).


% object-level clarification and groundnig questions
output_form( ask(C), Output  ):-
	output_form( icm:und*pos:_*C, IcmPos ),
	append( IcmPos0,['.'],IcmPos),
	append( IcmPos0, [', �r det korrekt?'], Output ).



output_form( ask(set([Alt0|Alts])), Output):-
	output_form(Alt0, Alt0out),
	altlist2alts_or( Alts, AltsOr ),
	append(['Vill du '|Alt0out], AltsOr, Output0 ),
	append(Output0, ['.'], Output).

% output_form( ask(set([Alt0|Alts])), Output):-
% 	output_form(Alt0, Alt0out),
% 	altlist2alts_or( Alts, AltsOr ),
% 	append(['Vill du '|Alt0out], AltsOr, Output0 ),
% 	append(Output0, ['?'], Output).

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


% for asking metaissue clarification question
%output_form( action(Action), ['to '|Out] ):-
%	input_form( Out, request( Action ) ).

% ICM

% contact
output_form( icm:con*neg, ['Hall�?'] ).


% perception
output_form( icm:per*int, ['Urs�kta?'] ).
output_form( icm:per*int, ['Vad sa du?'] ).
output_form( icm:per*neg, ['Urs�kta, Jag h�rde inte vad du sa.'] ).


output_form( icm:per*pos:String, ['Jag tyckte du sa',Name,'.'] ):-
	name( Name, String ).

output_form( icm:sem*int, ['Vad menar du'] ).
output_form( icm:sem*neg, ['F�rl�t, jag f�rst�r inte.'] ).
output_form( icm:sem*pos:Move, InputDot ):-
	input_form( Input, Move ),
	append( Input, ['.'], InputDot ).


% understanding(pragmatic)
output_form( icm:und*neg, ['Jag f�rst�r inte riktigt.']  ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% kommer ett helt set??  kolla fr�n generate i output-f�nstret i startit...
%%% kolla i generate-modulen vilka icm som finns i repr. d�r.
output_form( icm:und*pos:usr*issue(G^group(G)),
	    ['Du vill svara p� vilken artist'] ).
output_form( icm:und*pos:usr*issue(G^artist(G)),
	    ['Du vill svara p� vilken artist'] ).
output_form( icm:und*pos:usr*issue(G^song_artist(G)),
	    ['Du vill svara p� vilken artist'] ).

output_form( icm:und*pos:usr*issue(S^song(S)),
	    ['Du vill svara p� vilken s�ng'] ).
output_form( icm:und*pos:usr*issue(S^item(S)),
	    ['Du vill svara p� vilken s�ng'] ).

output_form( icm:und*pos:usr*issue(S^whatToPlay(S)),
	    ['Du vill svara p� vilken l�t du vill spela'] ).
output_form( icm:und*pos:usr*issue(S^itemRem(S)),
	    ['Du vill svara p� vilket indexnummer du ska ta bort'] ).

output_form( icm:und*pos:usr*issue(S^station(S)),
	    ['Du vill svara p� vilken radiostation'] ).

output_form( icm:und*pos:usr*issue(S^album(S)),
	    ['Du vill svara p� vilket album'] ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
output_form( icm:reraise:A, ['G�r tillbaks till '|InputQDot]):-
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
input_form( [japp], answer(yes) ).
input_form( [jajamen], answer(yes) ).
input_form( [jajamensan], answer(yes) ).
input_form( [yes ], answer(yes) ).
input_form( [visst], answer(yes) ).
input_form( [nej], answer(no) ).
input_form( [n�], answer(no) ).
input_form( [no ], answer(no) ).

% simple stuff

input_form( [hej], greet ).
input_form( [hejsan], greet ).
input_form( [tjena], greet ).
input_form( [hej,d�], quit ).
input_form( [sluta], quit ).
input_form( [avsluta], quit).
%input_form( [avbryt], quita ).

			% ICM

input_form( [f�rl�t], icm:per*neg ).
input_form( [urs�kta], icm:per*neg ).
input_form( [va], icm:per*neg ).
input_form( [vad,sa,du], icm:per*neg ).
input_form( [jag,h�rde,inte], icm:per*neg ).
input_form( [okej], icm:acc*pos ).
input_form( [ok], icm:acc*pos ).
input_form( [visst], icm:acc*pos ).
input_form( [vet, inte], icm:acc*neg:issue ).


/******************************
            ACTIONS
******************************/

%%%%%  Requests  %%%%%
input_form( [b�rja,om],   request(restart) ).
%%input_form( [top],        request(top) ).
input_form( [toppniv�],   request(top) ).
input_form( [g�,upp�t],   request(up) ).
input_form( [tillbaka],   request(up) ).

%%% f�r svar p� ask(set([...]))
input_form( [prata,med,spelaren],      request(handle_player) ).
input_form( [�ndra,i,spellistan],     request(handle_playlist) ).
input_form( [v�lja,en,radiostation], request(handle_stations) ).
input_form( [starta,spelaren],        request(start) ).
input_form( [spela,en,viss,l�t],      request(start_specific) ).
input_form( [stoppa,spelaren],        request(stop) ).
input_form( [pausa,musiken],          request(pause) ).
input_form( [�teruppta,musiken],      request(resume) ).
input_form( [spola,i,l�ten],          request(fast_rewind) ).
input_form( [spela,en,viss,spellista],request(start_playlist) ).
input_form( [spola,fram�t],           request(fast_forward) ).
input_form( [spola,bak�t],            request(rewind) ).
input_form( [till,n�sta],             request(next_song) ).
input_form( [n�sta,l�t],             request(next_song) ).
input_form( [spela,n�sta],             request(next_song) ).
input_form( [till,f�reg�ende],        request(previous_song) ).
input_form( [f�reg�ende,l�t],        request(previous_song) ).
input_form( [spela,f�reg�ende],        request(previous_song) ).
input_form( [l�gga,till,en,l�t,i,spellistan], request(playlist_add) ).
input_form( [l�gg,till,markerad,l�t,till,spellista], request(playlist_add) ).
input_form( [ta,bort,en,l�t,fr�n,spellistan], request(playlist_del_specific) ).
input_form( [ta,bort,markerad,l�t,fr�n,spellistan], request(playlist_del_specific) ).
input_form( [ta,bort,spellistan],             request(playlist_del) ).
input_form( [rensa,spellistan],               request(playlist_del) ).
input_form( [blanda,ordningen,p�,l�tarna],    request(playlist_shuffle) ).
input_form( [visa,spellistan],                request(show_list) ).
%%% slut p� fr�gealternativ

input_form( [l�gga,till,en,l�t],      request(playlist_add) ).
input_form( [l�gg,till,en,l�t],       request(playlist_add) ).

input_form( [l�gg,till|Group], [request(playlist_add),answer(group(Sem))]):-
	lexsem(Group,Sem),
	sem_sort(Sem,group).

input_form( [l�gg,till|Song], [request(playlist_add),answer(group(Sem))]):-
	lexsem(Song,Sem),
	sem_sort(Sem,item).


input_form( [l�gg,till|GroupToPlaylist], [request(playlist_add),answer(group(Sem))]):-
	( ToPlaylist=[till,spellistan];ToPlaylist=[i,spellistan]),
	append(Group,ToPlaylist,GroupToPlaylist),
	lexsem(Group,Sem),
	sem_sort(Sem,group).

input_form( [l�gg,till|SongToPlaylist], [request(playlist_add),answer(group(Sem))]):-( ToPlaylist=[till,spellistan];ToPlaylist=[i,spellistan]),
	append(Song,ToPlaylist,SongToPlaylist),
	lexsem(Song,Sem),
	sem_sort(Sem,item).

	
input_form( [Player],     request(handle_player) )     :- lexsem( Player,   player ).
input_form( [Playlist],   request(handle_playlist) )   :- lexsem( Playlist, playlist ).
input_form( [v�lja],      request(listen_to) ).

input_form( [spela|X],    [request(start_specific),answer(index(X))] ):-
	sem_sort(X,index).
input_form( [spela|Group], [request(start),request(playlist_add),answer(group(Sem))] ):-
	lexsem(Group,Sem),
	sem_sort(Sem,group).
input_form( [spela|Song], [request(start),request(playlist_add),answer(item(Sem))] ):-
	lexsem(Song,Sem),
	sem_sort(Sem,item).

input_form( [Station],    request(handle_stations) )   :- lexsem( Station,  station ). 

input_form( [Play],       request(start) )             :- lexsem( Play,     play ).
%input_form( [Play,n�sta],       request(next_song) )          :- lexsem( Play,     play ).

%%% input_form( [Play,Song],      request(play_song) ):-
%%% 	sem_sort(Song,item),
%%% 	lexsem( Play,     play ).

input_form( [Stop],       request(stop) )              :- lexsem( Stop,     stop ).
input_form( [Pause],      request(pause) )             :- lexsem( Pause,    pause ).
input_form( [Resume],     request(resume) )            :- lexsem( Resume,   resume ).
input_form( [spola],      request(fast_rewind) ).
input_form( [bak�t],      request(rewind) ).
input_form( [fram�t],     request(fast_forward) ).
input_form( [n�sta],      request(next_song) ).
input_form( [f�reg�ende], request(previous_song) ).

input_form( [spela,spellista],  request(start_playlist) ).
input_form( [en,spellista],     request(start_playlist) ).
input_form( [l�gga,till],       request(playlist_add) ).
input_form( [l�gg,till],        request(playlist_add) ).
input_form( [s�tta,p�],         request(playlist_add) ).
input_form( [visa,listan],      request(show_list) ).
input_form( [visa,spellistan],  request(show_list) ).

input_form( [h�ra,p�],    request(listen_to) ).
input_form( [lyssna,p�],  request(listen_to) ).
input_form( [blanda],     request(playlist_shuffle) ).

%input_form( [List],       request(playlist_del) )      :- lexsem(List,list).
%input_form( [l�t],        request(playlist_del_specific) ).

%% ny plan som fr�gar vad man vill ta bort
%input_form( [ta,bort],    request(remove) ).
input_form( [ta,bort,List],   request(playlist_del) )  :- lexsem(List,list).
input_form( [rensa,List],     request(playlist_del) )  :- lexsem(List,list).

input_form( [ta,bort|X],   [request(playlist_del_specific),
			    answer(index(C)) ] ):-
	lexsem(X,C),
	sem_sort(C,index).

input_form( X , answer(index(C)) ):-
	lexsem(X,C),
	sem_sort(C,index).

input_form( [ta,bort,en,l�t],   request(playlist_del_specific) ).
input_form( [ta,bort],   request(playlist_del_specific) ).
input_form( [ta,bort,l�t],      request(playlist_del_specific) ).
	  
input_form( [Inc],        request(vol_up) )            :- lexsem(Inc,increase).
input_form( [Dec],        request(vol_down) )          :- lexsem(Dec,decrease).



%%%%%  Answers  %%%%%
input_form( X,            answer(index(X)) ):-            sem_sort(X,index).
input_form( Station,      answer(station(Sem)) ):-
	lexsem(Station,Sem),sem_sort(Sem,station).
input_form( Group,        answer(group(Sem)) ):-
	lexsem(Group,Sem),  sem_sort(Sem,group).
input_form( Playlist,     answer(playlist(Playlist)) ):-  sem_sort(Playlist,playlist).
%%input_form( [Year],       answer(year(Year)) ):-          sem_sort(Year,year).
input_form( SongRadio,    answer(item(Sem)) ):-
	lexsem(SongRadio,Sem),sem_sort(Sem,item).
input_form( Album,        answer(album(Album)) ):-        sem_sort(Album,album).
input_form( Station,      answer(station(IP)) ):-
 	longNum(Station,IP),
 	sem_sort(IP,station).

%%%%%  Questions to DB  %%%%%

input_form( [vilka,album],          ask(A^albums_by_artist(A)) ).
input_form( [s�ka,efter,album],     ask(A^albums_by_artist(A)) ).
input_form( [vad,heter,den],        ask(X^current_song(X)) ).
input_form( [l�ten,som,spelas,nu],  ask(X^current_song(X)) ).
input_form( [vad,heter,den,h�r,l�ten],  ask(X^current_song(X))).
input_form( [vad,heter,denna,l�ten],  ask(X^current_song(X))).
input_form( [vad,heter,denna,l�t],  ask(X^current_song(X))).
input_form( [vilken,l�t,�r,detta],  ask(X^current_song(X))).
input_form( [vilken,l�t,�r,det,h�r],  ask(X^current_song(X))).


input_form( [vem,har,gjort,albumet], ask(A^artists_album(A)) ).
input_form( [vem,har,gjort,l�ten],   ask(A^artists_song(A)) ).
input_form( [vilken,s�kv�g],         ask(A^path(A)) ).

%%% f�r mer explicit input
input_form( [vem,har,gjort,l�ten|Song],    [ask(A^artists_song(A)),answer(item(Sem))] ):-
	lexsem(Song,Sem),
   	sem_sort(Sem,item).
input_form( [vem,har,gjort,albumet|Album], [ask(A^artists_album(A)),answer(album(Album))] ):-
   	sem_sort(Album,album).

%%% mer generellt
input_form( [vem,har,skrivit|Song],  [ask(A^artists_song(A)),answer(item(Sem))] ):-
	lexsem(Sem,Song),
   	sem_sort(Sem,item).
nput_form( [vem,har,gjort|Song],    [ask(A^artists_song(A)),answer(item(Sem))] ):-	lexsem(Song,Sem),
	sem_sort(Sem,item).
input_form( [vem,har,skrivit|Album], [ask(A^artists_album(A)),answer(album(Album))] ):-
   	sem_sort(Album,album).
input_form( [vem,har,gjort|Album],   [ask(A^artists_album(A)),answer(album(Album))] ):-	
	sem_sort(Album,album).

input_form( [vilka,l�tar],           ask(Songs^songs_by_artist(Songs)) ).

%%% input_form( [vilka,grupper],           ask(Groups^all_groups(Groups)) ).


/*

Kommande predikat...
input_form( [vilka,l�tar,har,X,gjort], ask(Songs^songs_by_current_artist(Songs)) ):-
 	lexsem(X,ask_current).

input_form( [n�gonting,med], request(find_group) ).

*/

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

%synset( [[videon],[video]], vcr ).
synset( [spelare,spelaren,spelarn,musiken],                    player ).
synset( [spellista,spellistor,spellistan,spellistorna],        playlist ).
synset( [listor,listan,lista],                                 list ).
synset( [starta,play,plej,spela],                                   play ).
synset( [radio,station,radiostation,stationer,radiostationer], station ).
synset( [[l�gg,till],[l�gga,till]],                            add ).
synset( [stop,stopp,stoppa,stanna],                            stop ).
synset( [resume,risyum,�teruppta],                                    resume ).
synset( [paus,pausa,p�s],                                          pause ).

synset( [h�j,h�ja,�ka],                                        increase ).
synset( [s�nk,s�nka,minska],                                   decrease ).

synset( [han,hon,de,dem,dom],                                  ask_current ).

%index
synset( [[n�sta]], next).
synset( [[f�reg�ende]], previous).
synset( [[ett]], 1).
synset( [[tv�]], 2).
synset( [[tre]], 3).
synset( [[fyra]], 4).
synset( [[fem]], 5).
synset( [[sex]], 6).
synset( [[sju]], 7).
synset( [[�tta]], 8).
synset( [[nio]], 9).
synset( [[tio]], 10).
synset( [[elva]],11).
synset( [[tolv]],12).
synset( [[tretton]],13).


synset([[wilmer,x]],wilmer_x).
synset([[uno,svenningsson]],uno_svenningsson).
synset([[ulf,lundell]],ulf_lundell).
synset([[tomas,ledin]],tomas_ledin).
synset([[tomas,ledin]],tomas_ledin).
synset([[thomas,di,leva]],thomas_di_leva).
synset([[staffan,hellstrand]],staffan_hellstrand).
synset([[petter]],petter).
synset([[peter,lemarc]],peter_lemarc).
synset([[peter,lemarc]],peter_lemarc).
synset([[patrik,isaksson]],patrik_isaksson).
synset([[orup]],orup).
synset([[monica,t�rnell]],monica_t�rnell).
synset([[mikael,wiehe]],mikael_wiehe).
synset([[mikael,rickfors]],mikael_rickfors).
synset([[mauro,scocco]],mauro_scocco).
synset([[mauro,scocco]],mauro_scocco).
synset([[marie,fredriksson]],marie_fredriksson).
synset([[lustans,lakejer]],lustans_lakejer).
synset([[lisa,nilsson]],lisa_nilsson).
synset([[lisa,ekdahl]],lisa_ekdahl).
synset([[lars,winnerb�ck]],lars_winnerb�ck).
synset([[kent]],kent).
synset([[jakob,hellman]],jakob_hellman).
synset([[irma]],irma).
synset([[imperiet]],imperiet).
synset([[gyllene,tider]],gyllene_tider).
synset([[freda]],freda).
synset([[eva,dahlgren]],eva_dahlgren).
synset([[eva,dahlgren]],eva_dahlgren).
synset([[eldkvarn]],eldkvarn).
synset([[ebba,gr�n]],ebba_gr�n).
synset([[docent,d�d]],docent_d�d).
synset([[christer,sandelin]],christer_sandelin).
synset([[bo,kaspers,orkester]],bo_kaspers_orkester).
synset([[annelie,ryde]],annelie_ryde).
synset([[adolphson,och,falk]],adolphson_och_falk).

synset([[teknikens,under]],teknikens_under).
synset([[under,ytan]],under_ytan).
synset([[�ppna,landskap]],�ppna_landskap).
synset([[sommaren,�r,kort]],sommaren_�r_kort).
synset([[en,del,av,mitt,hj�rta]],en_del_av_mitt_hj�rta).
synset([[vem,skall,jag,tro,p�]],vem_skall_jag_tro_p�).
synset([[lilla,f�gel,bl�]],lilla_f�gel_bl�).
synset([[vinden,har,v�nt]],vinden_har_v�nt).
synset([[s�g,som,det,�r]],s�g_som_det_�r).
synset([[h�ll,om,mig]],h�ll_om_mig).
synset([[du,f�r,g�ra,som,du,vill]],du_f�r_g�ra_som_du_vill).
synset([[jag,blir,hellre,jagad,av,vargar]],jag_blir_hellre_jagad_av_vargar).
synset([[vintersaga]],vintersaga).
synset([[flickan,och,kr�kan]],flickan_och_kr�kan).
synset([[vingar]],vingar).
synset([[sarah]],sarah).
synset([[det,finns]],det_finns).
synset([[efter,stormen]],efter_stormen).
synset([[diamanter]],diamanter).
synset([[himlen,runt,h�rnet]],himlen_runt_h�rnet).
synset([[vem,vet]],vem_vet).
synset([[kom,ih�g,mig]],kom_ih�g_mig).
synset([[om,du,var,h�r]],om_du_var_h�r).
synset([[vara,v�nner]],vara_v�nner).
synset([[precis,som,du]],precis_som_du).
synset([[du,ska,va,president]],du_ska_va_president).
synset([[flickorna,p�,tv,tv�]],flickorna_p�_tv_tv�).
synset([[vindarna]],vindarna).
synset([[�ngeln,i,rummet]],�ngeln_i_rummet).
synset([[vem,t�nder,stj�rnorna]],vem_t�nder_stj�rnorna).
synset([[k�rlekens,tunga]],k�rlekens_tunga).
synset([[�tta,hundra,grader]],�tta_hundra_grader).
synset([[solglas�gon]],solglas�gon).
synset([[det,hon,vill,ha]],det_hon_vill_ha).
synset([[undantag]],undantag).
synset([[segla,p�,ett,moln]],segla_p�_ett_moln).
synset([[blinkar,bl�]],blinkar_bl�).






