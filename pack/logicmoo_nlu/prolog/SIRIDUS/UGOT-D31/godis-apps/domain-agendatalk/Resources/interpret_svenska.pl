 
/*************************************************************************
 
         name: interpret_svenska.pl
      version: 
  description: An interpretation file for Swedish and the domain AgendaTalk
       author: Rebeca Jonson
 
*************************************************************************/

%:- discontiguous output_form/3, input_form/2, plan/2, postcond/2.
:-multifile input_form/2.


:- use_module(library(random)).
:- use_module( library(lists), [ member/2, select/3, append/3 ] ).
:- use_module( library(charsio), [ format_to_chars/3 ] ).
:- use_module(calendar, [ampm_disamb/3, day2date/2, day2nr/2, month2nr/2]).
:- ensure_loaded(digits_svenska).
%:- ensure_loaded(time_svenska).
:- ensure_loaded(semsort_agendatalk).

/*----------------------------------------------------------------------
     input_form( +Phrase, -Move )
     -- Almost canned input
----------------------------------------------------------------------*/
input_form( [nej,inte,S], answer(not(C))):- input_form([S],answer(C)).
input_form( [inte,S], answer(not(C))):- input_form([S],answer(C)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          AgendaTalk answers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


input_form([g�ra, en, anteckning], request(add_event)).
input_form([anteckna,i,kalendern], request(add_event)).
input_form([jag,vill,anteckna],request(add_event)).
input_form([anteckna],request(add_event)).
input_form([anteckning],request(add_event)).
input_form([l�gga,till],request(add_event)).
input_form([l�gg,till],request(add_event)).
input_form([boka],request(add_event)).
input_form([boka,in],request(add_event)).
input_form([f�ra,in],request(add_event)).
input_form([jag,ska,p�|Ev],[answer(event_to_store(C)),request(add_event)]):-
	input_form(Ev,answer(event_to_store(C))).
input_form([jag,ska,till|Ev],[answer(event_to_store(C)),request(add_event)]):-
	input_form(Ev,answer(event_to_store(C))).
input_form([jag,har|Ev],[answer(event_to_store(C)),request(add_event)]):-
	input_form(Ev,answer(event_to_store(C))).

input_form([kolla, med, kalendern],request(get_info)).
input_form([kolla, kalendern], request(get_info)).
input_form([fr�ga, kalendern], request(get_info)).
input_form([jag,vill,kolla],request(get_info)).

input_form([l�gga,till, mer, information], request(more_info)).
input_form([l�gga, till, information], request(more_info)).
input_form([l�gg, till, information], request(more_info)).

input_form([ta, bort,allt],request(delete_current_event)).
input_form([gl�m, allt], request(delete_current_event)).

input_form([ta, bort, en, bokning], request(delete_event)).
input_form([ta, bort],request(delete_event)).
input_form([tabort],request(delete_event)).
input_form([radera],request(delete_event)).
input_form([avboka],request(delete_event)).

input_form([�ndra, informationen], request(change_info)).
input_form([g�ra, en, �ndring], request(change_date)).
input_form([jag,vill,�ndra, datum], request(change_date)).
input_form([jag,vill,�ndra, datumet], request(change_date)).
input_form([jag,vill,byta,datum], request(change_date)).
input_form([jag,vill,byta,datumet], request(change_date)).
input_form([jag,vill,flytta], request(change_date)).
input_form([jag,vill,flytta,morgondagens], [request(change_date), answer(date_to_store(tomorrow))]).
input_form([jag,vill,flytta,dagens], [request(change_date), answer(date_to_store(today))]).
%input_form([flytta], request(change_date)).
input_form([jag,vill, g�ra, en, �ndring], request(change_date)).
input_form([�ndra, datum], request(change_date)).
input_form([�ndra, datumet], request(change_date)).
input_form([�ndra, tiden], request(change_time)).
input_form([jag,vill,�ndra, tiden], request(change_time)).
input_form([jag,vill,byta,tid], request(change_time)).
input_form([jag,vill,byta,tiden], request(change_time)).
input_form([�ndra, tid], request(change_time)).
input_form([byta, tid], request(change_time)).
input_form([byta, tiden], request(change_time)).

input_form([datum], answer(which_info(date))).
input_form([datumet], answer(which_info(date))).
input_form([dag], answer(which_info(date))).
input_form([tid], answer(which_info(time))).
input_form([tiden], answer(which_info(time))).
input_form([lokal], answer(which_info(location))).
input_form([lokalen], answer(which_info(location))).
input_form([annan, tid], [request(change_info), answer(which_info(time))]).
input_form([annat, datum], [request(change_info), answer(which_info(date))]).
input_form([�ndra,lokal], [request(change_info), answer(which_info(location))]).
input_form([�ndra,lokalen], [request(change_info), answer(which_info(location))]).
%HELP
input_form( [hj�lp], ask(usage) ).
input_form( [vad, kan, man, g�ra], ask(usage)).

input_form( [toppniv�],                 request(top) ).
input_form( [g�,tillbaka,till,toppniv�],                 request(top) ).
input_form( [g�,tillbaks,till,b�rjan],                 request(top) ).
input_form( [b�rja,om,fr�n,b�rjan], request(top)).
input_form( [b�rjan], request(top)).
input_form( [b�rja,om], request(top)).
input_form( [starta,om], request(top)).

%%%%%%%%%%%%Check Calendar%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_form([vilken, tid],ask(X^start_time_to_store(X))).
input_form([n�r, �r],ask(X^start_time_to_store(X))).
input_form([n�r, var],ask(X^start_time_to_store(X))).
input_form([n�r, b�rjar],ask(X^start_time_to_store(X))).
input_form([kolla, tiden],ask(X^start_time_to_store(X))).
input_form([har,jag,n�got,bokat],ask(X^event_to_store(X))).
input_form([�r,jag,ledig],ask(X^event_to_store(X))).
input_form([�r,jag,bokad],ask(X^event_to_store(X))).
input_form([�r,jag,upptagen],ask(X^event_to_store(X))).
input_form([�r,jag,uppbokad],ask(X^event_to_store(X))).
input_form([har,jag,n�got,planerat],ask(X^event_to_store(X))).
input_form([hur,ser,schemat,ut],ask(X^bookings(X))).
input_form([vad,g�r,jag],ask(X^bookings(X))).
input_form([vad,ska,jag,g�ra],ask(X^bookings(X))).
input_form([vad,har,jag,bokat],ask(X^bookings(X))).
input_form([vad,har,jag,p�,schemat],ask(X^bookings(X))).
input_form([kolla,min,kalender],ask(X^bookings(X))).
input_form([vad,har,jag],ask(X^bookings(X))).
nput_form([vad,h�nder],ask(X^bookings(X))).
input_form([vad,st�r,i,min,kalender],ask(X^bookings(X))).
input_form([hur,ser,dagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(today))]).
input_form([hur,ser,morgondagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(tomorrow))]).
input_form([hur,ser,m�ndagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(monday))]).
input_form([hur,ser,tisdagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(tuesday))]).
input_form([hur,ser,onsdagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(wednesday))]).
input_form([hur,ser,torsdagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,fredagens,schema,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,dagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(today))]).
input_form([hur,ser,morgondagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(tomorrow))]).
input_form([hur,ser,m�ndagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(monday))]).
input_form([hur,ser,tisdagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(tuesday))]).
input_form([hur,ser,onsdagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(wednesday))]).
input_form([hur,ser,torsdagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,fredagens,program,ut],[ask(X^bookings(X)),answer(date_to_store(friday))]).
input_form([hur,ser,mitt,schema,ut],ask(X^bookings(X))).
input_form([hur,ser,mitt,program,ut],ask(X^bookings(X))).
input_form([morgondagens,program],ask(X^bookings(X))).
input_form([morgondagens,schema],ask(X^bookings(X))).
input_form([dagens,schema],ask(X^bookings(X))).
input_form([dagens,program],ask(X^bookings(X))).
input_form([har,jag,Ev],[answer(event_to_store(C)),ask(X^start_time_to_store(X))]):- input_form([Ev],answer(event_to_store(C))).

input_form([dagens,datum],ask(X^todaysdate(X))).
input_form([vilken,dag,�r,det,idag],ask(X^todaysdate(X))).
input_form([vilket,datum,�r,det,idag],ask(X^todaysdate(X))).
input_form([vad,�r,det,f�r,dag,idag],ask(X^todaysdate(X))).
input_form([vad,�r,det,f�r,datum,idag],ask(X^todaysdate(X))).
input_form([vad,�r,dagens,datum],ask(X^todaysdate(X))).
%%%%%%%%%%%%%%%%%EVENTS%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

input_form( E,	answer(event_to_store(C))):- lexsem(E, C), sem_sort(C, event).
input_form( [ett|E],	answer(event_to_store(C))):- lexsem(E, C), sem_sort(C, event).
input_form( L,	[request(more_info),answer(location(C))] ):-lexsem(L, C), sem_sort(C, location).
%%%Attendees%input_form([med,S1,och,S2],attendee(C1,C2)):- lexsem(S1,C1),lexsem(S2,C1),sem_sort(C1,person),sem_sort(C2,person.)
%input_form([med|S],attendee(C)):- lexsem(S,C),sem_sort(C,person).

%%%DATES%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               DATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
input_form([till|DATE],answer(newdate(D))):-input_form(DATE,answer(date(D))).

input_form( DAY,               answer(date(today)) ):- lexsem(DAY,today).
input_form( DAY,            answer(date(tomorrow)) ):- lexsem(DAY,tomorrow).
input_form( DAY,            answer(date(aftertomorrow)) ):- lexsem(DAY,aftertomorrow).
input_form([n�sta,vecka], answer(date([next,week]))).
%%friday the first of july
input_form([Weekday, den, Day, Month], answer(date([Wkday, DayC, MonthC]))):-
	weekDaysSw(WDs),
	member(Weekday, WDs),
	lexsem([Weekday],Wkday),
	dayStr( [Day], DayNr),
	day2nr([DayC],DayNr),
	monthStr([Month],MonthNr),
	month2nr([MonthC],MonthNr).
%%friday first of july
input_form([Weekday, Day, Month], answer(date([Wkday, DayC, MonthC]))):-
	weekDaysSw(WDs),
	member(Weekday, WDs),
	lexsem([Weekday],Wkday),
	dayStr( [Day], Dnr),
	day2nr([DayC],Dnr),
	monthStr([Month],MonthNr),
	month2nr([MonthC],MonthNr).
%% (the) first of july
input_form([den, Day, Month], answer(date([DayC, MonthC]))):-
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr),
	monthStr([Month],MNr),
	month2nr([MonthC],MNr).
%input_form([thirtyfirst, of, april], answer(incons_date([thirtyfirst, april]))).
input_form([Day, Month], answer(date([DayC, MonthC]))):-
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr),
	monthStr([Month],MNr),
	month2nr([MonthC],MNr).

%%the first
input_form([den, Day], answer(date(DayC))):-
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr).
%%friday the second
input_form([Weekday, den, Day], answer(date([Wkday, DayC]))):-
	weekDaysSw(WDs),
	member(Weekday, WDs),
	lexsem([Weekday],Wkday),
	dayStr( [Day], Nr),
	day2nr( [DayC],Nr).

input_form( [p� , WeekDayStr, n�sta, vecka], answer( date([next,WkDay]))):-
	weekDaysSw(WDs),
	member(WeekDayStr, WDs),
	lexsem([WeekDayStr],WkDay).
%%next friday
input_form( [n�sta, WeekDay],             answer(date([next, WkDay])) ):-
	weekDaysSw(WDs),
	member(WeekDay, WDs),
	lexsem([WeekDay],WkDay).

%%friday
input_form( [p� , WeekDayStr ], answer( date(WkDay))):-
	weekDaysSw(WDs),
	member(WeekDayStr, WDs),
	lexsem([WeekDayStr],WkDay).

input_form( [WeekDay], answer(date(WkDay)) ):-
	weekDaysSw(WDs),
	member(WeekDay, WDs),
	lexsem([WeekDay],WkDay).
input_form( [WeekDay], answer(date(WkDay)) ):-
	weekDaysSwDef(WDs),
	member(WeekDay, WDs),
	lexsem([WeekDay],WkDay).

%%%%%%%%%%%%%%%%%AMPM%%%%%%%%%%%%%%%%%%%%%%%%%%%%
input_form( [p�,f�rmiddagen], answer(am_or_pm(am))).
input_form( [f�rmiddagen], answer(am_or_pm(am))).
input_form( [p�, morgonen], answer(am_or_pm(am))).
input_form( [morgon], answer(am_or_pm(am))).
input_form( [f�re,lunch], answer(am_or_pm(am))).
input_form( [p�,natten],answer(am_or_pm(am))).
input_form( [p�,eftermiddagen], answer(am_or_pm(pm))).
input_form( [eftermiddagen], answer(am_or_pm(pm))).
input_form( [efter,lunch], answer(am_or_pm(pm))).
input_form( [p�,kv�llen], answer(am_or_pm(pm))).
input_form( [kv�ll], answer(am_or_pm(pm))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%               TIME EXPRESSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%Time with ampm disambiguation RJ
input_form([lunchtid],[TIME, answer(am_or_pm(am))]):-input_form([klockan, tolv], TIME).
input_form([vid, midnatt],[TIME, answer(am_or_pm(pm))]):-input_form([klockan, tolv], TIME).
input_form( [ikv�ll],[answer(date(today)),answer(am_or_pm(pm))]).
input_form( [i,eftermiddag],[answer(date(today)),answer(am_or_pm(pm))]).

input_form([till|S],answer(time(C))):- input_form(S,answer(time(C))).
input_form([tiden,�r|S],answer(time(C))):- input_form(S,answer(time(C))).
input_form([tid|S],answer(time(C))):- input_form(S,answer(time(C))).
%P� svenska minska tiden med 1 timme
input_form([fem,�ver,halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[trettiofem],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([fem,i,halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[tjugofem],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([klockan, halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[trettio],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([halv|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[trettio],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([kvart,�ver|S1],answer(time(C))):-	
	append(S1,[femton],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([kvart,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[fyrtiofem],S1S2),
	input_form(S1S2, answer(time(C2))).


input_form([tio,�ver|S1],answer(time(C))):-	
	append(S1,[tio],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([tio,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[femtio],S1S2),
	input_form(S1S2, answer(time(C2))).

input_form([fem,�ver|S1],answer(time(C))):-	
	append(S1,[fem],S1S2),
	input_form(S1S2, answer(time(C))).

input_form([fem,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[femtiofem],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([tjugo,�ver|S1],answer(time(C))):-	
	append(S1,[tjugo],S1S2),
	input_form(S1S2, answer(time(C))).
input_form([tjugo,i|S1],answer(time(C2))):-
	input_form(S1,answer(number(C))),
	C1 is C - 1,
	input_form(S3,answer(number(C1))),
	append(S3,[fyrtio],S1S2),
	input_form(S1S2, answer(time(C2))).
input_form([klockan, S1, och, S2], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))), 
	calendar:dayhalf(C,pm).

input_form([S1, S2], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))), 
	calendar:dayhalf(C,pm).

input_form([klockan |[ S1,S2]], [answer(time(C)), answer(am_or_pm(pm))]):-
	input_form([S1,S2], answer(time(C))),
	calendar:dayhalf(C, pm).
input_form([klockan,S1,och,S2], answer(time(C))):-
	input_form([S1,S2], answer(time(C))).
input_form([klockan |[S1,S2]], answer(time(C))):-
	input_form([S1,S2], answer(time(C))).
	%sem_sort(C,_).
	
input_form([klockan| S1], [answer(time(C)),answer(am_or_pm(pm))]):-
	append(S1, [noll], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:dayhalf(C, pm).
input_form([klockan| S1], [answer(time(C)),answer(am_or_pm(pm))]):-
	append(S1, [noll], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:timeknowledge(C, pm).
input_form([klockan| S1], [answer(time(C)),answer(am_or_pm(am))]):-
	append(S1, [noll], S1S2),
	input_form(S1S2, answer(time(C))),
	calendar:timeknowledge(C, am).

input_form([klockan| S1], answer(time(C))):-
	append(S1, [noll], S1S2),
	input_form(S1S2 ,answer(time(C))).


% digital time: two numbers in sequence
input_form( [S1,S2] , answer( time( C ) ) ) :-
	!,
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	(C2S=[_,_] ->
	    C3S=C2S
	;
	    append([48],C2S,C3S)
	),
	append( C1S, C3S, CS ),
	name( C, CS ),
	sem_sort( C, time ).


% time: three numbers in sequence
input_form( [S1,S2,S3] , answer( time( C ) ) ) :-
	lexsem( [S1], C1 ),
	sem_sort( C1, number ),
	lexsem( [S2,S3], C2 ),
	sem_sort( C2, number ),
	name( C1, C1S ),
	name( C2, C2S ),
	append( C1S, C2S, CS),
	name( C, CS ),
	sem_sort( C, time ).


% numbers
input_form( S, answer( number( C ) ) ) :-
	lexsem( S, C ),
	sem_sort( C, number ).
%%To help out with generation
input_form(S,answer(newtime(T))):- input_form(S,answer(time(T))).
input_form(S,answer(olddate(T))):- input_form(S,answer(date(T))).
/*----------------------------------------------------------------------
     lexsem( ?Word, ?Concept )
     -- Lexical semantics
----------------------------------------------------------------------*/

% use semantics as surface forms (only possible for english???)
lexsem( Word, Concept ):-
	synset( Words, Concept ),
	member( Word, Words ).

%AgendaTalk
synset([[m�te],[m�tet],[aff�rsm�te],[lunchm�te]], meeting).
synset([[middag],[middagen],[kv�llsmat],[bjudning]], dinner).
synset([[lunch],[lunchen],[aff�rslunch]], lunch).
synset([[tr�ff],[dejt],[tr�ffen]], appointment).
synset([[konferens], [konferensen], [seminarie]], conference).
synset([[fika],[fikat]],coffee).
synset([[tandl�karbes�k],[tandl�karen],[tandl�kartid],[tandl�kartiden]],dentist).
synset([[klipptid],[frissan],[fris�ren],[klippning],[h�rfris�rskan]],haircut).
synset([[fest],[festen],[kalas],[party],[partyt]],party).
synset([[bio],[biobes�k],[bion]],movie).
synset([[handla],[shoppa],[shopping]],shopping).
synset([[resa],[flygresa],[t�gresa],[flyget], [resan], [resa,bort]],trip).
synset([[tr�ning],[yoga],[workout],[simning],[jogging],[vattengympa], [vattengympan], [taichi],[aerobic],[gymma],[styrketr�na],[tr�ningen],[styrketr�ningen],[yogan]],training).
synset([[presentation],[presentationen]], presentation).
synset([[f�rel�sning],[lektion],[lektionen],[f�rel�sningen]],lecture).
synset([[laboration],[labb],[laborationen],[labben]],lab).
synset([[tenta],[tentan],[skrivning]], exam).
synset([[inl�mning],[inl�mningen]], deadline).
sysnset([[konsert],[konserten]], concert).

synset([[p�,skolan],[i,skolan]],school).
synset([[p�,mitt,rum],[i,mitt,rum],[p�,mitt,kontor],[p�,rummet]],myroom).
synset([[i,akvariet]],aquarium).
synset([[p�,kontoret],[p�,jobbet]],office).
synset([[i,dialoglabbet],[p�,lindholmen]],dialoglab).
synset([[i,kafeet],[p�,kafeet]],cafe).
synset([[p�,m�ssan]],fair).
synset([[hemma],[hemma,hos,mig]],home).
synset([[sal,g,tre,hundra,tolv]],roomG312).
synset([[sal,f,tre,hundra,fjorton]],roomF314).
synset([[maclabbet]],maclab).

%synset([[rum, NR]], Room):- number_phrase(NR,Number), append("rum", Number,Room).
synset( [[idag], [i,dag]],today ).
synset( [[imorgon], [i,morgon], [imorrn]],tomorrow ).
synset( [[i�vermorgon], [i,�vermorgon]],aftertomorrow ).
     
synset( [[m�ndag],[m�ndagens]],monday).
synset( [[tisdag],[tisdagens]],tuesday).
synset( [[onsdag],[onsdagens]],wednesday).
synset( [[torsdag],[torsdagens]],thursday).
synset( [[fredag],[fredagens]],friday).
synset( [[l�rdag],[l�rdagens]],saturday).
synset( [[s�ndag],[s�ndagens]],sunday).

synset( [NumberPhrase], Number ):- number_phrase( NumberPhrase, Number ).
weekDaysSw([m�ndag, tisdag, onsdag, torsdag, fredag, l�rdag, s�ndag]).
weekDaysSwDef([m�ndagens, tisdagens, onsdagens, torsdagens, fredagens, l�rdagens, s�ndagens]).






