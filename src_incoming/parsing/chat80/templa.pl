/*
 _________________________________________________________________________
|	Copyright (C) 1982						  |
|									  |
|	David Warren,							  |
|		SRI International, 333 Ravenswood Ave., Menlo Park,	  |
|		California 94025, USA;					  |
|									  |
|	Fernando Pereira,						  |
|		Dept. of Architecture, University of Edinburgh,		  |
|		20 Chambers St., Edinburgh EH1 1JZ, Scotland		  |
|									  |
|	This program may be used, copied, altered or included in other	  |
|	programs only for academic purposes and provided that the	  |
|	authorship of the initial program is aknowledged.		  |
|	Use for commercial purposes without the previous written 	  |
|	agreement of the authors is forbidden.				  |
|_________________________________________________________________________|

*/

:- begin_dynamic_reader.

/* Nouns */

property_template80(area,measure&area,X,feature&place&_,Y,area(Y,X),[],_,_).
property_template80(capital,feature&city,X,feature&place&country,Y, capital(Y,X),[],_,_).
property_template80(latitude, measure&position,X,feature&_,Y,latitude(Y,X),[],_,_).
property_template80(longitude,measure&position,X,feature&_,Y, longitude(Y,X),[],_,_).
property_template80(population, measure&heads,X,feature&_,Y,population(Y,X),[],_,_).

thing_template80(place,feature&place&_,X,place(X),[],_).
thing_template80(person,_,X,person(X),[],_).

/* Proper nouns */

name_template(X,feature&person) :- catch(person(X),_,fail).

/* Verbs */

trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).

intrans(drain,feature&river,X,drains(X,Y),    [slot(prep(into),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y),    [slot(prep(through),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y,Z),    [slot(prep(into),feature&place&_,Z,_,free),     slot(prep(from),feature&place&_,Y,_,free)],_).
intrans(rise,feature&river,X,rises(X,Y),    [slot(prep(in),feature&place&_,Y,_,free)],_).

/* Prepositions */

adjunction(in,feature&_-X,feature&place&_-Y,in(X,Y)).
adjunction(eastof,feature&_-X,feature&_-Y,eastof(X,Y)).
adjunction(westof,feature&_-X,feature&_-Y,westof(X,Y)).
adjunction(northof,feature&_-X,feature&_-Y,northof(X,Y)).
adjunction(southof,feature&_-X,feature&_-Y,southof(X,Y)).

:- end_dynamic_reader.

