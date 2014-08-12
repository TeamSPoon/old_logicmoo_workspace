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
:- style_check(-discontiguous).

% =================================================================
% General Dictionary



% LEXICAL Data from newdic.pl
% ---------------------------


noun_sin(place).
noun_sin(area).
noun_sin(capital).
noun_sin(city).
noun_sin(continent).
noun_sin(country).
noun_sin(latitude).
noun_sin(longitude).
noun_sin(ocean).
noun_sin(person).
noun_sin(population).
noun_sin(region).
noun_sin(river).
noun_sin(sea).
noun_sin(seamass).

noun_plu(places,place).
noun_plu(areas,area).
noun_plu(capitals,capital).
noun_plu(cities,city).
noun_plu(continents,continent).
noun_plu(countries,country).
noun_plu(latitudes,latitude).
noun_plu(longitudes,longitude).
noun_plu(oceans,ocean).
noun_plu(persons,person).  noun_plu(people,person).
noun_plu(populations,population).
noun_plu(regions,region).
noun_plu(rivers,river).
noun_plu(seas,sea).
noun_plu(seamasses,seamass).

verb_root(border).
verb_root(contain).
verb_root(drain).
verb_root(exceed).
verb_root(flow).
verb_root(rise).
verb_root(govern).

regular_pres(rise).

verb_form(rises,rise,pres+fin,3+sin).
verb_form(rose,rise,past+fin,_).
verb_form(risen,rise,past+part,_).

regular_pres(border).

regular_past(bordered,border).

verb_form(borders,border,pres+fin,3+sin).
verb_form(bordering,border,pres+part,_).

regular_pres(contain).

regular_past(contained,contain).

verb_form(contains,contain,pres+fin,3+sin).
verb_form(containing,contain,pres+part,_).

regular_pres(drain).

regular_past(drained,drain).

verb_form(drains,drain,pres+fin,3+sin).
verb_form(draining,drain,pres+part,_).

regular_pres(govern).

regular_past(governed,govern).

verb_form(governs,govern,pres+fin,3+sin).
verb_form(governing,govern,pres+part,_).

verb_type(rise,main+intrans).
verb_type(border,main+trans).
verb_type(contain,main+trans).
verb_type(drain,main+intrans).
verb_type(exceed,main+trans).
verb_type(govern,main+trans).

regular_pres(flow).

regular_past(flowed,flow).

verb_form(flows,flow,pres+fin,3+sin).
verb_form(flowing,flow,pres+part,_).

verb_type(flow,main+intrans).


% =================================================================
% Specialised Dictionary

loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).

adj(african,restr).
adj(american,restr).
adj(asian,restr).
adj(european,restr).

