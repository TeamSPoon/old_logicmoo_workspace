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
:-discontiguous(verb_type/2).
:-discontiguous(verb_script/2).
:-discontiguous(verb_root/1).
:-discontiguous(verb_form/4).
:-discontiguous(trans/9).
:-discontiguous(regular_pres/1).
:-discontiguous(regular_past/2).
:-discontiguous(noun_form/3).
:-discontiguous(loc_pred/2).

:- style_check(+discontiguous).
:- style_check(-discontiguous).
:- op(600,xfy,--).
:- op(450,xfy,((:))).
:- op(400,xfy,((&))).
:- op(300,fx,(('`'))).
:- op(200,xfx,((--))).

:- dynamic_multifile_exported((moo:trans/9)).

:- begin_dynamic_reader.
:- asserta((thlocal:enable_src_loop_checking)).



% =================================================================
% General Dictionary
% LEXICAL Data from newdic.pl

terminator(.,_).
terminator(?,?).
terminator(!,!).

plt :- thlocal:usePlTalk.

adverb(Quickly):-plt,talk_db(adverb,Quickly).

conj(and).
conj(or).


int_pron(what,undef).
int_pron(which,undef).
int_pron(who,subj).
int_pron(whom,compl).

int_art(what,X,_,int_det(X)).
int_art(which,X,_,int_det(X)).

det(the,No,the(No),def).
det(a,sin,a,indef).
det(an,sin,a,indef).
det(every,sin,every,indef).
det(some,_,some,indef).
det(any,_,any,indef).
det(all,plu,all,indef).
det(each,sin,each,indef).
det(no,_,no,indef).

number(W,I,Nb) :-
   tr_number(W,I),
   ag_number(I,Nb).

tr_number(nb(I),I).
tr_number(one,1).
tr_number(two,2).
tr_number(three,3).
tr_number(four,4).
tr_number(five,5).
tr_number(six,6).
tr_number(seven,7).
tr_number(eight,8).
tr_number(nine,9).
tr_number(ten,10).

ag_number(1,sin).
ag_number(N,plu) :- N>1.

quantifier_pron(everybody,every,person).
quantifier_pron(everyone,every,person).
quantifier_pron(everything,every,thing).
quantifier_pron(somebody,some,person).
quantifier_pron(someone,some,person).
quantifier_pron(something,some,thing).
quantifier_pron(anybody,any,person).
quantifier_pron(anyone,any,person).
quantifier_pron(anything,any,thing).
quantifier_pron(nobody,no,person).
quantifier_pron(nothing,no,thing).

prep(as).
prep(at).
noun_plu(times,time).
adverb(yesterday).
adverb(tomorrow).

prep(of).
prep(to).
prep(by).
prep(with).
prep(in).
prep(on).
prep(from).
prep(into).
prep(through).
prep(Above):-plt,talk_db(prep,Above).

noun_form(Plu,Sin,plu) :- noun_plu(Plu,Sin).
noun_form(Sin,Sin,sin) :- noun_sin(Sin).

verb_form(V,V,inf,_) :- verb_root(V).
verb_form(V,V,pres+fin,Agmt) :-
   regular_pres(V),
   root_form(Agmt),
   verb_root(V).
verb_form(Past,Verb,past+_,_) :-
   regular_past(Past,Verb).

root_form(1+sin).
root_form(2+_).
root_form(1+plu).
root_form(3+plu).

verb_root(be).
verb_root(have).
verb_root(do).
   
verb_form(am,be,pres+fin,1+sin).
verb_form(are,be,pres+fin,2+sin).
verb_form(is,be,pres+fin,3+sin).
verb_form(are,be,pres+fin,_+plu).
verb_form(was,be,past+fin,1+sin).
verb_form(were,be,past+fin,2+sin).
verb_form(was,be,past+fin,3+sin).
verb_form(were,be,past+fin,_+plu).
verb_form(been,be,past+part,_).
verb_form(being,be,pres+part,_).

verb_type(be,aux+be).

regular_pres(have).

regular_past(had,have).

verb_form(has,have,pres+fin,3+sin).
verb_form(having,have,pres+part,_).

verb_type(have,aux+have).

regular_pres(do).

verb_form(does,do,pres+fin,3+sin).
verb_form(did,do,past+fin,_).
verb_form(doing,do,pres+part,_).
verb_form(done,do,past+part,_).

verb_type(do,aux+ditrans).

rel_pron(who,subj).
rel_pron(whom,compl).
rel_pron(which,undef).

poss_pron(my,_,1,sin).
poss_pron(your,_,2,_).
poss_pron(his,masc,3,sin).
poss_pron(her,fem,3,sin).
poss_pron(its,neut,3,sin).
poss_pron(our,_,1,plu).
poss_pron(their,_,3,plu).

pers_pron(i,_,1,sin,subj).
pers_pron(you,_,2,_,_).
pers_pron(he,masc,3,sin,subj).
pers_pron(she,fem,3,sin,subj).
pers_pron(it,neut,3,sin,_).
pers_pron(we,_,1,plu,subj).
pers_pron(them,_,3,plu,subj).
pers_pron(me,_,1,sin,compl(_)).
pers_pron(him,masc,3,sin,compl(_)).
pers_pron(her,fem,3,sin,compl(_)).
pers_pron(us,_,1,plu,compl(_)).
pers_pron(them,_,3,plu,compl(_)).


% =================================================================
% PROPER INSTANCES OF
% =================================================================

noun_plu(places,place).

noun_plu(P,S):-plt,talk_db(noun1,S,P).
noun_plu(continents,continent).
noun_plu(oceans,ocean).
noun_plu(regions,region).
noun_plu(rivers,river).
noun_plu(seas,sea).
noun_plu(seamasses,seamass).

noun_plu(PluralString,SingularString):- meetsForm80(PluralString,SingularString,noun+plural).
noun_sin(Singular):- meetsForm80(Singular,Singular,noun+singular).
noun_sin(Singular):-noun_plu(_,Singular).

thing(continent,feature&place&continent,X,continent(X),[],_).
thing(ocean,feature&place&seamass,X,ocean(X),[],_).
thing(river,feature&river,X,river(X),[],_).
thing(sea,feature&place&seamass,X,sea(X),[],_).
thing(seamass,feature&place&seamass,X,seamass(X),[],_).
thing(region,feature&place&_,X,region80(X),[],_).

/* WHICH WHICH DENOTES A  */

thing(place,feature&place&_,X,place(X),[],_).

/* WHICH EXISTENCE STEMS FROM A  */

place(X) :- continent(X); region80(X); seamass(X); country(X).
region80(R) :- in_continent(R,_).


continent(africa).
continent(america).
continent(antarctica).
continent(asia).
continent(australasia).
continent(europe).

circle_of_latitude(equator).
circle_of_latitude(tropic_of_cancer).
circle_of_latitude(tropic_of_capricorn).
circle_of_latitude(arctic_circle).
circle_of_latitude(antarctic_circle).


seamass(X) :- ocean(X).
seamass(X) :- sea(X).

ocean(arctic_ocean).
ocean(atlantic).
ocean(indian_ocean).
ocean(pacific).
ocean(southern_ocean).

sea(baltic).
sea(black_sea).
sea(caspian).
sea(mediterranean).
sea(persian_gulf).
sea(red_sea).

river(R) :- river(R,_L).

in_continent(north_america, america).

% ------------------------------
% "Whoable Count Nouns" 
% ------------------------------
noun_plu(persons,person).  noun_plu(people,person).
thing(person,_,X,person(X),[],_).




/* A CAPITOL  */


noun_plu(capitals,capital).
thing(capital,feature&city,X,capital(X),[],_).
property(capital,feature&city,X,feature&place&country,Y, capital(Y,X),[],_,_).
capital(C,Cap) :- country(C,_,_,_,_,_,Cap,_).
capital(C) :- capital(_X,C).

/* IS A SPECILIZATION OF A CITY */

noun_plu(cities,city).
thing(city,feature&city,X,city(X),[],_).
city(C) :- city(C,_,_).
city(tehran,iran,1010).

/* THAT INVOKES ACTION */

trans(govern,feature&_,X,feature&place&country,Y,capital(Y,X),[],_,_).
verb_root(govern).
regular_pres(govern).
regular_past(governed,govern).
verb_form(governs,govern,pres+fin,3+sin).
verb_form(governing,govern,pres+part,_).
verb_type(govern,main+trans).

/* UPON A  */

noun_plu(countries,country).
thing(country,feature&place&country,X,country(X),[],_).
country(C) :- country(C,_,_,_,_,_,_,_).
country(iran,middle_east,33,-53,636363,32001000,tehran,rial).


% =================================================================
% Having Referant Proper nouns
% =================================================================

name(Name) :-
   name_template(Name,_), !.

name_template(X,feature&circle) :- circle_of_latitude(X).
name_template(X,feature&city) :- city(X).
name_template(X,feature&place&continent) :- continent(X).
name_template(X,feature&place&country) :- country(X).
name_template(X,feature&place&_) :- region80(X).
name_template(X,feature&river) :- river(X).
name_template(X,feature&place&seamass) :- seamass(X).


% =================================================================
% FACETS (Adjectives) 
% =================================================================


restriction(african,feature&_,X,african(X)).
restriction(american,feature&_,X,american(X)).
restriction(asian,feature&_,X,asian(X)).
restriction(european,feature&_,X,european(X)).
african(X) :- in(X,africa).
american(X) :- in(X,america).
asian(X) :- in(X,asia).
european(X) :- in(X,europe).
adj(african,restr).
adj(american,restr).
adj(asian,restr).
adj(european,restr).


restriction(AdjNounEuropean,feature&_,X,adjIsa(X,AdjNounEuropean)):- adj(AdjNounEuropean,restr).
adjIsa(E,C):-isa_backchaing(E,C).
adj(AdjNounEuropean,restr):- plt,talk_db(adj,AdjNounEuropean),talk_db(noun1,AdjNounEuropean,_).
adj(AdjRestr,restr):-plt,talk_db(adj,AdjRestr),not(adj(AdjRestr,quant)),not(adverb(AdjRestr)).
adj(AdjRestr,restr):-meetsForm80(AdjRestr,AdjRestr,form80(adj+restr)).


verb_script(Verb,main+trans):- trans(Verb,_TypeS,_S,_TypeD,_D,_Pred,_Slots,_SlotD,_).
verb_script(Verb,main+intrans):- intrans(Verb,TypeS,S,Pred,Slots,_).
verb_script(Verb,main+ditrans):- ditrans(Verb,TypeS,S,TypeD,D,TypeI,I,Pred,Slots,SlotD,SlotI,_).


verb_script(Verb,main+trans):- plt,talk_db(transitive,Verb,_,_,_,_).
verb_script(Verb,main+intrans):-plt,talk_db(intransitive,Verb,_,_,_,_).

:-style_check(-singleton).

verb_root(Govern):-plt,talk_db(_Verb_i,Govern,_Governs,_GovernedImperfect,_Governing,_Governed).
regular_pres(Govern):-plt,talk_db(_,Govern,_Governs,_GovernedImperfect,_Governing,_Governed).
regular_past(Governed,Govern):-plt,talk_db(_,Govern,_Governs,_GovernedImperfect,_Governing,Governed).
verb_form(Active,Verb,pres+part,_):-plt,talk_db(_,Verb,VerbPL,Imperfect,Active,PastPart).
verb_form(VerbPL,Verb,pres+fin,3+sin):-plt,talk_db(_,Verb,VerbPL,Imperfect,Active,PastPart).
verb_form(Imperfect,Verb,past+fin,_):-plt,talk_db(_,Verb,VerbPL,Imperfect,Active,PastPart).
verb_form(PastPart,Verb,past+part,_):-plt,talk_db(_,Verb,VerbPL,Imperfect,Active,PastPart).

verb_root(Verb):-meetsForm80(Verb,Verb,form80(verb+root)).
verb_root(Verb):-meetsForm80(Verb,Verb,form80(3+sin)).
verb_script(Verb,MainPlusTrans):-verb_root(Verb),meetsForm80(_Form,Verb,form80(MainPlusTrans,main+trans)).
regular_pres(Verb):- meetsForm80(Verb,Verb,form80(regular_pres)).
regular_past(Form,Verb):- meetsForm80(Form,Verb,form80(regular_past)).
verb_form(Form,Verb,AsPresFin,As3_plus_sin):- meetsForm80(Form,Verb,form80(AsPresFin,pres+fin)), meetsForm80(Verb,Verb,form80(As3_plus_sin,3+sin)).
verb_form(Form,Verb,TensePlusPart,_):- meetsForm80(Form,Verb,form80(TensePlusPart)).
regular_pres(Verb):- verb_root(Verb).

:-style_check(+singleton).



verb_root(border).
regular_pres(border).
regular_past(bordered,border).
verb_form(borders,border,pres+fin,3+sin).
verb_form(bordering,border,pres+part,_).
verb_type(border,main+trans).
trans(border, feature&place&_,X,feature&place&_,Y,borders(X,Y),[],_,_).
borders(X,C) :- var(X), nonvar(C), !, borders(C,X).
borders(afghanistan,iran).
borders(iran,afghanistan).


/* THAT HAS COUNTABLE ATTRIBUTES SUCH AS.. */

thing(longitude,measure&position,X,longitude80(X),[],_).
thing(latitude,measure&position,X,latitude80(X),[],_).
property(longitude,measure&position,X,feature&_,Y,longitude80(Y,X),[],_,_).
property(latitude, measure&position,X,feature&_,Y,latitude80(Y,X),[],_,_).
noun_plu(longitudes,longitude). noun_plu(latitudes,latitude).

longitude(C,L--degrees) :- country(C,_,_,L,_,_,_,_).
latitude80(C,L--degrees) :- country(C,_,L,_,_,_,_,_).

longitude(_X--degrees).
latitude80(_X--degrees).

latitude80(tropic_of_capricorn,-23--degrees).
latitude80(tropic_of_cancer,23--degrees).
latitude80(equator,0--degrees).
latitude80(arctic_circle,67--degrees).
latitude80(antarctic_circle,-67--degrees).


% ------------------------------
% "N/S/E/W/of" 
% ------------------------------
loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).
adjunction(eastof,feature&_-X,feature&_-Y,eastof(X,Y)).
adjunction(westof,feature&_-X,feature&_-Y,westof(X,Y)).
adjunction(northof,feature&_-X,feature&_-Y,northof(X,Y)).
adjunction(southof,feature&_-X,feature&_-Y,southof(X,Y)).
eastof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L2,L1).
northof(X1,X2) :- latitude80(X1,L1), latitude80(X2,L2), exceeds(L1,L2).
southof(X1,X2) :- latitude80(X1,L1), latitude80(X2,L2), exceeds(L2,L1).
westof(X1,X2) :- longitude(X1,L1), longitude(X2,L2), exceeds(L1,L2).



% ------------------------------
% "Population is having a quantitity"
% ------------------------------
noun_plu(populations,population).
thing(population,measure&countables,X,population(X),[],_).

property(population, measure&countables,X,feature&_,Y,population(Y,X),[],_,_).

population(C,P--thousand) :- city(C,_,P).
population(C,P--million) :- country(C,_,_,_,_,P0,_,_), P is integer(P0/1.0E6).

population(_X--million).
population(_X--thousand).

measure(thousand,measure&countables,[],thousand).
measure(million,measure&countables,[],million).


% ------------------------------
% "Contains" Inversion of the 'in' relation.
% ------------------------------
verb_root(contain).
verb_type(contain,main+trans).
regular_pres(contain).
regular_past(contained,contain).
verb_form(contains,contain,pres+fin,3+sin).
verb_form(containing,contain,pres+part,_).
trans(contain,feature&place&_,X,feature&_,Y,in(Y,X),[],_,_).

contains(X,Y) :- contains0(X,Y).
contains(X,Y) :- contains0(X,W), contains(W,Y).

contains0(america,north_america).

% ------------------------------
% "In" 
% ------------------------------
contextWH(in,place,where).

adjunction(in,feature&_-X,feature&place&_-Y,in(X,Y)).

in(X,Y) :- var(X), nonvar(Y), !, contains(Y,X).
in(X,Y) :- in0(X,W), ( W=Y ; in(W,Y) ).

in0(X,Y) :- in_continent(X,Y).
in0(X,Y) :- city(X,Y,_).
in0(X,Y) :- country(X,Y,_,_,_,_,_,_).
in0(X,Y) :- flows(X,Y).

in_continent(middle_east,  asia).


% =================================================================
% INTERACTION OF TYPES
% =================================================================



/* Verbs */

verb_root(rise).
regular_pres(rise).
verb_form(rises,rise,pres+fin,3+sin).
verb_form(rose,rise,past+fin,_).
verb_form(risen,rise,past+part,_).
verb_type(rise,main+intrans).
intrans(rise,feature&river,X,rises(X,Y), [slot(prep(in),feature&place&_,Y,_,free)],_).
rises(R,C) :- river(R,L), last(L,C).


verb_root(drain).
regular_pres(drain).
regular_past(drained,drain).
verb_form(drains,drain,pres+fin,3+sin).
verb_form(draining,drain,pres+part,_).
verb_type(drain,main+intrans).
intrans(drain,feature&river,X,drains(X,Y), [slot(prep(into),feature&place&_,Y,_,free)],_).
drains(R,S) :- river(R,L), first(L,S).



verb_root(flow).
regular_pres(flow).
regular_past(flowed,flow).
verb_form(flows,flow,pres+fin,3+sin).
verb_form(flowing,flow,pres+part,_).
verb_type(flow,main+intrans).
intrans(flow,feature&river,X,flows(X,Y), [slot(prep(through),feature&place&_,Y,_,free)],_).
intrans(flow,feature&river,X,flows(X,Y,Z), [slot(prep(into),feature&place&_,Z,_,free), slot(prep(from),feature&place&_,Y,_,free)],_). 
flows(R,C) :- flows(R,C,_).
flows(R,C1,C2) :- river(R,L), flow_links(L,C2,C1).
flow_links([X1,X2|_],X1,X2).
flow_links([_|L],X1,X2) :- flow_links(L,X1,X2).



% ------------------------------
/* Measure of Mass */
% ------------------------------

noun_plu(areas,area).
thing(area,measure&area,X,area(X),[],_).
property(area,measure&area,X,feature&place&_,Y,area(Y,X),[],_,_).
area(C,A--ksqmiles) :- country(C,_,_,_,A0,_,_,_), A is integer(A0/1000).
area(_X--ksqmiles).


measure(sqmile,measure&area,[],sqmiles).
measure(ksqmile,measure&area,[],ksqmiles).
ratio(sqmiles,ksqmiles,1,1000).
ratio(ksqmiles,sqmiles,1000,1).
noun_plu(ksqmiles,ksqmile).
noun_plu(sqmiles,sqmile).


/* Measure of Proportions and the like */
noun_form(proportion,proportion,_).
comparator(proportion,_,V,[],proportion(V)).
noun_plu(degrees,degree).
measure(degree,measure&position,[],degrees).
comparator(percentage,_,V,[],proportion(V)).
noun_form(percentage,percentage,_).
noun_plu(thousand,thousand).
noun_plu(million,million).
ratio(million,thousand,1000,1).
ratio(thousand,million,1,1000).


adj(average,restr).
aggr_adj(average,_,_,average).
noun_plu(averages,average).
aggr_noun(average,_,_,average).


aggr_adj(minimum,_,_,minimum).
adj(minimum,restr).
aggr_adj(maximum,_,_,maximum).
adj(maximum,restr).

meta_noun(number,_,V,feature&_,X,P,numberof(X,P,V)).
noun_plu(numbers,number).


adj(total,restr).
noun_plu(totals,total).
aggr_adj(total,_,_,total).
aggr_noun(total,_,_,total).
aggr_noun(sum,_,_,total).
noun_plu(sums,sum).


/* Measure of Greater or Lesser amounts*/
verb_root(exceed).
verb_type(exceed,main+trans).
regular_pres(exceed).
regular_past(exceeded,exceed).
verb_form(exceeds,exceed,pres+fin,3+sin).
verb_form(exceeding,exceed,pres+part,_).
trans(exceed,measure&Type,X,measure&Type,Y,exceeds(X,Y),[],_,_).
attribute(great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).
exceeds(X--U,Y--U) :- !, X > Y.
exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.

sup_adj(Biggest,Big):-plt,talk_db(superl,Big,Biggest).

% /* Comparative */
rel_adj(Bigger,Big):-plt,talk_db(comp,Big,Bigger).
attribute(small,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(large,feature&place&_,X,measure&area,Y,area(X,Y)).
units(small,measure&_).
units(large,measure&_).
rel_adj(smaller,small).
sup_adj(smallest,small).
rel_adj(larger,large).
sup_adj(largest,large).
adj_sign(large,+).
adj_sign(small,-).
adj_sign(great,+).

rel_adj(less,small).
rel_adj(greater,great).


rel_adj(bigger,big).
sup_adj(biggest,big).
adj(small,quant).
adj(large,quant).
adj(great,quant).
adj(big,quant).

adj(Big,quant):-plt,talk_db(superl,Big,_Biggest).
adj(Big,quant):-plt,talk_db(comp,Big,_Bigger).

adj(old,quant).
adj(new,quant).
rel_adj(older,old).
sup_adj(oldest,old).
rel_adj(newer,new).
sup_adj(newest,new).



/*
We can parse:

which is the largest X?

but also need to parse:

which X is the largest?


*/
