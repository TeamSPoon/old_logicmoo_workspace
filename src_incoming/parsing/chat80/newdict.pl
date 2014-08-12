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

:- op(600,xfy,--).
:- op(450,xfy,((:))).
:- op(400,xfy,((&))).
:- op(300,fx,(('`'))).
:- op(200,xfx,((--))).
% =================================================================
% General Dictionary

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
prep(of).
prep(to).
prep(by).
prep(with).
prep(in).
prep(on).
prep(from).
prep(into).
prep(through).

noun_form(Plu,Sin,plu) :- noun_plu(Plu,Sin).
noun_form(Sin,Sin,sin) :- noun_sin(Sin).

verb_form(V,V,inf,_) :- verb_root(V).
verb_form(V,V,pres+fin,Agmt) :-
   regular_pres(V),
   root_form(Agmt),
   verb_root(V).
verb_form(Past,Root,past+_,_) :-
   regular_past(Past,Root).

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

terminator(.,_).
terminator(?,?).
terminator(!,!).

name(Name) :-
   name_template(Name,_), !.




aggr_noun(average,_,_,average).
aggr_noun(sum,_,_,total).
aggr_noun(total,_,_,total).

meta_noun(number,_,V,feature&_,X,P,numberof(X,P,V)).


/* Adjectives */

restriction(african,feature&_,X,african(X)).
restriction(american,feature&_,X,american(X)).
restriction(asian,feature&_,X,asian(X)).
restriction(european,feature&_,X,european(X)).

attribute(large,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(small,feature&place&_,X,measure&area,Y,area(X,Y)).
attribute(great,measure&Type,X,measure&Type,Y,exceeds(X,Y)).

aggr_adj(average,_,_,average).
aggr_adj(total,_,_,total).
aggr_adj(minimum,_,_,minimum).
aggr_adj(maximum,_,_,maximum).

/* Measure */

measure(ksqmile,measure&area,[],ksqmiles).
measure(sqmile,measure&area,[],sqmiles).
measure(degree,measure&position,[],degrees).
measure(thousand,measure&heads,[],thousand).
measure(million,measure&heads,[],million).

units(large,measure&_).
units(small,measure&_).

sign(large,+).
sign(small,-).
sign(great,+).

/* Proportions and the like */

comparator(proportion,_,V,[],proportion(V)).
comparator(percentage,_,V,[],proportion(V)).


% =================================================================
% Specialised Dictionary

loc_pred(east,prep(eastof)).
loc_pred(west,prep(westof)).
loc_pred(north,prep(northof)).
loc_pred(south,prep(southof)).

adj(minimum,restr).
adj(maximum,restr).
adj(average,restr).
adj(total,restr).

adj(great,quant).
adj(big,quant).
adj(small,quant).
adj(large,quant).
adj(old,quant).
adj(new,quant).
adj(great,quant).

rel_adj(greater,great).
rel_adj(less,small).
rel_adj(bigger,big).
rel_adj(smaller,small).
rel_adj(larger,large).
rel_adj(older,old).
rel_adj(newer,new).

sup_adj(biggest,big).
sup_adj(smallest,small).
sup_adj(largest,large).
sup_adj(oldest,old).
sup_adj(newest,new).

noun_form(proportion,proportion,_).
noun_form(percentage,percentage,_).

noun_sin(average).
noun_sin(total).
noun_sin(sum).
noun_sin(degree).
noun_sin(sqmile).
noun_sin(ksqmile).
noun_sin(thousand).
noun_sin(million).
noun_sin(time).
noun_sin(number).


noun_plu(averages,average).
noun_plu(totals,total).
noun_plu(sums,sum).
noun_plu(degrees,degree).
noun_plu(sqmiles,sqmile).
noun_plu(ksqmiles,ksqmile).
noun_plu(million,million).
noun_plu(thousand,thousand).
noun_plu(times,time).
noun_plu(numbers,number).

regular_pres(exceed).

regular_past(exceeded,exceed).

verb_form(exceeds,exceed,pres+fin,3+sin).
verb_form(exceeding,exceed,pres+part,_).


adverb(yesterday).
adverb(tomorrow).



exceeds(X--U,Y--U) :- !, X > Y.
exceeds(X1--U1,X2--U2) :- ratio(U1,U2,M1,M2), X1*M1 > X2*M2.

ratio(thousand,million,1,1000).
ratio(million,thousand,1000,1).
ratio(ksqmiles,sqmiles,1000,1).
ratio(sqmiles,ksqmiles,1,1000).

