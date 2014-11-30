
:-swi_module(logicmooCURT,[]).

:-swi_export('^'/2).
:-meta_predicate('^'(+,0)).
'^'(_X,Call):-call(Call).


lexEntry(pn,[symbol:China,syntax:[China]]):-name_db(China).
lexEntry(det,[syntax:[A],mood:decl,num:Sg,type:Indef]):-det_db(A,Sg,A,Indef).

lexEntry(TV,[symbol:Have,syntax:[Have],inf:inf,num:sg]):-verb_type_db(Have,main+TV),verb_form_db(Have,Have,inf,_).
lexEntry(TV,[symbol:Have,syntax:[Has],inf:Fin,num:Sg]):-verb_type_db(Have,main+TV),verb_form_db(Has,Have,pres+Fin,3+Sg).
lexEntry(TV,[symbol:Have,syntax:[Have],inf:fin,num:pl]):-verb_type_db(Have,main+TV),verb_form_db(Have,Have,inf,_).


