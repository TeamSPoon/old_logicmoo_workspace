%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )).
:- 
  module(common_logic_sexpr,[
  codelist_to_forms/2,
  svar_fixvarname/2,
  current_input_to_forms/2,
  input_to_forms/3,
  input_to_forms_debug/1,
  sexpr_sterm_to_pterm_list/2,
  sexpr//1,
  fixvars/4,
  txt_to_codes/2,
  with_lisp_translation/2,
  to_untyped/2,
  ok_varname/1,
  svar_fixvarname/2,
  sexpr_sterm_to_pterm/2,
  lisp_read_from_stream/2,
  parse_sexpr/2]).

%:- endif.

 :- meta_predicate with_lisp_translation(*,2).
 :- meta_predicate see_seen(0).

see_seen(_):-!.
see_seen(G):-call(G).

:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.

/*
:- prolog_load_context(directory,Dir),
   DirFor = plarkc,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- attach_packs.
:- initialization(attach_packs).
*/

% [Required] Load the Logicmoo Library Utils
% = % :- ensure_loaded(logicmoo(logicmoo_utils)).

% % :- ensure_loaded(logicmoo(plarkc/mpred_cyc_api)).


:- export(fixvars/4).

%= 	 	 

%% fixvars( ?P, ?VALUE2, :TermARG3, ?P) is semidet.
%
% Fixvars.
%
fixvars(P,_,[],P):-!.
fixvars(P,N,[V|VARS],PO):-  
     atom_string(Name,V),clip_qm(Name,NB),Var = '$VAR'(NB),
     subst(P,'$VAR'(N),Var,PM0),
     subst(PM0,'$VAR'(Name),Var,PM),
   %  (get_varname_list(Vs)->true;Vs=[]),
  %   append(Vs,[Name=Var],NVs),
  %   nput_variable_names( NVs),
     N2 is N + 1, fixvars(PM,N2,VARS,PO).



%= 	 	 

%% clip_us( ?A, ?AO) is semidet.
%
% Clip Us.
%
clip_us(A,AO):-concat_atom(L,'-',A),concat_atom(L,'_',AO).

%= 	 	 

%% clip_qm( ?QA, ?AO) is semidet.
%
% Clip Qm.
%
clip_qm(QA,AO):-atom_concat('??',A1,QA),!,atom_concat('_',A1,A),clip_us(A,AO).
clip_qm(QA,AO):-atom_concat('?',A,QA),!,clip_us(A,AO).
clip_qm(A,AO):-clip_us(A,AO).

:- meta_predicate(sexpr_sterm_to_pterm(?,?)).
:- meta_predicate(sexpr_sterm_to_pterm_list(?,?)).

is_relation_sexpr('=>').
is_relation_sexpr('<=>').
is_relation_sexpr('==>').
is_relation_sexpr('<==>').
is_relation_sexpr('not').
is_relation_sexpr(typeGenls).

is_va_relation('or').
is_va_relation('and').
%= 	 	 

%:- baseKB:ensure_loaded(logicmoo('plarkc/logicmoo_i_cyc_rewriting')).

maybe_var(S,Name,'$VAR'(Name)):- S=='?',atom(Name),!.

%% sexpr_sterm_to_pterm( ?VAR, ?V) is semidet.
%
% S-expression Sterm Converted To Pterm.
%
sexpr_sterm_to_pterm(VAR,'$VAR'(V)):- atom(VAR),atom_concat('?',_,VAR),clip_qm(VAR,V),!.
sexpr_sterm_to_pterm(VAR,kw((V))):- atom(VAR),atom_concat(':',V2,VAR),clip_qm(V2,V),!.
sexpr_sterm_to_pterm(VAR,VAR):-is_ftVar(VAR),!.
% sexpr_sterm_to_pterm(List,PTERM):- append(Left,[S,Name|TERM],List),maybe_var(S,Name,Var),!,append(Left,[Var|TERM],NewList), sexpr_sterm_to_pterm(NewList,PTERM).
% sexpr_sterm_to_pterm([S|TERM],dot_holds(PTERM)):- \+ (is_list(TERM)),sexpr_sterm_to_pterm_list([S|TERM],PTERM),!.
%sexpr_sterm_to_pterm([S|TERM],PTERM):- \+ atom(S),sexpr_sterm_to_pterm_list([S|TERM],PTERM),!.
/*
sexpr_sterm_to_pterm([S,Vars|TERM],PTERM):- nonvar(S),
   call_if_defined(common_logic_snark:is_quantifier(S)),
   must_det_l((sexpr_sterm_to_pterm_list(TERM,PLIST),
   PTERM=..[S,Vars|PLIST])),!.
*/
sexpr_sterm_to_pterm([S|TERM],PTERM):- !, sexpr_sterm_to_pterm_list([S|TERM],PLIST),s_univ(PTERM,PLIST),!.


sexpr_sterm_to_pterm(Const,NConst):-atom(Const),rename_sumo(Const,NConst),!.
sexpr_sterm_to_pterm(Const,NConst):-string(Const),string_to_mws(Const,NConst),!.
sexpr_sterm_to_pterm(VAR,VAR).

% for SUMO
rename_sumo('Collection','ttSumoCollection').
rename_sumo(format,formatSumo).
% rename_sumo(documentation,comment).
rename_sumo('instance', isa).
rename_sumo('subclass', genls).
rename_sumo('domain', 'argIsa').
rename_sumo('disjoint', 'disjointWith').

rename_sumo('range', 'resultIsa').
rename_sumo('domainSubclass', 'argGenl').
rename_sumo('rangeSubclass', 'resultGenl').
rename_sumo(immediateInstance,nearestIsa).
rename_sumo('partition', 'sumo_partition').
rename_sumo('Entity','tThing').
rename_sumo('ListFn',vTheListFn).
rename_sumo('ListOrderFn',vSumoListOrderFn).
rename_sumo('AssignmentFn',uFn).
rename_sumo('SymbolicString',ftString).
rename_sumo('Attribute','sumoAttribute').
rename_sumo('EnglishLanguage','vEnglishLanguage').
rename_sumo('Formula','ftFormula').
rename_sumo('Function','rtFunction').
rename_sumo(forall,all).
rename_sumo(subrelation,genlPreds).
rename_sumo('Class','tSet').
rename_sumo('SetOrClass', 'tCol').
rename_sumo(I,O):- builtin_rn_or_rn_new(I,O),!.



%sexpr_sterm_to_pterm([S|TERM],PTERM):- (number(S);  (atom(S),fail,atom_concat(_,'Fn',S))),sexpr_sterm_to_pterm_list([S|TERM],PTERM),!.            
%sexpr_sterm_to_pterm([S],O):- is_ftVar(S),sexpr_sterm_to_pterm(S,Y),!,s_univ(O,[Y]),!.
%sexpr_sterm_to_pterm([S],O):- nonvar(S),sexpr_sterm_to_pterm(S,Y),!,s_univ(O,[Y]),!.
%sexpr_sterm_to_pterm([S|TERM],PTERM):- is_ftVar(S), sexpr_sterm_to_pterm_list(TERM,PLIST),s_univ(PTERM,[t,S|PLIST]),!.
%sexpr_sterm_to_pterm([S|TERM],PTERM):- \+ atom(S), sexpr_sterm_to_pterm_list(TERM,PLIST),s_univ(PTERM,[t,S|PLIST]),!.
%sexpr_sterm_to_pterm([S|TERM],PTERM):- S==and,!,must_det_l((maplist(sexpr_sterm_to_pterm,TERM,PLIST),list_to_conjuncts(',',PLIST,PTERM))).
% sexpr_sterm_to_pterm([S|TERM],PTERM):- is_va_relation(S),!,must_det_l((maplist(sexpr_sterm_to_pterm,TERM,PLIST),list_to_conjuncts(S,PLIST,PTERM))).
%sexpr_sterm_to_pterm([S|TERM],PTERM):- is_relation_sexpr(S),must_det_l((sexpr_sterm_to_pterm_list(TERM,PLIST),PTERM=..[S|PLIST])),!.
%sexpr_sterm_to_pterm(STERM,PTERM):- STERM=..[S|TERM],sexpr_sterm_to_pterm_list(TERM,PLIST),s_univ(PTERM,[S|PLIST]),!.


s_univ(P,[F|ARGS]):- atom(F),is_list(ARGS),length(ARGS,A),l_arity(F,A),P=..[F|ARGS].
s_univ(P,[F|ARGS]):- atom(F),is_list(ARGS),P=..[F|ARGS].
s_univ(P,S):-P=S.

l_arity(F,A):- clause_b(arity(F,A)).
l_arity(function,1).
l_arity(quote,1).
l_arity('$BQ',1).
l_arity(F,A):-current_predicate(F/A).
l_arity(_,1).

%% sexpr_sterm_to_pterm_list( ?VAR, ?VAR) is semidet.
%
% S-expression Converted To Pterm List.
%

sexpr_sterm_to_pterm_list(TERM,PTERMO):- is_list(TERM),append(BEFORE,[VAR],TERM),atom(VAR),atom_concat('@',RVAR,VAR),clip_qm(RVAR,V),!,append(BEFORE,'$VAR'(V),PTERM),
   sexpr_sterm_to_pterm_list0(PTERM,PTERMO).
sexpr_sterm_to_pterm_list(TERM,PTERM):- sexpr_sterm_to_pterm_list0(TERM,PTERM).

sexpr_sterm_to_pterm_list0(VAR,VAR):-is_ftVar(VAR),!.
sexpr_sterm_to_pterm_list0([],[]):-!.
sexpr_sterm_to_pterm_list0([S|STERM],[P|PTERM]):-sexpr_sterm_to_pterm(S,P),sexpr_sterm_to_pterm_list0(STERM,PTERM),!.
sexpr_sterm_to_pterm_list0(VAR,VAR).


/*===================================================================
% input_to_forms/3 does less consistancy checking then conv_to_sterm

Always a S-Expression: 'WFFOut' placing variables in 'VARSOut'

|?-input_to_forms(`(isa a b)`,Clause,Vars).
Clause = [isa,a,b]
Vars = _h70

| ?- input_to_forms(`(isa a (b))`,Clause,Vars).
Clause = [isa,a,[b]]
Vars = _h70

|?-input_to_forms(`(list a b )`,Clause,Vars)
Clause = [list,a,b]
Vars = _h70

?- input_to_forms_debug("(=> (isa ?NUMBER ImaginaryNumber) (exists (?REAL) (and (isa ?REAL RealNumber) (equal ?NUMBER (MultiplicationFn ?REAL (SquareRootFn -1))))))").

?- input_to_forms_debug("(=> (isa ?PROCESS DualObjectProcess) (exists (?OBJ1 ?OBJ2) (and (patient ?PROCESS ?OBJ1) (patient ?PROCESS ?OBJ2) (not (equal ?OBJ1 ?OBJ2)))))").


| ?- input_to_forms(`(genlMt A ?B)`,Clause,Vars).
Clause = [genlMt,'A',_h998]
Vars = [=('B',_h998)|_h1101]

| ?- input_to_forms(`
 (goals Iran  (not   (exists   (?CITIZEN)   
  (and    (citizens Iran ?CITIZEN)    (relationExistsInstance maleficiary ViolentAction ?CITIZEN)))))`).

Clause = [goals,Iran,[not,[exists,[_h2866],[and,[citizens,Iran,_h2866],[relationExistsInstance,maleficiary,ViolentAction,_h2866]]]]]
Vars = [=(CITIZEN,_h2866)|_h3347]

| ?- input_to_forms_debug(`
(queryTemplate-Reln QuestionTemplate definitionalDisplaySentence 
       (NLPatternList 
           (NLPattern-Exact "can you") 
           (RequireOne 
               (NLPattern-Word Acquaint-TheWord Verb) 
               (NLPattern-Word Tell-TheWord Verb)) 
           (RequireOne 
               (NLPattern-Exact "me with") 
               (NLPattern-Exact "me what")) 
           (OptionalOne 
               (WordSequence "the term") "a" "an") 
           (NLPattern-Template NPTemplate :THING) 
           (OptionalOne "is" ) 
           (OptionalOne TemplateQuestionMarkMarker)) 
       (definitionalDisplaySentence :THING ?SENTENCE)) `).

| ?- input_to_forms_debug(`
 (#$STemplate #$bioForProposal-short 
  (#$NLPatternList (#$NLPattern-Template #$NPTemplate :ARG1) 
   (#$NLPattern-Exact "short bio for use in proposals" ) (#$NLPattern-Word #$Be-TheWord #$Verb) 
      (#$NLPattern-Exact "") (#$NLPattern-Template #$NPTemplate :ARG2)) (#$bioForProposal-short :ARG1 :ARG2))`).
 
input_to_forms_debug("(=> (disjointDecomposition ?CLASS @ROW) (forall (?ITEM1 ?ITEM2) (=> (and (inList ?ITEM1 (ListFn @ROW)) (inList ?ITEM2 (ListFn @ROW)) (not (equal ?ITEM1 ?ITEM2))) (disjoint ?ITEM1 ?ITEM2))))").


input_to_forms_debug(`
 (#$STemplate #$bioForProposal-short 
  (#$NLPatternList (#$NLPattern-Template #$NPTemplate :ARG1) 
   (#$NLPattern-Exact "short bio for use in proposals" ) (#$NLPattern-Word #$Be-TheWord #$Verb) 
      (#$NLPattern-Exact "") (#$NLPattern-Template #$NPTemplate :ARG2)) (#$bioForProposal-short :ARG1 :ARG2)) `).

txt_to_codes("(documentation Predicate EnglishLanguage \"A &%Predicate is a sentence-forming &%Relation. Each tuple in the &%Relation is a finite, ordered sequence of objects. The fact that a particular tuple is an element of a &%Predicate is denoted by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the objects so related. In the case of &%BinaryPredicates, the fact can be read as `arg_1 is *predicate* arg_2' or `a *predicate* of arg_1 is arg_2'.\")",X).
input_to_forms_debug("(documentation Predicate EnglishLanguage \"A &%Predicate is a sentence-forming &%Relation. Each tuple in the &%Relation is a finite, ordered sequence of objects. The fact that a particular tuple is an element of a &%Predicate is denoted by '(*predicate* arg_1 arg_2 .. arg_n)', where the arg_i are the objects so related. In the case of &%BinaryPredicates, the fact can be read as `arg_1 is *predicate* arg_2' or `a *predicate* of arg_1 is arg_2'.\")",X,Y).

// ==================================================================== */
:- export(current_input_to_forms/2).

%= 	 	 

%% input_to_forms( ?FormsOut, ?Vars) is semidet.
%
% Input Converted To Forms.
%
current_input_to_forms(FormsOut,Vars):- 
    current_input(In),
    input_to_forms(In, FormsOut,Vars).


input_to_forms_debug(String):-kif_assertion_recipe(String,Wff),wdmsg(Wff),!.
input_to_forms_debug(String):-input_to_forms(String,Wff,Vs),
  b_setval('$variable_names',Vs),wdmsg(Wff),!.

:- export(input_to_forms/3).

	 	 

:- export(input_to_forms/3).

%% input_to_forms( ?In, ?FormsOut, ?Vars) is semidet.
%
% Get Input Converted To Forms.
%
input_to_forms(Codes,FormsOut,Vars):- 
    is_openable(Codes)->
    parse_sexpr(Codes, Forms0)->
    to_untyped(Forms0, Forms1)->
    extract_lvars(Forms1,FormsOut,Vars),!.
input_to_forms(Forms,FormsOut,Vars):-
    to_untyped(Forms, Forms1) ->
    extract_lvars(Forms1,FormsOut,Vars)-> true.


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    Lisprolog -- Interpreter for a simple Lisp. Written in Prolog.
    Written Nov. 26th, 2006 by Markus Triska (triska@gmx.at).
    Public domain code.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Parsing
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


tstl:- tstl('./games/ontologyportal_sumo/Merge.kif'),
         tstl('./games/ontologyportal_sumo/Translations/relations-en.txt'),
         tstl('./games/ontologyportal_sumo/english_format.kif'),
         tstl('./games/ontologyportal_sumo/domainEnglishFormat.kif'),
         tstl('./games/ontologyportal_sumo/Mid-level-ontology.kif'),
         !.

writeqnl(O):-writeq(O),nl.

tstl(I):- with_lisp_translation(I,writeqnl).

phrase_from_stream_part(Grammar, In) :- at_end_of_stream(In),!,(Grammar=end_of_file;term_variables(In,[end_of_file|_]);throw(at_end_of_stream(In))).
phrase_from_stream_part(Grammar, In) :-   
     seek(In, 0, current, Prev),
     stream_to_lazy_list(In, List),
     phrase(Grammar, List, More),
     length(List,Used),
     length(More,UnUsed),
     Offset is Used - UnUsed + Prev,
     % dmsg((Offset is Used - UnUsed + Prev)),
     seek(In,Offset,bof,NewPos),!.

%= 	 	 

%% parse_sexpr( :TermS, ?Expr) is semidet.
%
% Parse S-expression.
%

parse_sexpr_str(S,Expr):- nb_setval('$maybe_string',t),parse_sexpr(string(S), Expr),nb_setval('$maybe_string',[]).


parse_sexpr(S, Expr) :- is_stream(S),!,catch(with_stream_pos(S,phrase_from_stream_part(file_sexpr(Expr),S)),at_end_of_stream(S),Expr=end_of_file).
parse_sexpr(string(String), Expr) :- txt_to_codes(String,Codes),parse_sexpr_ascii(Codes, Expr).
parse_sexpr(atom(String), Expr) :- txt_to_codes(String,Codes),parse_sexpr_ascii(Codes, Expr).
parse_sexpr(text(String), Expr) :- txt_to_codes(String,Codes),parse_sexpr_ascii(Codes, Expr).
parse_sexpr([E|List], Expr) :- parse_sexpr_ascii([E|List], Expr).
parse_sexpr(Other, Expr) :- l_open_input(Other,In),!,parse_sexpr(In, Expr).

:- export(txt_to_codes/2).
txt_to_codes(AttVar,AttVar):-attvar(AttVar),!.
txt_to_codes(S,Codes):- is_stream(S),!,stream_to_lazy_list(S,Codes),!.
txt_to_codes([C|Text],[C|Text]):- number(C),!.
txt_to_codes([C|Text],_):- atom(C),atom_length(C,1),!,throw(txt_to_codes([C|Text])).
txt_to_codes(Text,Codes):- catch((text_to_string(Text,String),string_codes(String,Codes)),_,fail).

%% parse_sexpr_ascii( ?Codes, ?Expr) is semidet.
%
% Parse S-expression Codes.
%
parse_sexpr_ascii(S, Expr) :- is_stream(S),!,phrase_from_stream_part(file_sexpr(Expr),S).
parse_sexpr_ascii(Text, Expr) :- must(txt_to_codes(Text,Codes)),phrase(file_sexpr(Expr), Codes),!.

:- export(file_sexpr//1).
:- export(sexpr//1).

% Use DCG for parser.

file_sexpr('$COMMENT'(Expr)) --> line_comment(Expr),!.
file_sexpr('$COMMENT'([])) --> sblank_lines,!.
file_sexpr(end_of_file) --> [end_of_file],!.
file_sexpr(end_of_file) --> [-1],!.
file_sexpr(Expr) --> sexpr(Expr),!.


%%  sexpr(L)// is semidet.
%
sexpr(L)                      --> sblank,!,sexpr(L),!.
sexpr(L)                      --> `(`, !, swhite, sexpr_list(L),!, swhite.
sexpr('$OBJ'(vector,V))                 --> `#(`, !, sexpr_vector(V,`)`),!, swhite.
sexpr('$OBJ'(vugly,V))                 --> `#<`, sexpr_vector(V,`>`),!, swhite.
sexpr('$OBJ'(ugly,V))                 --> `#<`, read_string_until(V,`>`),!, swhite.
sexpr('#'(t))                 --> `#t`, !, swhite.
sexpr('#'(f))                 --> `#f`, !, swhite.
sexpr((A))              --> `|`, !, read_string_until(S,`|`), swhite,{atom_string(A,S)}.
sexpr('#'(E))              --> `#$`, !, rsymbol('#$',E), swhite.
sexpr('#'(E))              --> `&%`, !, rsymbol('#$',E), swhite.
sexpr('#'(E))              --> `?`, !, rsymbol('?',E), swhite.
sexpr('#\\'(C))                 --> `#\\`,rsymbol('',C), swhite.
sexpr((""))             --> `""`,!, swhite.
sexpr((Txt))                 --> `#|`, !, read_string_until(S,`|#`), swhite,{text_to_string(S,Txt)}.
sexpr((Txt))                 --> `"`, !, sexpr_string(S), swhite,{text_to_string(S,Txt)}.
sexpr(['#'(quote),E])              --> `'`, !, swhite, sexpr(E).
sexpr(['#'(backquote),E])         --> [96] , !, swhite, sexpr(E).
sexpr(['#'(function),E])                 --> `#\'`, sexpr(E), !, swhite.
sexpr(['$BQ-COMMA-ELIPSE',E]) --> `,@`, !, swhite, sexpr(E).
sexpr('$COMMA'(E))            --> `,`, !, swhite, sexpr(E).
sexpr(E)                      --> sym_or_num(E), swhite.

sym_or_num('$COMPLEX'(L)) --> `#C(`,!, swhite, sexpr_list(L), swhite.
sym_or_num((E)) --> snumber(S),{number_string(E,S)}.
sym_or_num((E)) --> rsymbol_maybe('',E),!.
sym_or_num('#'(E)) --> [C],{name(E,[C])}.

sblank --> [C], {C =< 32},!, swhite.
sblank --> line_comment(S),{nop(dmsg(line_comment(S)))},!,swhite.

sblank_lines --> [C], {eoln(C)},!.
sblank_lines --> [C], {C =< 32},!, sblank_lines.

swhite --> sblank,!.
swhite --> [].


%= 	 	 

%% eoln( :PRED10VALUE1) is semidet.
%
% Eoln.
%
eoln(13).
eoln(10).


line_comment([T])--> `;`,!,l_line_comment(S),{text_to_string(S,T)},!.


l_line_comment([]) --> [C], {eoln(C)}, !.
l_line_comment([C|L]) --> [C], l_line_comment(L).


sexprs([H|T]) --> sexpr(H), !, sexprs(T).
sexprs([]) --> [].


:- export(sexpr_list//1).

sexpr_list([]) --> `)`, !.
sexpr_list(_) --> `.`, [C], {\+ sym_char(C)}, !, {fail}.
sexpr_list([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_rest([]) --> `)`, !.
sexpr_rest(E) --> `.`, [C], {\+ sym_char(C)}, !, sexpr(E,C), !, `)`.
sexpr_rest([Car|Cdr]) --> sexpr(Car), !, sexpr_rest(Cdr).

sexpr_vector([],End) --> End, !.
sexpr_vector([First|Rest],End) --> sexpr(First), !, sexpr_vector(Rest,End).

sexpr_string([C|S]) --> `\\`, lchar(C),!, sexpr_string(S).
sexpr_string([]) --> `"`, !.
sexpr_string([32|S]) --> [C],{eoln(C)}, sexpr_string(S).
sexpr_string([35, 36|S]) --> `&%`, !, sexpr_string(S).
sexpr_string([C|S]) --> [C], sexpr_string(S).

read_string_until([H|S],[H|B]) --> `\\`,[H],!, read_string_until(S,[H|B]).
read_string_until([],HB) --> HB, !.
read_string_until([C|S],HB) --> [C],read_string_until(S,HB).

lchar(92) --> `\\`, !.
lchar(34) --> `"`, !.
lchar(N)  --> [C], {C >= 32, N is C}.

rsymbol(Prepend,E) --> [C], {sym_char(C)}, sym_string(S), {string_to_atom([C|S],E0),atom_concat(Prepend,E0,E)}.

rsymbol_maybe(Prepend,ES) --> rsymbol(Prepend,E),{maybe_string(E,ES)}.

maybe_string(E,ES):- nb_current('$maybe_string',t),!,text_to_string(E,ES).
maybe_string(E,E).

sym_string([H|T]) --> [H], {sym_char(H)}, sym_string(T).
sym_string([]) --> [].

string_vector([First|Rest]) --> sexpr(First), !, string_vector(Rest).
string_vector([]) --> [], !.

snumber([45|N]) --> `-`, unsigned_number(N).
snumber([43|N]) --> `+`, unsigned_number(N).
snumber(N) -->  unsigned_number(N).

unsigned_number([X|N]) -->  cdigit(X),unsigned_number0(N).

unsigned_number0([69|N]) --> `E`,snumber(N).
unsigned_number0([101|N]) --> `e`,snumber(N).
unsigned_number0([X|N]) --> cdigit(X), unsigned_number0(N).
unsigned_number0([]) --> [].

cdigit(C) --> [C], {C >= 48, C =<57}.
cdigit(46) --> `.`.

% . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .


%= 	 	 

%% sexpr( ?E, ?C, ?X, ?Z) is semidet.
%
% S-Expression.
%
sexpr(E,C,X,Z) :- swhite([C|X],Y), sexpr(E,Y,Z).

% dquote semicolon parens  hash qquote  comma backquote

%= 	 	 

%% sym_char( ?C) is semidet.
%
% Sym Char.  (not ";()#',`
%  )
%
sym_char(C) :- nb_current('$maybe_string',t),!, C > 32, not(member(C,[34,59,40,41,35,39,44,96|`.:;!%`])).  
sym_char(C) :- C > 32, not(member(C,[34,59,40,41,35,39,44,96])).  

:- nb_setval('$maybe_string',[]).

:- thread_local(t_l:s2p/1).



%= 	 	 

%% to_unbackquote( ?I, ?O) is semidet.
%
% Converted To Unbackquote.
%
to_unbackquote(I,O):-to_untyped(I,O).

:- export(to_untyped/2).

%= 	 	 

%% to_untyped( :TermVar, :TermName) is semidet.
%
% Converted To Untyped.
%
to_untyped(S,S):- var(S),!.
to_untyped([],[]):-!.
to_untyped(VAR,NameU):-atomic(VAR),atom_concat('#$',NameU,VAR),!.
%to_untyped(S,s(L)):- string(S),atom_contains(S,' '),atomic_list_concat(['(',S,')'],O),parse_sexpr_str(O,L),!.
to_untyped(S,S):- string(S),!.
to_untyped(S,S):- number(S),!.
%to_untyped(S,O):- atom(S),catch(atom_number(S,O),_,fail),!.
to_untyped(Var,'$VAR'(Name)):-svar(Var,Name),!.
to_untyped(Atom,Atom):- \+ compound(Atom),!.
to_untyped('$BQ'(VarName),'$BQ'(VarName)):-!.
to_untyped('#'('#'(S)),O):-nonvar(S),!,to_untyped('#'(S),O).
to_untyped('#'(S),O):-nonvar(S),!,to_untyped(S,O),!.
to_untyped('#\\'(S),'#\\'(S)):-!.
to_untyped('$COMMA'(S),'$COMMA'(O)):-to_untyped(S,O),!.
to_untyped('$OBJ'(S),'$OBJ'(O)):-to_untyped(S,O),!.
to_untyped('$OBJ'(Ungly,S),O):-to_untyped(S,SO),!,O=..[Ungly,SO].
to_untyped('$NUMBER'(S),O):-to_untyped(S,O),!.
% to_untyped([[]],[]):-!.
to_untyped('$STR'(Expr),Forms):- (text_to_string_safe(Expr,Forms);to_untyped(Expr,Forms)),!.
to_untyped(['#'(backquote),Rest],Out):- !,to_untyped(['#'('$BQ'),Rest],Out).
to_untyped(['#'(S)|Rest],Out):-is_list(Rest),maplist(to_untyped,[S|Rest],[F|Mid]), 
          ((atom(F),t_l:s2p(F))-> Out=..[F|Mid];Out=[F|Mid]),
          to_untyped(Out,OOut).
to_untyped([H|T],Forms):-is_list([H|T]),must(text_to_string_safe([H|T],Forms);maplist(to_untyped,[H|T],Forms)).
to_untyped([H|T],[HH|TT]):-!,must_det_l((to_untyped(H,HH),to_untyped(T,TT))).
to_untyped(ExprI,ExprO):- must(ExprI=..Expr),
  must_maplist(to_untyped,Expr,[HH|TT]),(atom(HH)-> ExprO=..[HH|TT] ; ExprO=[HH|TT]).
% to_untyped(Expr,Forms):-compile_all(Expr,Forms),!.



%= 	 	 

%% remove_incompletes( :TermN, :TermCBefore) is semidet.
%
% Remove Incompletes.
%
remove_incompletes([],[]).
remove_incompletes([N=_|Before],CBefore):-var(N),!,
 remove_incompletes(Before,CBefore).
remove_incompletes([NV|Before],[NV|CBefore]):-
 remove_incompletes(Before,CBefore).

:- export(extract_lvars/3).

%= 	 	 

%% extract_lvars( ?A, ?B, ?After) is semidet.
%
% Extract Lvars.
%
extract_lvars(A,B,After):-
     (get_varname_list(Before)->true;Before=[]),
     remove_incompletes(Before,CBefore),!,
     copy_lvars(A,CBefore,B,After).

% copy_lvars( VAR,Vars,VAR,Vars):- var(VAR),!.

%= 	 	 

%% copy_lvars( :TermVAR, ?Vars, :TermNV, ?NVars) is semidet.
%
% Copy Lvars.
%
copy_lvars( VAR,Vars,NV,NVars):- svar(VAR,Name),must(atom(Name)),!,must(register_var(Name=NV,Vars,NVars)).
copy_lvars([],Vars,[],Vars).
copy_lvars(Term,Vars,Term,Vars):- \+compound(Term),!.
copy_lvars('?'(Inner),Vars,Out,NVars):- !,
    copy_lvars((Inner),Vars,(NInner),NVars),
    (atom(NInner) -> atom_concat('?',NInner,Out) ; Out = '?'(NInner)),!.

copy_lvars([H|T],Vars,[NH|NT],NVars):- !, copy_lvars(H,Vars,NH,SVars), copy_lvars(T,SVars,NT,NVars).
copy_lvars(Term,Vars,NTerm,NVars):-    
    Term=..[F|Args],    % decompose term
    (svar(F,_)-> copy_lvars( [F|Args],Vars,NTerm,NVars);
    % construct copy term
    (copy_lvars(Args,Vars,NArgs,NVars), NTerm=..[F|NArgs])).  


%= 	 	 

%% svar( ?Var, ?NameU) is semidet.
%
% Svar.
%
svar(Var,NameU):-var(Var),!,format(atom(Name),'~w',[(Var)]),!,atom_concat('_',NameU,Name).
svar('$VAR'(Var),Name):-number(Var),format(atom(Name),'~w',['$VAR'(Var)]),!.
svar('$VAR'(VarName),VarNameU):-svar_fixvarname(VarName,VarNameU),!.
svar('$VAR'(Name),Name):-!.
svar('?'(Name),NameU):-svar_fixvarname(Name,NameU),!.
svar(VAR,NameU):-atom(VAR),atom_concat('??',Name,VAR),ok_varname(Name),!,svar_fixvarname(Name,NameI),atom_concat('_',NameI,NameU).
svar(VAR,NameU):-atom(VAR),atom_concat('?',Name,VAR),ok_varname(Name),svar_fixvarname(Name,NameU).
svar(Var,Var):-var(Var),!.


:- export(svar_fixvarname/2).

%= 	 	 

%% svar_fixvarname( ?SVARIN, ?UP) is semidet.
%
% Svar Fixvarname.
%
svar_fixvarname(SVARIN,UP):- compound(SVARIN),!, SVARIN = '?'(SVAR),!,atom(SVAR), svar_fixvarname(SVAR,UP).
svar_fixvarname(SVAR,UP):- \+ atom(SVAR),dtrace,UP=SVAR.
svar_fixvarname(SVAR,UP):- atom(SVAR)->(ok_varname(SVAR),fix_varcase(SVAR,UP),must(ok_varname(UP)));UP=SVAR.


%= 	 	 

%% fix_varcase( ?I, ?O) is semidet.
%
% Fix Varcase.
%
fix_varcase(I,O):-fix_varcase0(I,M),atom_subst(M,'-','_',O).

%= 	 	 

%% fix_varcase0( ?Word, ?WordC) is semidet.
%
% Fix Varcase Primary Helper.
%
fix_varcase0(Word,WordC):-!,name(Word,[F|R]),to_upper(F,U),name(WordC,[U|R]).
% the cut above stops the rest 
fix_varcase0(Word,Word):-upcase_atom(Word,UC),UC=Word,!.
fix_varcase0(Word,WordC):-downcase_atom(Word,UC),UC=Word,!,name(Word,[F|R]),to_upper(F,U),name(WordC,[U|R]).
fix_varcase0(Word,Word). % mixed case

:- export(ok_varname/1).

%= 	 	 

%% ok_varname( ?Name) is semidet.
%
% Ok Varname.
%
ok_varname(Name):- number(Name).
ok_varname(Name):- atom(Name),atom_codes(Name,[C|_List]),char_type(C,csym).

%:- export(ok_codes_in_varname/1).
%ok_codes_in_varname([]).
%ok_codes_in_varname([C|List]):-!,ok_in_varname(C),ok_codes_in_varname(List).

%:- export(ok_in_varname/1).
%ok_in_varname(C):-sym_char(C),\+member(C,`!@#$%^&*?()`).



%= 	 	 

%% atom_upper( ?A, ?U) is semidet.
%
% Atom Upper.
%
atom_upper(A,U):-string_upper(A,S),atom_string(U,S).


%= 	 	 

%% lisp_read_from_input( ?Forms) is semidet.
%
% Lisp Read Converted From Input.
%
lisp_read_from_input(Forms):-lisp_read_from_stream(current_input,Forms),!.

%= 	 	 

%% lisp_read_from_stream( ?I, ?Forms) is semidet.
%
% Lisp Read Converted From Input.
%
lisp_read_from_stream(In,Forms):- is_stream(In), at_end_of_stream(In),!,end_of_file=Forms.
lisp_read_from_stream(In,Forms):- stream_source_typed(In,Type),stream_position(In,Pos,Pos),
 wdmsg(pos),must(to_untyped(Type,Forms)).


%= 	 	 

%% stream_source_typed( ?I, ?Expr) is semidet.
%
% Stream Source Typed.
%
stream_source_typed(In,Expr):- !,parse_sexpr(In,Expr).
stream_source_typed(In,Expr):-
 (read_line_to_codes(current_input,AsciiCodes),
      (AsciiCodes==[]-> (at_end_of_stream(In) -> (Expr=end_of_file); stream_source_typed(In,Expr)); 
        once(must(parse_sexpr(AsciiCodes,Expr);stream_source_typed(In,Expr));read_term_from_codes(AsciiCodes,Expr,[])))).



%= 	 	 

%% lowcase( :TermC1, :TermC2) is semidet.
%
% Lowcase.
%
lowcase([],[]).
lowcase([C1|T1],[C2|T2]) :- lowercase(C1,C2), lowcase(T1,T2).


%= 	 	 

%% lowercase( ?C1, ?C2) is semidet.
%
% Lowercase.
%
lowercase(C1,C2) :- C1 >= 65, C1 =< 90, !, C2 is C1+32.
lowercase(C,C).


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Interpretation
   --------------

   Declaratively, execution of a Lisp form is a relation between the
   (function and variable) binding environment before its execution
   and the environment after its execution. A Lisp program is a
   sequence of Lisp forms, and its result is the sequence of their
   results. The environment is represented as a pair of association
   lists Fs-Vs, associating function names with argument names and
   bodies, and variables with values. DCGs are used to implicitly
   thread the environment state through.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */



%= 	 	 

%% codelist_to_forms( ?AsciiCodesList, ?FormsOut) is semidet.
%
% Codelist Converted To Forms.
%
codelist_to_forms(AsciiCodesList,FormsOut):-
    parse_sexpr(AsciiCodesList, Forms0),    
    compile_all(Forms0, FormsOut),!.


%= 	 	 

%% run( ?Program, ?Values) is semidet.
%
% Run.
%
run(Program, Values) :-
    parse_sexpr(Program, Forms0),    
    empty_assoc(E),
    compile_all(Forms0, Forms),
    writeq(seeingFormas(Forms)),nl,
    phrase(eval_all(Forms, Values0), [E-E], _),
    maplist(unfunc, Values0, Values).


%= 	 	 

%% unfunc( :TermS, :TermS) is semidet.
%
% Unfunc.
%
unfunc('#'(S), S).
unfunc(t, t).
unfunc('$NUMBER'(N), N).
unfunc([], []).
unfunc([Q0|Qs0], [Q|Qs]) :- unfunc(Q0, Q), unfunc(Qs0, Qs).


%= 	 	 

%% fold( :TermARG1, ?VALUE2, ?V, ?V) is semidet.
%
% Fold.
%
fold([], _, V, '$NUMBER'(V)).
fold(['$NUMBER'(F)|Fs], Op, V0, V) :- E =.. [Op,V0,F], V1 is E, fold(Fs, Op, V1, V).


%= 	 	 

%% compile_all( ?Fs0, ?Fs) is semidet.
%
% Compile All.
%
compile_all(Fs0, Fs) :- maplist(compile, Fs0, Fs).

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    compile/2 marks (with 'user/1') calls of user-defined functions.
    This eliminates an otherwise defaulty representation of function
    calls and thus allows for first argument indexing in eval//3.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


%= 	 	 

%% compile( ?F0, ?F) is semidet.
%
% Compile.
%
compile(F0, F) :-
    (   F0 = '$NUMBER'(_)   -> F = F0
    ;   F0 = '#'(t)   -> F = t
    ;   F0 = '#'(nil) -> F = []
    ;   F0 = '#'(_)   -> F = F0
    ;   F0 = [] -> F = []
    ;   F0 = ['#'(quote),Arg] -> F = [quote,Arg]
    ;   F0 = ['#'(setq),'#'(Var),Val0] -> compile(Val0, Val), F = [setq,Var,Val]
    ;   F0 = ['#'(Op)|Args0],
        memberchk(Op, [+,-,*,equal,if,>,<,=,progn,eval,list,car,cons,
                       cdr,while,not]) ->
        compile_all(Args0, Args),
        F = [Op|Args]
    ;   F0 = ['#'(defun),'#'(Name),Args0|Body0] ->
        compile_all(Body0, Body),
        maplist(arg(1), Args0, Args),
        F = [defun,Name,Args|Body]
    ;   F0 = ['#'(Op)|Args0] -> compile_all(Args0, Args), F = ['$USER'(Op)|Args]
    ).

eval_all([], [])         --> [].
eval_all([A|As], [B|Bs]) --> eval(A, B), eval_all(As, Bs).

eval('$NUMBER'(N), '$NUMBER'(N))       --> [].
eval(t, t)             --> [].
eval([], [])           --> [].
eval('#'(A), V), [Fs-Vs] --> [Fs-Vs], { get_assoc(A, Vs, V) }.
eval([L|Ls], Value)    --> eval(L, Ls, Value).

eval(quote, [Q], Q) --> [].
eval(+, As0, V)     --> eval_all(As0, As), { fold(As, +, 0, V) }.
eval(-, As0, V)     --> eval_all(As0, ['$NUMBER'(V0)|Vs0]), { fold(Vs0, -, V0, V) }.
eval(*, As0, V)     --> eval_all(As0, Vs), { fold(Vs, *, 1, V) }.
eval(car, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [C|_] }.
eval(cdr, [A], C)   --> eval(A, V), { V == [] -> C = [] ; V = [_|C] }.
eval(list, Ls0, Ls) --> eval_all(Ls0, Ls).
eval(not, [A], V)   --> eval(A, V0), goal_truth(V0=[], V).
eval(>, [A,B], V)   --> eval(A, '$NUMBER'(V1)), eval(B, '$NUMBER'(V2)), goal_truth(V1>V2, V).
eval(<, [A,B], V)   --> eval(>, [B,A], V).
eval(=, [A,B], V)   --> eval(A, '$NUMBER'(V1)), eval(B, '$NUMBER'(V2)), goal_truth(V1=:=V2, V).
eval(progn, Ps, V)  --> eval_all(Ps, Vs), { last(Vs, V) }.
eval(eval, [A], V)  --> eval(A, F0), { compile(F0, F1) }, eval(F1, V).
eval(equal, [A,B], V) --> eval(A, V1), eval(B, V2), goal_truth(V1=V2, V).
eval(cons, [A,B], [V0|V1])  --> eval(A, V0), eval(B, V1).
eval(while, [Cond|Bs], [])  -->
    (   eval(Cond, []) -> []
    ;   eval_all(Bs, _),
        eval(while, [Cond|Bs], _)
    ).
eval(defun, [F,As|Body], '#'(F)), [Fs-Vs0] -->
    [Fs0-Vs0],
    { put_assoc(F, Fs0, As-Body, Fs) }.
eval('$USER'(F), As0, V), [Fs-Vs] -->
    eval_all(As0, As1),
    [Fs-Vs],
    { empty_assoc(E),
      get_assoc(F, Fs, As-Body),
      bind_arguments(As, As1, E, Bindings),
      phrase(eval_all(Body, Results), [Fs-Bindings], _),
      last(Results, V) }.
eval(setq, [Var,V0], V), [Fs0-Vs] -->
    eval(V0, V),
    [Fs0-Vs0],
    { put_assoc(Var, Vs0, V, Vs) }.
eval(if, [Cond,Then|Else], Value) -->
    (   eval(Cond, []) -> eval_all(Else, Values), { last(Values, Value) }
    ;   eval(Then, Value)
    ).

:- meta_predicate goal_truth(0,*,*,*).
goal_truth(Goal, T) --> { Goal -> T = t ; T = [] }.


%= 	 	 

%% bind_arguments( :TermARG1, :TermARG2, ?Bs, ?Bs) is semidet.
%
% Bind Arguments.
%
bind_arguments([], [], Bs, Bs).
bind_arguments([A|As], [V|Vs], Bs0, Bs) :-
    put_assoc(A, Bs0, V, Bs1),
    bind_arguments(As, Vs, Bs1, Bs).


%= 	 	 

%% run( ?S) is semidet.
%
% Run.
%
run(S):-'format'('~n~s~n',[S]),run(S,V),writeq(V).


:- meta_predicate(if_script_file_time(0)).


%= 	 	 

%% if_script_file_time( :GoalX) is semidet.
%
% If Script File Time.
%
if_script_file_time(X):-if_startup_script_local((X)).


%= 	 	 

%% if_startup_script_local( ?VALUE1) is semidet.
%
% If Startup Script Local.
%
if_startup_script_local(_).

% Append:
    :- if_script_file_time(run(`
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))`
)).

    %@ V = [append, [a, b, 3, 4, 5]].
    

% Fibonacci, naive version:
    :- if_script_file_time(run(`
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)`
)).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].
    

% Fibonacci, accumulating version:
    :- if_script_file_time(run(`
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)`
)).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].
    

% Fibonacci, iterative version:
    :- if_script_file_time(run(`
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)`
)).

    %@ % 34,233 inferences, 0.010 CPU in 0.010 seconds (98% CPU, 3423300 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].
    

% Higher-order programming and eval:
    :- if_startup_script_local(run(`
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        `
        )).

    %@ V = [map, plus1, [2, 3, 4]].

/*

:- export(baseKB:rff/0).

baseKB:rff:-baseKB:rff(wdmsg(n(first)),wdmsg(n(retry)),wdmsg(n(success)),wdmsg(n(failure))).

:- export(baseKB:rff/4).
baseKB:rff(OnFirst,OnRetry,OnSuccess,OnFailure) :- CU = was(never,first),
  call_cleanup((
    process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure),
    (nb_setarg(1,CU,first));((nb_setarg(1,CU,second)),!,fail)),
    (nb_setarg(2,CU,second),process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure),wdmsg(cleanup(CU)))),
  once((
    process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure),
    CU \= was(second, _))).

:- export(process_rff/5).
process_rff(CU,OnFirst,OnRetry,OnSuccess,OnFailure):-
   wdmsg(next(CU)),
   once(((CU==was(first,first)->OnFirst;true),
   (CU==was(second,first)->OnRetry;true),
   (CU==was(second,second)->OnFailure;true),
   (CU==was(first,second)-e>OnSuccess;true))).


*/

% % UNDO % :- add_import_module(baseKB,common_logic_sexpr,end).

read_pending_whitespace(In):- repeat, peek_char(In,Code),
   (( \+ char_type(Code,space), \+ char_type(Code,white))-> ! ; (get_char(In,_),fail)).



w_l_t(I,O):- parse_sexpr(I,M),to_untyped(M,MM),sexpr_sterm_to_pterm(MM,O),!.

with_lisp_translation(F,With):- 
   l_open_input(F,I),
   set_stream(I,encoding(octet)),
   repeat,
     w_l_t(I,O),
      (O==end_of_file->! ; (must_det(call(With,O)),fail)).

with_lisp_translation(Other,With):- 
  l_open_input(Other,In),
   setup_call_cleanup(true,(repeat,with_lisp_translation(In,With)),true).

% with_lisp_translation:-with_lisp_translation('dump.txt',=).
wlt2(O):- 
   open('dump.txt',read,I),see(I),repeat,current_input(I),baseKB:parse_sexpr(I,O),stream_property(I,position(POD)).

wlt_w(O):- 
   see('dump.txt'),repeat,current_input(I),baseKB:parse_sexpr(I,O),stream_property(I,position(POD)).

wlt3(O):-
  open('dump.txt',read,I),see(I),read_pending_whitespace(I),repeat,read_pending_whitespace(I),get_char(I,O),
    nop((stream_to_lazy_list(I,LL),parse_sexpr_ascii(LL, Expr))).

wlt4(Expr):-
  open('dump.txt',read,I),see(I),read_pending_whitespace(I),repeat,
   read_pending_whitespace(I),stream_to_lazy_list(I,LL),repeat,parse_sexpr_ascii(LL, Expr).

wlt5(Expr):-
  open('dump.txt',read,I),see(I),read_pending_whitespace(I),repeat,
   read_pending_whitespace(I),read_line_to_codes(I,LL),parse_sexpr_ascii(LL, Expr).

wlt6(Expr):-
   open('dump.txt',read,I),see(I),read_pending_whitespace(I),repeat,read_pending_whitespace(I),stream_to_lazy_list(I,LL),sexpr(Expr,LL,_).





