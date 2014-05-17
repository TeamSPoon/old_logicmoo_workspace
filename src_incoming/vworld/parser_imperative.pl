/* <module>
% Imperitive Sentence Parser (using DCG)
%
% Project Logicmoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- module(parser_imperative, [
                   parse_agent_text_command/5,            
                   parse_agent_text_command_1/5,            
                   parseIsa//2,
                   objects_match/3,
                   call_tabled/1,
                   findall_tabled/3,
                   parse_for/2,
                   parse_for/3,
                   parse_for/4,
                   object_match/2,
                   object_string/2,
                   order_descriptions/3,
                   string_equal_ci/2,
                   starts_with_icase/2,
                   sort_by_strlen/2,
                   remove_predupes/2,
                   ends_with_icase/2,
                   parseForTypes//2]).


:- include(logicmoo('vworld/moo_header')).

:- register_module_type(utility).

:- ensure_loaded(logicmoo('vworld/parser_e2c')).

% ===========================================================
% PARSE command
% ===========================================================
moo:decl_action(_Human_Player,parse(prolog,list(term)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack").

moo:agent_text_command(Agent,[parse,Type|List],Agent,parse(Type,List)).

moo:agent_call_command(_Gent,parse(Type,StringM)):-
   parse_for(Type,StringM,_Term,_LeftOver).

parse_for(Type,StringM):- parse_for(Type,StringM, _Term).

parse_for(Type,StringM, Term):-parse_for(Type,StringM, Term, []).

list_tail(_,[]).
list_tail(String,LeftOver):-ground(String),to_word_list(String,List),length(List,L),!,between(1,L,X),length(LeftOver,X).

parse_for(Type,StringM,Term,LeftOver):-parse_for(Type,StringM,Term,LeftOver,fmt).
parse_for(Type,StringM,Term,LeftOver,Print2):-
   to_word_list(StringM,String),
   list_tail(String,LeftOver),
   HOW = phrase(parseIsa(Type,Term),String,LeftOver),
   call(Print2,'parsing with ~q ~n.',[HOW]),
   (debugOnError(HOW)->
      call(Print2,'Success! parse \'~q\' "~q" = ~q   (leftover=~q) . ~n',[Type,String,Term,LeftOver]);
      call(Print2,'No Success.~n',[])).


meets_desc_spec(T,_L):- term_to_atom(T,S0),string_to_atom(S0,A),atomic_list_concat([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(_,[]):-!.
meets_desc_spec(S,[DS|SL]):-!,meets_desc_spec(S,DS),meets_desc_spec(S,SL),!.
meets_desc_spec(S,From-To):-!, desc_len(S,Len),!, between(From,To,Len).
meets_desc_spec(_,_).

desc_len(S0,Region):- term_to_atom(S0,S),
   atomic_list_concat(Words,' ',S),length(Words,Ws),atomic_list_concat(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.

order_descriptions(O,DescSpecs,ListO):-findall(S,(description(O,S),meets_desc_spec(S,DescSpecs)),Rev),reverse(Rev,List),delete_repeats(List,ListO).

delete_repeats([],[]):-!.
delete_repeats([Region|List],[Region|ListO]):-delete(List,Region,ListM), delete_repeats(ListM,ListO),!.

objects_match(SObj,Inv,List):-
   findall(Obj, (member(Obj,Inv),object_match(SObj,Obj)), List).

:-dynamic object_string_used/2.

call_listing(_):-!.
call_listing(Call):-forall(Call,fmt('~q.~n',[Call])).



object_string(O,String):-call_tabled((object_string(_,O,1-4,String))),!.

% fast maybe works slower in long run
object_string(Agent,O,DescSpecs,String):- fail,
 with_output_to(string(StringI),
   object_print_details(format,Agent,O,DescSpecs,[type,item,agent])),
   string_dedupe(StringI,String),!.

% object_string(Agent,O,DescSpecs,String):-object_string_list1(Agent,O,DescSpecs,String),!.
object_string(Agent,O,DescSpecs,String):-object_string_list2(Agent,O,DescSpecs,String).

object_string_list1(Agent,O,DescSpecs,String):- 
   OpenList = _,
   object_print_details(save_ol(OpenList),Agent,O,DescSpecs,[type,item,agent]),   
   OpenList = StringI,
   append(OpenList,[],StringI),
   trace,list_to_set(StringI,String).

save_ol_e(OS,E):-string(E),!,to_word_list(E,WL),save_ol_list(OS,WL),!.
save_ol_e(OS,mud_isa(A)):-!,save_ol_e(OS,A).
save_ol_e(OS,E):-is_list(E),!,save_ol_list(OS,E).
save_ol_e(OS,E):-append([E],_,AE),!,append(_,AE,OS),!.

save_ol(OS,' ~w ',[A]):-!,save_ol_e(OS,A),!.
save_ol(OS,'~w',[A]):-!,save_ol_e(OS,A),!.
save_ol(OS,Fmt,[A|KW]):-sformat(Str,Fmt,[A|KW]),to_word_list(Str,WL),save_ol_list(OS,WL),!.
save_ol_list(_,[]):-!.
save_ol_list(OS,[E|L]):-!,save_ol_e(OS,E),!,save_ol_list(OS,L),!.

% slow but works
object_string_list2(Agent,O,DescSpecs,String):- 
   gensym(object_string,OS),
   object_print_details(save_fmt(OS),Agent,O,DescSpecs,[type,item,agent]),
   call_listing(object_string_used(OS,_)),
   findall(Str,retract(object_string_used(OS,Str)),StringI),
   list_to_set(StringI,String).

save_fmt_e(OS,E):-string(E),!,to_word_list(E,WL),save_fmt_list(OS,WL),!.
save_fmt_e(OS,E):-is_list(E),!,save_fmt_list(OS,E).
save_fmt_e(OS,mud_isa(A)):-!,save_fmt_e(OS,A).
save_fmt_e(OS,E):-retractall(object_string_used(OS,E)),assert(object_string_used(OS,E)).

save_fmt(OS,' ~w ',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,Fmt,[A|KW]):-sformat(Str,Fmt,[A|KW]),to_word_list(Str,WL),save_fmt_list(OS,WL),!.
save_fmt_list(_,[]):-!.
save_fmt_list(OS,[E|L]):-!,save_fmt_e(OS,E),!,save_fmt_list(OS,L),!.


string_dedupe(StringI,StringO):- to_word_list(StringI,Words),remove_predupes(Words,StringO).

remove_predupes([],[]).
remove_predupes([L|ListI],ListO):- member(L,["",''," ",' ']),!,remove_predupes(ListI,ListO),!.
remove_predupes([L|ListI], ListO):- (member_ci(L,ListI) -> remove_predupes(ListI,ListO) ; (remove_predupes(ListI,ListM),[L|ListM]=ListO)),!.

member_ci(L,[List|I]):-!,member(LL2,[List|I]),string_equal_ci(LL2,L).
member_ci(L,L):-to_word_list(L,ListI),member(LL2,ListI),string_equal_ci(LL2,L).

string_ci(A,LIC):-hotrace((any_to_string(A,S),text_to_string(S,SS),string_lower(SS,SL),atomics_to_string(SLIC,"_",SL),atomics_to_string(SLIC," ",LIC))),!.

string_equal_ci(L0,L1):-once(string_ci(L0,SL0)),string_ci(L1,SL0),!.
string_equal_ci(L0,L0):-!.

object_print_details(Print,Agent,O,DescSpecs,Skipped):-
   once(member(O,Skipped);
  (
   call(Print,' ~w ',[O]),
   forall((holds_t(keyword,O,KW),meets_desc_spec(KW,DescSpecs)),call(Print,' ~w ',[KW])),
   forall((holds_t(nameString,O,KW)/*,meets_desc_spec(KW,DescSpecs)*/),call(Print,' ~w ',[KW])),
   (mud_isa(O,type);forall((mud_isa(O,S), meets_desc_spec(mud_isa(O,S),DescSpecs)),call(Print,' ~w ',[mud_isa(S)]))),
   ignore((order_descriptions(O,DescSpecs,List),forall_member(M,List,call(Print,' ~w ',[M])))),
   forall(mud_isa(O,S),object_print_details(Print,Agent,S,DescSpecs,[O|Skipped])) )).


object_match(SObj,Obj):- isaOrSame(Obj,SObj).
object_match(S,Obj):- 
   atoms_of(S,Atoms),
   current_agent_or_var(P),
   object_string(P,Obj,0-5,String),!,
   string_ci(String,LString),!,
   str_contains_all(Atoms,LString).

str_contains_all([],_String):-!.
str_contains_all(A,SL):-string_ci(SL,SLIC),SL\=SLIC,!,str_contains_all(A,SLIC).
str_contains_all([A|Atoms],String):-
      string_ci(A,L),
      sub_string(String,_,_,Aft,L),
      sub_string(String,Aft,_,0,SubString),!,
      str_contains_all(Atoms,SubString).

atoms_of(Var,[]):- (var(Var);Var==[]),!.
atoms_of('$VAR',[]):-!.
atoms_of(Atom,[]):-number(Atom),!.
atoms_of(Atom,[Atom]):-atomic(Atom),!.
atoms_of([H|T],L):-atoms_of(H,HL),atoms_of(T,TL),append(HL,TL,L),!.
atoms_of(C,L):-C=..CL,atoms_of(CL,L),!.

call_no_cuts(CALL):-clause(CALL,TEST),call_no_cuts_0(TEST).

call_no_cuts_0(true):-!.
call_no_cuts_0((!)):-!.
call_no_cuts_0((A,B)):-!.call_no_cuts_0(A),call_no_cuts_0(B).
call_no_cuts_0(C):-call(C).

% ===========================================================
% PARSER
% ===========================================================
parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
   parse_vp(Agent,do(NewAgent,GOAL),[SVERB|ARGS],[]).

parse_vp(Agent,do(NewAgent,GOAL),[SVERB|ARGS],[]):-
      parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!.

parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):-
  parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg(parserm(succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL))),!.

parse_agent_text_command_0(Agent,VERB,[PT2|ARGS],NewAgent,GOAL):-atomic(VERB),atomic(PT2),
   atomic_list_concat([VERB,PT2],'_',SVERB),
   parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg(parserm(special_succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL))),!.

parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- !,
 dmsg(parserm(failed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL))),fail,
 debug,visible(+all),leash(+all),trace,
 parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL),!.


parse_agent_text_command_1(Agent,VERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(moo:agent_text_command(Agent,[VERB|ARGS],NewAgent,GOAL)).

parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(moo:agent_text_command(Agent,[VERB|ARGS],NewAgent,GOAL)),
   verb_matches(SVERB,VERB).

% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS),
   dmsg(parserm("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg(parserm("chooseBestGoal"=GOAL)).

parse_agent_text_command_1(Agent,IVERB,ARGS,NewAgent,GOAL):- 
   verb_alias_to_verb(IVERB,SVERB),
   parse_agent_text_command_11(Agent,SVERB,ARGS,NewAgent,GOAL),!.

parse_agent_text_command_11(Agent,SVERB,ARGS,NewAgent,GOAL):-parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL).
parse_agent_text_command_11(Agent,SVERB,ARGS,NewAgent,GOAL):-to_word_list(SVERB,L),!,L=[A,B|C],append([B|C],ARGS,BCARGS),
   debugOnError(parse_agent_text_command_1(Agent,A,BCARGS,NewAgent,GOAL)).

moo:verb_alias('l','look').
moo:verb_alias('s','move s').
moo:verb_alias('where is','where').

pos_word_formula('infinitive',Verb,Formula):- dbase:'infinitive'(TheWord, Verb, _, _G183), dbase:'verbSemTrans'(TheWord, 0, 'TransitiveNPCompFrame', Formula, _, _).

verb_alias_to_verb(IVERB,SVERB):-moo:verb_alias(L,Look),verb_matches(L,IVERB),SVERB=Look,!.
verb_alias_to_verb(IVERB,SVERB):-specifiedItemType(IVERB,verb,SVERB), IVERB \= SVERB.

verb_matches(SVERB,VERB):-samef(VERB,SVERB).

parse_vp_templates(Agent,SVERB,_ARGS,TEMPLATES):-
   findall([VERB|TYPEARGS],
    ((     
     moo:decl_action(What,TEMPL,_),
     mud_isa(Agent,What),
     TEMPL=..[VERB|TYPEARGS],
     verb_matches(SVERB,VERB))),
     TEMPLATES_FA),
   % (TEMPLATES=[]->throw(noTemplates(Agent,SVERB,ARGS));true),
   sort(TEMPLATES_FA,TEMPLATES),!.
   
% parses a verb phrase and retuns multiple interps
parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS):-
   parse_vp_templates(Agent,SVERB,ARGS,TEMPLATES),
   dmsg(parserm("TEMPLATES"= ([SVERB,ARGS] = TEMPLATES))),
   TEMPLATES \= [],
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATES),      
      dmsg(parserm("parseForTypes"=phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver))),
      call_tabled((phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver))),
      GOAL=..[VERB|GOODARGS])),
      GOALANDLEFTOVERS_FA),
   sort(GOALANDLEFTOVERS_FA,GOALANDLEFTOVERS).

chooseBestGoal([_LeftOver - GOAL],GOAL):-!.
chooseBestGoal(GOALANDLEFTOVERS,GOAL):-
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),
   dmsg(parserm("Sorted"=Sorted)),
   member(_LeftOver - GOAL,Sorted),!.

% bestParse(?Order, @Term1, @Term2)
bestParse(Order,LeftOver1-GOAL1,LeftOver2-GOAL2):-
   length(LeftOver1,L1),length(LeftOver2,L2),
   functor(GOAL1,_,A1),functor(GOAL2,_,A2),
   must(once(bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2))).

:-style_check(-singleton).

bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,L1,L2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,-A1,-A2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,GOAL1,GOAL2).

:-style_check(+singleton).

moo:specifier_text(Dir,dir):-member_ci(Dir,[n,s,e,w,ne,nw,se,sw,u,d]).


moo:specifier_text(Text,Subclass):-moo:subclass(Subclass,spatialthing),mud_isa(X,Subclass),req(keyword(X,Text)).

phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver):-
   to_word_list(ARGS,ARGSL),!,
    phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver).

phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver):-
    debugOnError(phrase(parseForTypes(TYPEARGS,GOODARGS),ARGSL,LeftOver)).    


parseForTypes([], [], A, A).
parseForTypes([TYPE], [B], C, []) :-
        parseIsa(TYPE, B, C, []),!.
parseForTypes([TYPE|TYPES], [B|E], C, G) :-
        parseIsa(TYPE, B, C, F),
        parseForTypes(TYPES, E, F, G).

% parseIsa(T)-->parseIsa(T,_).
parseIsa(A, B, C) :-
        parseIsa(A, _, B, C).

% parseIsa(not(T),Term) --> dcgAnd(dcgNot(parseIsa(T)),theText(Term)).

parseIsa(_T, _, [AT|_], _):- var(AT),!,fail.
parseIsa(FT, B, C, D):- to_word_list(C,O),O\=C,!,parseIsa(FT, B, O, D).
parseIsa(FT, B, C, D):-  call_tabled(parseIsa0(FT, B, C, D)).

:-dynamic(call_tabled_list/2).

make_key(CC,Key):-copy_term(CC,Key),numbervars(Key,'$VAR',0,_),!.

expire_tabled_list(T):-CT= call_tabled_list(Key,List),doall(((CT,any_term_overlap(T,Key:List),retract(CT)))).

any_term_overlap(T1,T2):-atoms_of(T1,A1),atoms_of(T2,A2),!,member(A,A1),member(A,A2),!.

moo:decl_database_hook(assert(_),C):-expire_tabled_list(C).
moo:decl_database_hook(retract(_),C):-expire_tabled_list(C).

call_tabled(findall(A,B,C)):-!,findall_tabled(A,B,C).
call_tabled(C):-copy_term(C,CC),numbervars(CC,'$VAR',0,_),call_tabled(C,C).
call_tabled(CC,C):-make_key(CC,Key),call_tabled0(Key,C,C,List),!,member(C,List).
call_tabled0(Key,_,_,List):-call_tabled_list(Key,List),!.
call_tabled0(Key,E,C,List):-findall(E,C,List1),list_to_set(List1,List),asserta(call_tabled_list(Key,List)),!.

findall_tabled(Result,C,List):-make_key(Result^C,RKey),findall_tabled4(Result,C,RKey,List).
findall_tabled4(_,_,RKey,List):-call_tabled_list(RKey,List),!.
findall_tabled4(Result,C,RKey,List):-findall(Result,call_tabled(C),RList),list_to_set(RList,List),asserta(call_tabled_list(RKey,List)).

parseIsa0(FT, B, C, D):- list_tail(C,D),parseForIsa(FT, B, C, D).

parseForIsa(actor,A,B,C) :- parseForIsa(agent,A,B,C).
parseForIsa(optional(Type,_Who), Term, C, D) :- parseForIsa(Type, Term, C, D).
parseForIsa(optional(_Type,Who), Who, D, D).


parseForIsa(not(Type), Term, C, D) :-  dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).
parseForIsa(FT, B, C, D):-to_word_list(C,O),O\=C,!,parseForIsa(FT, B, O, D).
parseForIsa(FT, B, [AT|C], D) :- nonvar(AT),member_ci(AT,['at','the','a','an']),!,parseForIsa(FT, B, C, D).
parseForIsa(FT, B, C, D) :- trans_decl_sub(FT,Sub), parseFmtOrIsa(Sub, B, C, D),!.
parseForIsa(FT, B, C, D) :- trans_decl_sub(Sub,FT), parseFmtOrIsa(Sub, B, C, D),!.
parseForIsa(FT, B, C, D) :- parseFmtOrIsa(FT, B, C, D),!.

trans_decl_sub(Sub,Super):-trans_decl_subft(Sub,Super).
trans_decl_sub(Sub,Super):-trans_decl_sc(Sub,Super).

trans_decl_subft(FT,Sub):-moo:decl_subft(FT,Sub).
trans_decl_subft(FT,Sub):-moo:decl_subft(FT,A),moo:decl_subft(A,Sub).
trans_decl_subft(FT,Sub):-moo:decl_subft(FT,A),moo:decl_subft(A,B),moo:decl_subft(B,Sub).
trans_decl_sc(FT,Sub):-moo:subclass(FT,Sub).
trans_decl_sc(FT,Sub):-moo:subclass(FT,A),moo:subclass(A,Sub).
trans_decl_sc(FT,Sub):-moo:subclass(FT,A),moo:subclass(A,B),moo:subclass(B,Sub).


equals_icase(A,B):-string_ci(A,U),string_ci(B,U).
starts_with_icase(A,B):-string_ci(A,UA),string_ci(B,UB),atom_concat(UB,_,UA).
starts_or_ends_with_icase(A,B):-string_ci(A,UA),string_ci(B,UB),(atom_concat(UB,_,UA);atom_concat(_,UA,UB)).
ends_with_icase(A,B):-string_ci(A,UA),string_ci(B,UB),atom_concat(_,UB,UA).

parseFmtOrIsa(Sub, B, C, D):-parseFmt(Sub, B, C, D).

parseFmt(_, _, [AT|_], _):- var(AT),!,fail.
parseFmt(string,String)--> theString(String).
parseFmt(or([L|_]),Term) --> parseForIsa(L,Term).
parseFmt(or([_|List]),Term) --> parseForIsa(or(List),Term).

parseFmt(optional(Type,_),Term) --> parseForIsa(Type,Term).
parseFmt(optional(_,Term),Term) --> [].

parseFmt(and([L|List]),Term1) --> dcgAnd(parseForIsa(L,Term1),parseForIsa(and(List),Term2)),{ignore(Term1==Term2),!}.
parseFmt(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{specifiedItemType(String,Type,Term)}.

specifiedItemType([String],Type,StringO):-nonvar(String),!,specifiedItemType(String,Type,StringO).
specifiedItemType(String,Type,Inst) :- moo:specifier_text(Inst,Type), equals_icase(Inst,String),!.
specifiedItemType(String,Type,Inst):- instances_of_type(Inst,Type),object_match(String,Inst),!.
specifiedItemType(String,Type,Longest) :- findall(Inst, (moo:specifier_text(Inst,Type),starts_or_ends_with_icase(Inst,String)), Possibles), sort_by_strlen(Possibles,[Longest|_]),!.
specifiedItemType(A,T,AA):- is_decl_ft(T), format_complies(A,T,AA),!.

instances_of_type(Inst,Type):- setof(Inst-Type,mud_isa(Inst,Type),Set),member(Inst-Type,Set).
% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], logOnError(req(Term)).

sort_by_strlen(List,Sorted):-predsort(longest_string,List,Sorted).

% longest_string(?Order, @Term1, @Term2)
longest_string(Order,TStr1,TStr2):-
   text_to_string(TStr1,Str1),string_length(Str1,L1),
   text_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L2-Str2,L1-Str1).



:- include(logicmoo('vworld/moo_footer')).


