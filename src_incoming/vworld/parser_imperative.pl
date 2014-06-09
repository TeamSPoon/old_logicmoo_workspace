/* <module>
% Imperitive Sentence Parser (using DCG)
%
% Project LogicMoo: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/


:- module(parser_imperative, [
                   parse_agent_text_command/5,            
                   parse_agent_text_command_1/5,            
                   parse_vp_templates/4,
                   parseIsa//2,
                   parseIsa0//2,
                   objects_match/3,
                   dmsg_parserm/2,
                   dmsg_parserm/1,
                   parse_for/2,
                   parse_for/3,
                   parse_for/4,
                   print_parse_for/5,
                   object_match/2,
                   object_string/2,
                   order_descriptions/3,
                   parseForTypes//2]).


:- meta_predicate object_print_details(2,?,?,?,?).
:- meta_predicate print_parse_for(2,?,?,?,?).

:- include(logicmoo(vworld/moo_header)).

:- moodb:register_module_type(utility).

% ===========================================================
% PARSE command
% ===========================================================
moo:action_help(parse(prolog,list(term)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack").

moo:agent_text_command(Agent,[parse,Type|List],Agent,parse(Type,List)).

moo:agent_call_command(_Gent,parse(Type,StringM)):-
   print_parse_for(Type,StringM,_Term,_LeftOver,fmt).

parse_for(Type,StringM):- parse_for(Type,StringM, _Term).

parse_for(Type,StringM, Term):-parse_for(Type,StringM, Term, []).

list_tail(_,[]).
list_tail(String,LeftOver):-ground(String),to_word_list(String,List),length(List,L),!,between(1,L,X),length(LeftOver,X).

parse_for(Type,StringM,Term,LeftOver):- 
     print_parse_for(dmsg_parserm,Type,StringM,Term,LeftOver).

print_parse_for(Print2, Type,StringM,Term,LeftOver):- 
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



object_string(O,String):-object_string(_,O,1-4,String),!.

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
   list_to_set(StringI,String).

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




:-debug.

dmsg_parserm(_).
dmsg_parserm(_,_).
% ===========================================================
% PARSER
% ===========================================================
parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
   parse_vp(Agent,do(NewAgent,GOAL),[SVERB|ARGS],[]).

parse_vp(Agent,do(NewAgent,GOAL),[SVERB|ARGS],[]):-
      parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!.

parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):-
  parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg_parserm((succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL))),!.

parse_agent_text_command_0(Agent,VERB,[PT2|ARGS],NewAgent,GOAL):-atomic(VERB),atomic(PT2),
   atomic_list_concat([VERB,PT2],'_',SVERB),
   parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg_parserm((special_succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL))),!.

parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- !,
 dmsg_parserm((failed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL))),fail,
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
   dmsg_parserm(("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg_parserm(("chooseBestGoal"=GOAL)).

parse_agent_text_command_1(Agent,IVERB,ARGS,NewAgent,GOAL):- 
   verb_alias_to_verb(IVERB,SVERB),
   parse_agent_text_command_11(Agent,SVERB,ARGS,NewAgent,GOAL),!.

parse_agent_text_command_11(Agent,SVERB,ARGS,NewAgent,GOAL):-parse_agent_text_command_1(Agent,SVERB,ARGS,NewAgent,GOAL).
parse_agent_text_command_11(Agent,SVERB,ARGS,NewAgent,GOAL):-to_word_list(SVERB,L),!,L=[A,B|C],append([B|C],ARGS,BCARGS),
   debugOnError(parse_agent_text_command_1(Agent,A,BCARGS,NewAgent,GOAL)).

moo:verb_alias('l','look').
moo:verb_alias('s','move s').
moo:verb_alias('where is','where').

pos_word_formula('infinitive',Verb,Formula):- 'infinitive'(TheWord, Verb, _, _G183), 'verbSemTrans'(TheWord, 0, 'TransitiveNPCompFrame', Formula, _, _).

verb_alias_to_verb(IVERB,SVERB):-moo:verb_alias(L,Look),verb_matches(L,IVERB),SVERB=Look,!.
verb_alias_to_verb(IVERB,SVERB):-specifiedItemType(IVERB,verb,SVERB), IVERB \= SVERB.

verb_matches(SVERB,VERB):-samef(VERB,SVERB).

parse_vp_templates(Agent,SVERB,_ARGS,TEMPLATES):-
   findall([VERB|TYPEARGS],
    ((     
     moodb:type_action_help(What,TEMPL,_),
     mud_isa(Agent,What),
     TEMPL=..[VERB|TYPEARGS],
     verb_matches(SVERB,VERB))),
     TEMPLATES_FA),
   % (TEMPLATES=[]->throw(noTemplates(Agent,SVERB,ARGS));true),
   sort(TEMPLATES_FA,TEMPLATES),!.
   
% parses a verb phrase and retuns multiple interps
parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS):-
   parse_vp_templates(Agent,SVERB,ARGS,TEMPLATES),
   dmsg_parserm(("TEMPLATES"= ([SVERB,ARGS] = TEMPLATES))),
   TEMPLATES \= [],
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATES),      
      dmsg_parserm(("parseForTypes"=phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver))),
      phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver),
      GOAL=..[VERB|GOODARGS])),
      GOALANDLEFTOVERS_FA),
   sort(GOALANDLEFTOVERS_FA,GOALANDLEFTOVERS).

chooseBestGoal([_LeftOver - GOAL],GOAL):-!.
chooseBestGoal(GOALANDLEFTOVERS,GOAL):-
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),
   dmsg_parserm(("Sorted"=Sorted)),
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
parseIsa(FT, B, C, D):-  dbase:call_tabled(parseIsa0(FT, B, C, D)).

parseIsa0(FT, B, C, D):- list_tail(C,D),parseForIsa(FT, B, C, D).

parseForIsa(actor,A,B,C) :- parseForIsa(agent,A,B,C).
parseForIsa(optional(Type,_Who), Term, C, D) :- parseForIsa(Type, Term, C, D).
parseForIsa(optional(_Type,Who), Who, D, D).


parseForIsa(not(Type), Term, C, D) :-  dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).
parseForIsa(FT, B, C, D):-to_word_list(C,O),O\=C,!,parseForIsa(FT, B, O, D).
parseForIsa(FT, B, [AT|C], D) :- nonvar(AT),member_ci(AT,['at','the','a','an']),!,parseForIsa(FT, B, C, D).
parseForIsa(FT, B, C, D) :- query_trans_sub(FT,Sub), parseFmtOrIsa(Sub, B, C, D),!.
parseForIsa(FT, B, C, D) :- query_trans_sub(Sub,FT), parseFmtOrIsa(Sub, B, C, D),!.
parseForIsa(FT, B, C, D) :- parseFmtOrIsa(FT, B, C, D),!.

query_trans_sub(Sub,Super):-query_trans_subft(Sub,Super).
query_trans_sub(Sub,Super):-query_trans_sc(Sub,Super).

query_trans_subft(FT,Sub):-moo:subft(FT,Sub).
query_trans_subft(FT,Sub):-moo:subft(FT,A),moo:subft(A,Sub).
query_trans_subft(FT,Sub):-moo:subft(FT,A),moo:subft(A,B),moo:subft(B,Sub).
query_trans_sc(FT,Sub):-moo:subclass(FT,Sub).
query_trans_sc(FT,Sub):-moo:subclass(FT,A),moo:subclass(A,Sub).
query_trans_sc(FT,Sub):-moo:subclass(FT,A),moo:subclass(A,B),moo:subclass(B,Sub).


parseFmtOrIsa(Sub, B, C, D):-parseFmt(Sub, B, C, D).

parseFmt(_, _, [AT|_], _):- var(AT),!,fail.
parseFmt(string,String)--> theString(String).
parseFmt(or([L|_]),Term) --> parseForIsa(L,Term).
parseFmt(or([_|List]),Term) --> parseForIsa(or(List),Term).

parseFmt(optional(Type,_),Term) --> parseForIsa(Type,Term).
parseFmt(optional(_,Term),Term) --> [].

parseFmt(list(Type),[Term|List]) --> parseForIsa(Type,Term),parseForIsa(list(Type),List).
parseFmt(list(_Type),[]) --> [].

parseFmt(countBetween(_Type,_,High),[]) --> {High==0,!}, [].
parseFmt(countBetween(Type,Low,High),[Term|List]) --> parseForIsa(Type,Term),{!,Low2 is Low -1,High2 is High -1 },
   parseForIsa(countBetween(Type,Low2,High2),List).
parseFmt(countBetween(_Type,Low,_),[]) --> {!, Low < 1}, [].

parseFmt(and([L|List]),Term1) --> dcgAnd(parseForIsa(L,Term1),parseForIsa(and(List),Term2)),{ignore(Term1==Term2),!}.
parseFmt(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{specifiedItemType(String,Type,Term)}.

specifiedItemType([String],Type,StringO):-nonvar(String),!,specifiedItemType(String,Type,StringO).
specifiedItemType(String,Type,Inst) :- specifier_text(Inst,Type), equals_icase(Inst,String),!.
specifiedItemType(String,Type,Inst):- instances_of_type(Inst,Type),object_match(String,Inst),!.
specifiedItemType(String,Type,Longest) :- findall(Inst, (moo:specifier_text(Inst,Type),starts_or_ends_with_icase(Inst,String)), Possibles), sort_by_strlen(Possibles,[Longest|_]),!.
specifiedItemType(A,T,AA):- is_ft(T), correctFormatType(tell(_),A,T,AA),!.

instances_of_type(Inst,Type):- setof(Inst-Type,mud_isa(Inst,Type),Set),member(Inst-Type,Set).
% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], logOnError(req(Term)).


% :- include(logicmoo(vworld/moo_footer)).


