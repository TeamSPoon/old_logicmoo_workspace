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
                   parseIsa//2,
                   objects_match/3,
                   object_match/2,
                   object_string/2,
                   order_descriptions/3,
                   parseForTypes//2]).


:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).


% ===========================================================
% PARSE command
% ===========================================================
moo:decl_action(_Human_Player,parse(prolog,list(term)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack").

moo:agent_call_command(_Gent,parse(Type,StringM)):-
   type_parse(Type,StringM,_Term,_LeftOver).

type_parse(Type,StringM):- type_parse(Type,StringM, _Term).

type_parse(Type,StringM, Term):-type_parse(Type,StringM, Term, []).

type_parse(Type,StringM,Term,LeftOver):-
   to_word_list(StringM,String),  
   HOW = phrase(parseIsa(Type,Term),String,LeftOver),
   fmt('parsing with ~q ~n.',[HOW]),
   (debugOnError(HOW)->
      fmt('Success! parse \'~q\' "~q" = ~q   (leftover=~q) . ~n',[Type,String,Term,LeftOver]);
      fmt('No Success.~n',[])).


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

:-dynamic (object_string_used/2).

object_string(O,String):-object_string(_,O,1-4,String),!.

object_string(Agent,O,DescSpecs,String):- 
   gensym(object_string,OS),
   object_print_details(OS,Agent,O,DescSpecs,[]),
   with_output_to(string(StringI),doall((retract(object_string_used(OS,Str)),write(Str)))),
   retractall(object_string_used(OS,_)),
   string_dedupe(StringI,String).

remove_predupes([L|ListI],[L|ListO]):-not(member_ci(L,ListI)),remove_predupes(ListI,ListO).
remove_predupes([_|ListI],ListO):- remove_predupes(ListI,ListO).

member_ci(L,ListI):-member(L,ListI),!.
member_ci(L,ListI):-string_lower(L,LL),member(LLL,ListI),string_lower(LLL,LL),!.

string_dedupe(StringI,StringO):- atomics_to_string(StringL," ",StringI),remove_predupes(StringL,StringL0),atomics_to_string(StringL0," ",StringO).

object_print_details(OS,Agent,O,DescSpecs,Skipped):-
   once(member(O,Skipped);
   (forall((req(keyword(O,KW)),meets_desc_spec(KW,DescSpecs)),object_print_details_fmt(OS,' ~w',[KW])),
   forall((req(nameStrings(O,KW)),meets_desc_spec(KW,DescSpecs)),object_print_details_fmt(OS,' ~w',[KW])),
   ignore((mud_isa(O,S), meets_desc_spec(mud_isa(O,S),DescSpecs),object_print_details_fmt(OS,' ~w',[mud_isa(O,S)]))),
   order_descriptions(O,DescSpecs,List),
   forall_member(M,List,object_print_details_fmt(OS,' ~w.',[M])),
   forall(mud_isa(O,S),object_print_details(OS,Agent,S,DescSpecs,[O|Skipped])))).

object_print_details_fmt(OS,Fmt,[A|KW]):- sformat(Str,Fmt,[A|KW]), assert_if_new(object_string_used(OS,Str)).

object_match(SObj,Obj):- isaOrSame(Obj,SObj).
object_match(S,Obj):- 
   atoms_of(S,Atoms),
   current_agent_or_var(P),
   object_string(P,Obj,1-3,String),
   string_lower(String,LString),
   str_contains_all(Atoms,LString).

str_contains_all([],_String):-!.
str_contains_all([A|Atoms],String):-
      string_lower(A,L),
      sub_string(String,_,_,Aft,L),sub_string(String,Aft,_,0,SubString),!,
      str_contains_all(Atoms,SubString).

atoms_of(Var,[]):- (var(Var);Var==[]),!.
atoms_of(Atom,[Atom]):-atomic(Atom),!.
atoms_of([H|T],L):-atoms_of(H,HL),atoms_of(T,TL),append(HL,TL,L),!.
atoms_of(C,L):-C=..CL,atoms_of(CL,L),!.

% ===========================================================
% PARSER
% ===========================================================


parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):- moo:agent_text_command(Agent,[SVERB|ARGS],NewAgent,GOAL).

% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_verb_pharse(Agent,SVERB,ARGS,GOALANDLEFTOVERS),
   dmsg(parserm("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg(parserm("chooseBestGoal"=GOAL)).

verb_matches(SVERB,VERB):-same(VERB,SVERB).

% parses a verb phrase and retuns multiple interps
parse_verb_pharse(Agent,SVERB,ARGS,GOALANDLEFTOVERS):-
   findall([VERB|TYPEARGS],
    ((
     isa(Agent,What),
     moo:decl_action(What,TEMPL,_),
     TEMPL=..[VERB|TYPEARGS],
     verb_matches(SVERB,VERB))),
     TEMPLATES_FA),
   sort(TEMPLATES_FA,TEMPLATES),
   dmsg(parserm("TEMPLATES"=TEMPLATES)),
   TEMPLATES \= [],
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATES),
      phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver),
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

moo:specifier_text(Dir,dir):-member(Dir,[n,s,e,w,ne,nw,se,sw,u,d]).


moo:specifier_text(Text,Subclass):-moo:subclass(Subclass,spatialthing),isa(X,Subclass),req(keyword(X,Text)).

phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver):-
   to_word_list(ARGS,ARGSL),!,
    phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver).

phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver):-
    debugOnError(phrase(parseForTypes(TYPEARGS,GOODARGS),ARGSL,LeftOver)).    


parseForTypes([], [], A, A).
parseForTypes([TYPE|TYPES], [B|E], C, G) :-
        parseIsa(TYPE, B, C, F),
        parseForTypes(TYPES, E, F, G).

% parseIsa(T)-->parseIsa(T,_).
parseIsa(A, B, C) :-
        parseFmt(A, _, B, C).

% parseIsa(not(T),Term) --> dcgAnd(dcgNot(parseIsa(T)),theText(Term)).

parseIsa(optional(_Type,Who), Who, D, D).
parseIsa(optional(Type,_Who), Term, C, D) :- parseIsa(Type, Term, C, D).


parseIsa(not(Type), Term, C, D) :-  dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).

parseIsa(FT, B, C, D) :- trans_decl_subft(FT,Sub), parseFmt(Sub, B, C, D),!.
parseIsa(FT, B, C, D) :- trans_decl_subft(Sub,FT), parseFmt(Sub, B, C, D),!.
parseIsa(FT, B, C, D) :- parseFmt(FT, B, C, D),!.

trans_decl_subft(FT,Sub):-moo:decl_subft(FT,Sub).
trans_decl_subft(FT,Sub):-moo:decl_subft(FT,A),moo:decl_subft(A,Sub).
trans_decl_subft(FT,Sub):-moo:decl_subft(FT,A),moo:decl_subft(A,B),moo:decl_subft(B,Sub).


equals_icase(A,B):-string_upper(A,U),string_upper(B,U).

parseFmt(Type,Dir)--> {moo:specifier_text(Dir,Type)}, dcgReorder(theString(String),{equals_icase(Dir,String)}).

parseFmt(string,String)--> theString(String).
parseFmt(or([L|_]),Term) --> parseIsa(L,Term).
parseFmt(or([_|List]),Term) --> parseIsa(or(List),Term).

parseFmt(optional(Type,_),Term) --> parseIsa(Type,Term).
parseFmt(optional(_,Term),Term) --> [].

parseFmt(and([L|List]),Term1) --> dcgAnd(parseIsa(L,Term1),parseIsa(and(List),Term2)),{ignore(Term1==Term2),!}.
parseFmt(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{specifiedItemType(String,Type,Term)}.

specifiedItemType([String],Type,StringO):-nonvar(String),specifiedItemType(String,Type,StringO).
specifiedItemType(String,Type,Inst):- instances_of_type(Inst,Type),object_match(String,Inst),!.
specifiedItemType(A,T,AA):- is_decl_ft(T),!, format_complies(A,T,AA),!.

instances_of_type(Inst,Type):- isa(Inst,Type).
%% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], logOnError(req(Term)).



:- include(logicmoo('vworld/moo_footer.pl')).


