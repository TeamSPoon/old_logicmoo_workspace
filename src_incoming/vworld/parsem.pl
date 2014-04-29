%
% Dec 13, 2035
% Douglas Miles
%
/** <module>
% Initial Telnet/Text console parser (using DCG)
% Comments below document the basic idea.
%
*/

:- module(parsem, [
                   parse_agent_text_command/5,
                   specifier_text/2,
                   parseIsa//2,
                   parseForTypes//2]).


:- include(logicmoo('vworld/vworld_header.pl')).

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

specifier_text(Dir,dir):-member(Dir,[n,s,e,w,ne,nw,se,sw,u,d]).

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

parseIsa(not(Type), Term, C, D) :-  dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).

parseIsa(FT, B, C, D) :- trans_decl_subft(FT,Sub), parseFmt(Sub, B, C, D),!.
parseIsa(FT, B, C, D) :- trans_decl_subft(Sub,FT), parseFmt(Sub, B, C, D),!.
parseIsa(FT, B, C, D) :- parseFmt(FT, B, C, D),!.

trans_decl_subft(FT,Sub):-moo:decl_subft(FT,Sub).
trans_decl_subft(FT,Sub):-moo:decl_subft(FT,A),moo:decl_subft(A,Sub).
trans_decl_subft(FT,Sub):-moo:decl_subft(FT,A),moo:decl_subft(A,B),moo:decl_subft(B,Sub).



parseFmt(number,Term)--> dcgReorder(theText([String]),{any_to_number(String,Term)}).
parseFmt(string,Term)--> dcgReorder(theText([String]),{atom_string(Term,String)}).
parseFmt(or([L|_]),Term) --> parseIsa(L,Term).
parseFmt(or([_|List]),Term) --> parseIsa(or(List),Term).
parseFmt(and([L|List]),Term1) --> dcgAnd(parseIsa(L,Term1),parseIsa(and(List),Term2)),{ignore(Term1==Term2),!}.
parseFmt(item,Term)--> dcgAnd(dcgLenBetween(2,1),theText(String)),{specifiedItemType(String,item,Term)}.
parseFmt(actor,Term)--> dcgAnd(dcgLenBetween(2,1),theText(String)),{specifiedItemType(String,actor,Term)}.
parseFmt(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{specifiedItemType(String,Type,Term)}.

specifiedItemType([String],Type,StringO):-nonvar(String),specifiedItemType(String,Type,StringO).
specifiedItemType(A,T,AA):- format_complies(A,T,AA),!.
specifiedItemType(String,Type,String):- atom(Type), Term =..[Type,String], logOnError(req(Term)),!.

:- include(logicmoo('vworld/vworld_footer.pl')).


