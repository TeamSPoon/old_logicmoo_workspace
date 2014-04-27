%
% Dec 13, 2035
% Douglas Miles
%
/** <module>
% Initial Telnet/Text console parser (using DCG)
% Comments below document the basic idea.
%
*/

:- module(parsem, [specifier_text/2,
                  agent_message_stream/3,
                  do_player_action/1,
                   login_and_run/0,
                   parseIsa//2,
                   parseForTypes//2,
                   run_player/1,
		   generate_new_player/1]).

:- dynamic(agent_message_stream/3).

:- include(logicmoo('vworld/vworld_header.pl')).

:- register_module_type(utility).

% ===========================================================
% GAME REPL
% ===========================================================

foc_current_player(P):- thlocal:current_agent(P),!.
foc_current_player(P):-
   generate_new_player(P),
   !,
   asserta(thlocal:current_agent(P)).

generate_new_player(P) :-
  gensym(player,N),
   P=explorer(N),
   must(create_agent(P)).

login_and_run:-
  foc_current_player(P),
   fmt('~n~n~nHello ~w! Welcome to the MUD!~n',[P]),
   run_player(P),!,
   fmt('~n~n~Goodbye ~w! ~n',[P]).

run_player(P) :- repeat, once(read_and_do(P)), retract(wants_logout(P)).

read_and_do(P):- with_assertions(thlocal:current_agent(P),read_and_do0(P)).
read_and_do0(P):-
            must(ignore(look_brief(P))),!,
   current_input(Input),
   current_output(Output),
   retractall(agent_message_stream(P,_,_)),
   assert(agent_message_stream(P,Input,Output)),
            notrace((sformat(S,'~w>',[P]),prompt_read(S,List))),!,
            must(once(do_player_action(List))),!.

prompt_read(Prompt,Atom):-
        current_input(In),
        fresh_line,
        fmt0('~n~w ',[Prompt]),
	read_line_to_codes(In,Codes),
        foc_current_player(P),
         (is_list(Codes)-> atom_codes(Atom,Codes);
           assert(wants_logout(P))),!.

tick_tock:-fmt('tick tock',[]),sleep(1),!.


do_player_action(VA):- foc_current_player(Agent), call_player_action(Agent,VA),!.


call_player_action(Agent,CMD):-var(CMD),!,fmt('unknown_var_command(~q,~q).',[Agent,CMD]).
call_player_action(_,end_of_file):-tick_tock.
call_player_action(_,''):-tick_tock.
call_player_action(Agent,CMD):-do_player_action(Agent, CMD),!.
% call_player_action(Agent,CMD):-do_player_action(Agent, CMD),!.
call_player_action(Agent,CMD):-fmt('unknown_call_command(~q,~q).',[Agent,CMD]).



% execute a prolog command including prolog/0
do_player_action(Agent,[VERB|ARGS]):-
      debugOnError((moo:agent_text_command(Agent,[VERB|ARGS],NewAgent,CMD))),
      debugOnError(moo:agent_call_command(NewAgent,CMD)),!,
      atloc(Agent,Where),
      raise_location_event(Where,notice(reciever,do(Agent,CMD))),!.

% lists
do_player_action(A,Atom):-atom(Atom),atomSplit(Atom,List),do_player_action(A,List).
% prolog command
do_player_action(_Gent,Atom):- atom(Atom), catch(((once((read_term_from_atom(Atom,OneCmd,[variables(VARS)]),
      predicate_property(OneCmd,_),!,
      fmt('doing command ~q~n',[OneCmd]))), ignore((OneCmd,fmt('Yes: ~w',[VARS]),fail)))),_,fail).

% remove period at end
do_player_action(A,PeriodAtEnd):-append(New,[(.)],PeriodAtEnd),!,do_player_action(A,New).

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

moo:agent_text_command(Agent,[SVERB|ARGS],Agent,GOAL):-
   parse_agent_text_command(Agent,SVERB,ARGS,GOAL),!.


% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command(Agent,SVERB,ARGS,GOAL):-
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
parseFmt(string,Term)--> dcgReorder(theText([String]),{trace,atom_string(Term,String)}).
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


