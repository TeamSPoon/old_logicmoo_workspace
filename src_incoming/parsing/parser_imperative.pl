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
                   parse_agent_text_command_0/5,            
                   parseIsa//2,
                   parseIsa0//2,
                   parseForIsa//2,
                   objects_match/3,
                   object_match/2,
                   object_string/2,
                   order_descriptions/3,
                   get_term_specifier_text/2,
                   parseForTypes//2]).


:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

hook:decl_database_hook(assert(_),C):- expire_tabled_list(C).
hook:decl_database_hook(retract(_),C):- expire_tabled_list(C).


% =====================================================================================================================
% get_agent_text_command/4
% =====================================================================================================================
:-export(get_agent_text_command/4).

get_agent_text_command(Agent,VERBOrListIn,AgentR,CMD):-
   debugOnError(loop_check(get_agent_text_command_0(Agent,VERBOrListIn,AgentR,CMD),fail)).

get_agent_text_command_0(Agent,ListIn,AgentR,CMD):- 
   (is_list(ListIn) -> UseList=ListIn ; UseList=[ListIn]),
       call_no_cuts(moo:agent_text_command(Agent,UseList,AgentR,CMD)).

% ===========================================================
% DEBUG/NODEBUG command
% ===========================================================
moo:type_action_info(human_player,debug(term),"Development Usage: debug  the blue backpack").

moo:agent_call_command(Agent,debug(Term)):- agent_call_safely(Agent,debug(Term)).

% ===========================================================
% PARSE command
% ===========================================================
moo:type_action_info(human_player,parse(prolog,list(term)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack").

moo:agent_call_command(_Gent,parse(Type,StringM)):-
   parse_for(Type,StringM,_Term,_LeftOver).

% ===========================================================
% parsetemps command
% ===========================================================
moo:type_action_info(human_player,parsetemps(list(term)),"Development test to see what verb phrase heads are found. (uses get_vp_templates/4)  Usage: parsetemps who").

moo:agent_text_command(Agent,[parsetemps|List],Agent,parsetemps(List)).

moo:agent_call_command(Agent,parsetemps(StringM)):-
  to_word_list(StringM,[SVERB|ARGS]),
  get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),fmt(templates=TEMPLATES),
  ignore((
     parse_for(vp,StringM,Goal,LeftOver),
     fmt([goal=Goal,lfto=LeftOver]))).

% ===========================================================
% parse_for/2-N
% ===========================================================
parse_for(Type,StringM):- parse_for(Type,StringM, _Term).

parse_for(Type,StringM, Term):-parse_for(Type,StringM, Term, []).

list_tail(_,[]).
list_tail(String,LeftOver):-ground(String),to_word_list(String,List),length(List,L),!,between(1,L,X),length(LeftOver,X).

:-export(parse_for/4).
parse_for(Type,StringM,Term,LeftOver):-
   to_word_list(StringM,String),  
   list_tail(String,LeftOver),
   HOW = phrase(parseIsa(Type,Term),String,LeftOver),
   fmt('parsing with ~q ~n.',[HOW]),
   (debugOnError(HOW)->
      fmt('Success! parse \'~q\' "~q" = ~q   (leftover=~q) . ~n',[Type,String,Term,LeftOver]);
      fmt('No Success.~n',[])).

meets_desc_spec(T,_L):- term_to_atom(T,S0),string_to_atom(S0,A),atomic_list_concat_catch([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(_,[]):-!.
meets_desc_spec(S,[DS|SL]):-!,meets_desc_spec(S,DS),meets_desc_spec(S,SL),!.
meets_desc_spec(S,From-To):-!, desc_len(S,Len),!, between(From,To,Len).
meets_desc_spec(_,_).

desc_len(S0,Region):- term_to_atom(S0,S),
   atomic_list_concat_catch(Words,' ',S),length(Words,Ws),atomic_list_concat_catch(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.

order_descriptions(O,DescSpecs,ListO):-findall(S,(description(O,S),meets_desc_spec(S,DescSpecs)),Rev),reverse(Rev,List),delete_repeats(List,ListO).

delete_repeats([],[]):-!.
delete_repeats([Region|List],[Region|ListO]):-delete(List,Region,ListM), delete_repeats(ListM,ListO),!.

:-export(objects_match_for_agent/3).
objects_match_for_agent(Agent,Text,ObjList):- objects_match_for_agent(Agent,Text,[possess(Agent,value),same(atloc),same(inRegion),agent,item,region],ObjList).  
:-export(objects_match_for_agent/4).
objects_match_for_agent(Agent,Text,Match,ObjList):- objects_for_agent(Agent,or([text_means(Agent,Text,value),and([or(Match),object_match(Text,value)])]),ObjList).  

text_means(Agent,Text,Agent):- equals_icase(Text,"self"),!.
text_means(Agent,Text,Loc):- equals_icase(Text,"here"),where_atloc(Agent,Loc).
text_means(_Agent,_Text,_Value):-fail.

relates(Agent,Relation,Obj):-loop_check(relates_lc(Agent,Relation,Obj),fail).
relates_lc(Agent,Relation,Obj):-text_means(Agent,Relation,Obj),!.
relates_lc(_    ,Relation,Obj):- atom(Relation),type(Relation),!,isa(Obj,Relation).
relates_lc(_    ,Relation,Obj):-contains_term(Relation,value),subst(Relation,value,Obj,Call),!,req(Call).
relates_lc(Agent,same(Relation),Obj):- !, relates(Agent,Relation,Value),relates(Obj,Relation,Value).
relates_lc(Agent,Relation,Obj):- atom(Relation),!, prop(Agent,Relation,Obj).
relates_lc(_    ,Relation,Obj):-contains_term(Relation,Obj),!,req(Relation).
relates_lc(Agent,Relation,Obj):-contains_term(Relation,Agent),append_term(Relation,Obj,Call),!,req(Call).
relates_lc(Agent,Relation,Obj):-objects_for_agent(Agent,Relation,MatchList),MatchList\=[],!,member(MatchList,Obj).


objects_for_agent(_Agent,and([]),[]):-!.
objects_for_agent(_Agent,or([]),[]):-!.

objects_for_agent(Agent,or([Possible|Relations]),MatchList):-!,
   objects_for_agent(Agent,Possible,L1),
   objects_for_agent(Agent,Relations,L2),
   append(L1,L2,MatchListL),!,
   list_to_set(MatchListL,MatchList),!.

objects_for_agent(Agent,members(List,Relations),MatchList):-!, findall(Obj,(member(Obj,List),relates(Agent,Relations,Obj)),MatchListL),list_to_set(MatchListL,MatchList),!.

objects_for_agent(Agent,and([Possible|Relations]),MatchList):-!,
   objects_for_agent(Agent,Possible,L1),
   objects_for_agent(Agent,members(L1,Relations),MatchListL),
   list_to_set(MatchListL,MatchList),!.
objects_for_agent(Agent,Relation,MatchList):- findall(Obj, relates(Agent,Relation,Obj), MatchListL),list_to_set(MatchListL,MatchList),!.


objects_match(Text,Possibles,MatchList):- findall(Obj,(member(Obj,Possibles),object_match(Text,Obj)), MatchList).

:-dynamic object_string_used/2.



object_string(O,String):-object_string(_,O,1-4,String),!.

% fast maybe works slower in long run
object_string(Agent,O,DescSpecs,String):- fail,
 with_output_to(string(StringI),
   object_print_details(format,Agent,O,DescSpecs,[type,item,agent])),
  must(StringI\=[]),
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
save_ol_e(OS,isa(A)):-!,save_ol_e(OS,A).
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
save_fmt_e(OS,isa(A)):-!,save_fmt_e(OS,A).
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
   forall((req(keyword(O,KW)),meets_desc_spec(KW,DescSpecs)),call(Print,' ~w ',[KW])),
   forall((req(nameStrings(O,KW))/*,meets_desc_spec(KW,DescSpecs)*/),call(Print,' ~w ',[KW])),
   (isa(O,type);forall((isa(O,S), meets_desc_spec(isa(O,S),DescSpecs)),call(Print,' ~w ',[isa(S)]))),
   ignore((order_descriptions(O,DescSpecs,List),forall_member(M,List,call(Print,' ~w ',[M])))),
   forall(isa(O,S),object_print_details(Print,Agent,S,DescSpecs,[O|Skipped])) )).



object_match(SObj,Obj):-non_empty(SObj),non_empty(Obj), isaOrSame(Obj,SObj).
object_match(S,Obj):- 
   atoms_of(S,Atoms),
   current_agent_or_var(P),
   must((once((object_string(P,Obj,0-5,String))),nonvar(String),
   non_empty(String))),!,
   string_ci(String,LString),!,
   str_contains_all(Atoms,LString).


dmsg_parserm(D):-ignore((debugging(parser),dmsg(D))).
dmsg_parserm(F,A):-ignore((debugging(parser),dmsg(F,A))).


% ===========================================================
% PARSER
% ===========================================================
:-debug(parser).
:-nodebug(parser).

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
  parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),
   dmsg_parserm(succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),!.

parse_agent_text_command(Agent,VERB,[PT2|ARGS],NewAgent,GOAL):-
   atomic_list_concat_catch([VERB,PT2],'_',SVERB),
   parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!,
   dmsg_parserm(special_succeed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),!.

parse_agent_text_command(Agent,PROLOGTERM,[],Agent,prologCall(PROLOGTERM)):- must(nonvar(PROLOGTERM)),predicate_property(PROLOGTERM,_),!.

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
 dmsg(failed_parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL)),
 debugging(parser),
 debug,visible(+all),leash(+all), dtrace,
 parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL),!.

% try directly parsing first
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(moo:agent_text_command(Agent,[SVERB|ARGS],NewAgent,GOAL)),nonvar(NewAgent),nonvar(GOAL),!.   

% try indirectly parsing
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(moo:agent_text_command(Agent,[VERB|ARGS],NewAgent,GOAL)),ground(GOAL),nonvar(VERB),
   verb_matches(SVERB,VERB).

% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command_0(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS),
   dmsg_parserm(parserm("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg_parserm(parserm("chooseBestGoal"=GOAL)).

parse_agent_text_command_0(Agent,IVERB,ARGS,NewAgent,GOAL):-
   verb_alias_to_verb(IVERB,SVERB), IVERB\=SVERB,!,
   parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL).

parse_agent_text_command_0(Agent,IVERB,ARGS,Agent,GOAL):- nonvar(IVERB), string_to_atom(IVERB,VERB),GOAL=..[VERB|ARGS],!.

moo:verb_alias('l','look').
moo:verb_alias('lo','look').
moo:verb_alias('s','move s').
moo:verb_alias('go','go').
moo:verb_alias('where is','where').

% pos_word_formula('infinitive',Verb,Formula):- 'infinitive'(TheWord, Verb, _, _G183), 'verbSemTrans'(TheWord, 0, 'TransitiveNPCompFrame', Formula, _, _).

verb_alias_to_verb(IVERB,SVERB):- moo:verb_alias(L,Look),verb_matches(L,IVERB),SVERB=Look,!.
verb_alias_to_verb(IVERB,SVERB):-specifiedItemType(IVERB,verb,SVERB), IVERB \= SVERB.

% verb_matches("go",VERB):-!,VERB=go.
verb_matches(SVERB,VERB):-samef(VERB,SVERB).

get_vp_templates(_Agent,SVERB,_ARGS,TEMPLATES):-
   findall([VERB|TYPEARGS],
    ((
      get_type_action_templates(TEMPL),
     %isa(Agent,What),
     %moo:action_info(What,TEMPL,_),
     TEMPL=..[VERB|TYPEARGS],
     verb_matches(SVERB,VERB))),
     TEMPLATES_FA),
    % ( TEMPLATES_FA=[] -> (dmsg(noTemplates(Agent,SVERB,ARGS)),!,fail); true),
   sort(TEMPLATES_FA,TEMPLATES),!.
   
% parses a verb phrase and retuns multiple interps
parse_vp_real(Agent,SVERB,ARGS,Sorted):-
   get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),
   dmsg_parserm(("TEMPLATES"= ([SVERB|ARGS] = TEMPLATES))),
   TEMPLATES \= [],
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATES),
      dmsg_parserm(("parseForTypes"=phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver))),
      phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver),
      GOAL=..[VERB|GOODARGS])),
      GOALANDLEFTOVERS_FA),
   sort(GOALANDLEFTOVERS_FA,GOALANDLEFTOVERS),
   predsort(bestParse,GOALANDLEFTOVERS,Sorted).

chooseBestGoal([_LeftOver - GOAL],GOAL):-!.
chooseBestGoal(GOALANDLEFTOVERS,GOAL):-
   predsort(bestParse,GOALANDLEFTOVERS,Sorted),
   dmsg_parserm(("Sorted"=Sorted)),
   member(_LeftOver - GOAL,Sorted),!.

% bestParse(?Order, @Term1, @Term2)
bestParse(Order,LeftOver1-GOAL1,LeftOver2-GOAL2):-
   length(LeftOver1,L1),length(LeftOver2,L2),
   functor_safe(GOAL1,_,A1),functor_safe(GOAL2,_,A2),
   must(once(bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2))).

:-style_check(-singleton).

bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,L1,L2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,-A1,-A2), Order \== '='.
bestParse(Order,LeftOver1-GOAL2,LeftOver1-GOAL2,L1,L2,A1,A2):-
   compare(Order,GOAL1,GOAL2).

:-style_check(+singleton).

moo:term_specifier_text(Dir,dir):-member(Dir,[n,s,e,w,ne,nw,se,sw,u,d]).

moo:term_specifier_text(Text,Subclass):- 
   not(memberchk(Subclass,[dir,'TemporallyExistingThing'])),
   once((isa_asserted(X,Subclass),
   arg_to_var(text,Text,TextVar),
   req(keyword(X,TextVar)),   
   same_arg(text,TextVar,Text))). % dmsg(todo(term_specifier_text(Text,Subclass))),transitive_subclass(Subclass,spatialthing).

phrase_parseForTypes(TYPEARGS,GOODARGS,ARGS,LeftOver):-
   to_word_list(ARGS,ARGSL),!,
    phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver).

phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver):-
    catch(phrase(parseForTypes(TYPEARGS,GOODARGS),ARGSL,LeftOver),_,fail),!.    
phrase_parseForTypes_l(TypesIn,Out,In,[]):- length(TypesIn,L),between(1,4,L),length(In,L),must(Out=In),!,dmsg(fake_phrase_parseForTypes_l(foreach_isa(In,TypesIn))).
phrase_parseForTypes_l(TYPEARGS,GOODARGS,ARGSL,LeftOver):-
    debugOnError(phrase(parseForTypes(TYPEARGS,GOODARGS),ARGSL,LeftOver)).    

parseForTypes([], [], A, A).
parseForTypes([TYPE], [B], C, []) :-
        parseIsa(TYPE, B, C, []),!.
parseForTypes([TYPE|TYPES], [B|E], C, G) :-
        parseIsa(TYPE, B, C, F),
        parseForTypes(TYPES, E, F, G).

% this parseIsa(T)-->parseIsa(T,_).
parseIsa(A, B, C) :-
        parseIsa(A, _, B, C).

% this parseIsa(not(T),Term) --> dcgAnd(dcgNot(parseIsa(T)),theText(Term)).
parseIsa(_T, _, [AT|_], _):- var(AT),!,fail.

parseIsa(FT, B, C, D):- var(FT),trace_or_throw(var_parseIsa(FT, B, C, D)).

parseIsa(FT, B, C, D):- to_word_list(C,O),O\=C,!,parseIsa(FT, B, O, D).
parseIsa(FT, B, C, D):-  dbase:call_tabled(parseIsa0(FT, B, C, D)).

parseIsa0(FT, B, C, D):- list_tail(C,D),parseForIsa(FT, B, C, D).

is_parsable_type(T):-formattype(T).
is_parsable_type(T):-type(T).
is_parsable_type(vp).


%:- begin_tests(test_bad_verb).

test(test_bad_verb, [ true(
       not(phrase(parseIsa(verb,ff),[ff],[]))
       )] ).


test(food_is_a_droppable, [ true(
       parse_agent_text_command(explorer(player1),drop,[food],_D2,_E2))]).


%:- end_tests(test_bad_verb).


parseForIsa(actor,A,B,C) :- parseForIsa(agent,A,B,C).
parseForIsa(optional(Type,_Who), Term, C, D) :- nonvar(Type),parseForIsa(Type, Term, C, D).
parseForIsa(optional(_Type,Who), Who, D, D):-!. %,nonvar(Who).

parseForIsa(Var, _B, _C, _D):-var(Var),!,fail. % trace_or_throw(var_parseForIsa(Var, B, C, D)).
% parseForIsa(Var, B, C, D):-var(Var),!,trace_or_throw(var_parseForIsa(Var, B, C, D)).
parseForIsa(not(Type), Term, C, D) :-  dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).
parseForIsa(FT, B, C, D):-to_word_list(C,O),O\=C,!,parseForIsa(FT, B, O, D).

parseForIsa(FT, B, [AT|C], D) :- nonvar(AT),member_ci(AT,["at","the","a","an"]),!,parseForIsa(FT, B, C, D).
parseForIsa(FT, B, C, D) :- query_trans_sub(FT,Sub), parseFmtOrIsa(Sub, B, C, D),!.
parseForIsa(FT, B, C, D) :- query_trans_sub(Sub,FT), parseFmtOrIsa(Sub, B, C, D),!.
parseForIsa(FT, B, C, D) :- parseFmtOrIsa(FT, B, C, D),!.

query_trans_sub(Sub,Super):-query_trans_subft(Sub,Super).
query_trans_sub(Sub,Super):-transitive_subclass(Sub,Super).

query_trans_subft(FT,Sub):-moo:subft(FT,Sub).
query_trans_subft(FT,Sub):-moo:subft(FT,A),moo:subft(A,Sub).
query_trans_subft(FT,Sub):-moo:subft(FT,A),moo:subft(A,B),moo:subft(B,Sub).



parseFmtOrIsa(Var, _B, _C, _D):-var(Var),!,fail. % trace_or_throw(var_parseForIsa(Var, B, C, D)).
% parseFmtOrIsa(Var, B, C, D):-var(Var),!,trace_or_throw(var_parseForIsa(Var, B, C, D)).
parseFmtOrIsa(Sub, B, C, D):- parseFmt(Sub, B, C, D).

parseFmtOrIsa(vp,Goal,Left,Right):-!,one_must(parseFmt_vp1(self,Goal,Left,Right),parseFmt_vp2(self,Goal,Left,Right)).

parseFmt_vp1(Agent, do(NewAgent,Goal),[SVERB|ARGS],[]):- parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,Goal),!.
parseFmt_vp2(Agent,GOAL,[SVERB|ARGS],UNPARSED):- parse_vp_real(Agent,SVERB,ARGS,TRANSLATIONS),!,member(UNPARSED-GOAL,TRANSLATIONS).

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
specifiedItemType(A,T,AA):- nonvar(T), formattype(T),!, checkAnyType(assert(_),A,T,AAA),!,AA=AAA.
specifiedItemType(String,Type,Inst) :- get_term_specifier_text(Inst,Type),equals_icase(Inst,String),!.
specifiedItemType(String,Type,Inst):- instances_of_type(Inst,Type),object_match(String,Inst),!.
specifiedItemType(String,Type,Longest) :- findall(Inst, (get_term_specifier_text(Inst,Type),equals_icase(Inst,String)), Possibles), sort_by_strlen(Possibles,[Longest|_]),!.
specifiedItemType(A,T,AA):- checkAnyType(assert(parse),A,T,AAA),AA=AAA.

checkAnyType(Op,A,Type,AA):- var(A),correctAnyType(Op,A,Type,AA),must_det(var(AA)),must_det(A==AA),!.
checkAnyType(Op,A,Type,AA):- correctAnyType(Op,A,Type,AA),nonvar(AA),!.

instances_of_type(Inst,Type):- setof(Inst-Type,isa(Inst,Type),Set),member(Inst-Type,Set).
% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], logOnError(req(Term)).
% longest_string(?Order, @Term1, @Term2)
longest_string(Order,TStr1,TStr2):-
   text_to_string(TStr1,Str1),string_length(Str1,L1),
   text_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L2-Str2,L1-Str1).

get_term_specifier_text(Text,Type):- call_no_cuts(moo:term_specifier_text(Text,Type)).

:- include(logicmoo('vworld/moo_footer.pl')).


