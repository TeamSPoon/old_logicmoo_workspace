/* <module>
% Imperitive Sentence Parser (using DCG)
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:-swi_module(parser_imperative, []).

:-swi_export((
                   parse_agent_text_command/5,            
                   parse_agent_text_command_0/5,            
                   parseIsa//2,
                   phrase_parseForTypes_9//2,
                   objects_match/3,
                   match_object/2,
                   object_string/2,
                   get_term_specifier_text/2,
                   parseForTypes//2)).

:- include(logicmoo('vworld/moo_header.pl')).

:- register_module_type(utility).

decl_database_hook(assert(_),C):- expire_tabled_list(C).
decl_database_hook(retract(_),C):- expire_tabled_list(C).

% =====================================================================================================================
% get_agent_text_command/4
% =====================================================================================================================
:-swi_export(get_agent_text_command/4).

get_agent_text_command(Agent,VERBOrListIn,AgentR,CMD):-
   debugOnError(loop_check(get_agent_text_command_0(Agent,VERBOrListIn,AgentR,CMD),fail)).

get_agent_text_command_0(Agent,ListIn,AgentR,CMD):- 
   (is_list(ListIn) -> UseList=ListIn ; UseList=[ListIn]),
       call_no_cuts(agent_text_command(Agent,UseList,AgentR,CMD)).

% ===========================================================
% DEBUG/NODEBUG command
% ===========================================================
type_action_info(human_player,debug(term),"Development Usage: debug  the blue backpack").

agent_call_command(Agent,debug(Term)):- agent_call_safely(Agent,debug(Term)).

% ===========================================================
% PARSE command
% ===========================================================
type_action_info(human_player,parse(prolog,list(term)),"Development test to parse some Text for a human.  Usage: parse 'item' the blue backpack").

agent_call_command(_Gent,parse(Type,StringM)):-
   parse_for(Type,StringM,_Term,_LeftOver).

% ===========================================================
% CMDPARSE command
% ===========================================================
type_action_info(human_player,cmdparse(list(term)),"Development test to parse some Text for a human.  Usage: cmdparse take the blue backpack").

agent_call_command(_Gent,cmdparse(StringM)):- parse_for(command,StringM,Term,LeftOver),fmt([Term,LeftOver]).

% mud_test("cmdparse test",...)
  

% ===========================================================
% parsetemps command
% ===========================================================
type_action_info(human_player,parsetemps(list(term)),"Development test to see what verb phrase heads are found. (uses get_vp_templates/4)  Usage: parsetemps who").

agent_text_command(Agent,[parsetemps|List],Agent,parsetemps(List)).

agent_call_command(Agent,parsetemps(StringM)):-
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

:-swi_export(parse_for/4).
parse_for(Type,StringM,Term,LeftOver):-
   to_word_list(StringM,String),  
   list_tail(String,LeftOver),
   HOW = phrase(parseIsa(Type,Term),String,LeftOver),
   fmt('parsing with ~q ~n.',[HOW]),
   (debugOnError(HOW)->
      fmt('Success! parse \'~q\' "~q" = ~q   (leftover=~q) . ~n',[Type,String,Term,LeftOver]);
      fmt('No Success.~n',[])).

meets_desc_spec(T,_L):- show_call(term_to_atom(T,S0)),string_to_atom(S0,A),atomic_list_concat_catch([_,_|_],'mudBareHandDa',A),!,fail.
meets_desc_spec(_,[]):-!.
meets_desc_spec(S,[DS|SL]):-!,meets_desc_spec(S,DS),meets_desc_spec(S,SL),!.
meets_desc_spec(S,From-To):-!, desc_len(S,Len),!, between(From,To,Len).
meets_desc_spec(_,_).

desc_len(S0,Region):- show_call(term_to_atom(S0,S)),
   atomic_list_concat_catch(Words,' ',S),length(Words,Ws),atomic_list_concat_catch(Sents,'.',S),length(Sents,Ss),Region is Ss+Ws,!.


:-swi_export(objects_match_for_agent/3).
objects_match_for_agent(Agent,Text,ObjList):- objects_match_for_agent(Agent,Text,[possess(Agent,value),same(atloc),same(localityOfObject),agent,item,region],ObjList).  
:-swi_export(objects_match_for_agent/4).
objects_match_for_agent(Agent,Text,Match,ObjList):- objects_for_agent(Agent,or([text_means(Agent,Text,value),and([or(Match),match_object(Text,value)])]),ObjList).  

text_means(Agent,Text,Agent):- equals_icase(Text,"self"),!.
text_means(Agent,Text,Loc):- equals_icase(Text,"here"),where_atloc(Agent,Loc).
text_means(Agent,Text,Region):- equals_icase(Text,"region"),where_atloc(Agent,Loc),locationToRegion(Loc,Region).
text_means(_Agent,_Text,_Value):-fail.

relates(Agent,Relation,Obj):-loop_check(relates_lc(Agent,Relation,Obj),fail).
relates_lc(Agent,Relation,Obj):-text_means(Agent,Relation,Obj),!.
relates_lc(_    ,Relation,Obj):- atom(Relation),col(Relation),!,isa(Obj,Relation).
relates_lc(_    ,Relation,Obj):-contains_var(Relation,value),subst(Relation,value,Obj,Call),!,req(Call).
relates_lc(Agent,same(Relation),Obj):- !, relates(Agent,Relation,Value),relates(Obj,Relation,Value).
relates_lc(Agent,Relation,Obj):- atom(Relation),!, prop(Agent,Relation,Obj).
relates_lc(_    ,Relation,Obj):-contains_var(Relation,Obj),!,req(Relation).
relates_lc(Agent,Relation,Obj):-contains_var(Relation,Agent),append_term(Relation,Obj,Call),!,req(Call).
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


objects_match(Text,Possibles,MatchList):- findall(Obj,(member(Obj,Possibles),match_object(Text,Obj)), MatchList).


object_string(O,String):-object_string(_,O,1-4,String),!.
object_string_0_5(O,String):-object_string(_,O,0-5,String),!.

:-swi_export(object_string/4).
:-dynamic object_string_fmt/3.
object_string(_,O,DescSpecs,String):- object_string_fmt(O,DescSpecs,String),!.
object_string(Agent,O,DescSpecs,String):- String = [O], 
   object_print_details(save_fmt(String),Agent,O,DescSpecs,[col,item,agent,channel]),
   asserta(object_string_fmt(O,DescSpecs,String)),!.

save_fmt(OS,' ~w ',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,'~w',[A]):-!,save_fmt_e(OS,A),!.
save_fmt(OS,Fmt,[A|KW]):-sformat(Str,Fmt,[A|KW]),to_word_list(Str,WL),save_fmt_e(OS,WL),!.

save_fmt_e(O,A):-atom(A),save_fmt_a(O,A),!.
save_fmt_e(O,[E|L]):-!,save_fmt_e(O,E),!,save_fmt_e(O,L),!.
save_fmt_e(O,isa(A)):-!,must(save_fmt_e(O,A)).
save_fmt_e(_,E):-compound(E),!. % cycPred(_),mped_type(_),cycPlus2(_),hasStub(_),def_module(_),stubType(_),arity(_),mped_type(_)
save_fmt_e(O,E):- string(E),!,must((to_word_list(E,WL),save_fmt_e(O,WL))),!.
save_fmt_e(O,E):- member(E,O) -> true ; (O=[_|CDR],nb_setarg(2,O,[E|CDR])).

save_fmt_a(_,A):-atom_length(A,L),L =< 1,!.
save_fmt_a(O,E):-atom_contains(E,'-'),!,must((to_word_list(E,WL),save_fmt_e(O,WL))),!.
save_fmt_a(O,E):-member(E,[obj,value,the,is,spatialthing,prologHybrid,prologOnly,relation,mpred,'',[]]),O\== E,!.


object_name_is_descriptive(O):- (isa(O,col);isa(O,mpred);isa(O,colDeclarer);isa(O,valuetype),isa(O,name_is_descriptive)).

:-swi_export(object_print_details/5).
object_print_details(Print,Agent,O,DescSpecs,Skipped):-
   member(O,Skipped) -> true ;
  (
   ignore((atom(O),to_word_list(O,WL),call(Print,' ~w ',[WL]))),
   forall(is_asserted(keyword(O,KW)),ignore((meets_desc_spec(KW,DescSpecs)->call(Print,' ~w ',[KW])))),
   forall(is_asserted(nameStrings(O,KW)),call(Print,' ~w ',[KW])),
   (object_name_is_descriptive(O) -> true ; 
    (( 
       forall(is_asserted(descriptionHere(O,KW)),ignore((meets_desc_spec(KW,DescSpecs)->call(Print,' ~w ',[KW])))),
       forall(isa(O,S),object_print_details(Print,Agent,S,DescSpecs,[O|Skipped])))))).

must_make_object_string_list(P,Obj,WList):- call_tabled_can(must_make_object_string_list_cached(P,Obj,WList)).
must_make_object_string_list_cached(P,Obj,WList):-
  must((object_string(P,Obj,0-5,String),nonvar(String),non_empty(String),string_ci(String,LString),to_word_list(LString,WList))).

same_ci(A,B):-must((non_empty(A),non_empty(B))),any_to_string(A,StringA),any_to_string(B,StringB),!,string_ci(StringA,StringB),!.

match_object(A,Obj):-same_ci(A,Obj),!.
match_object(A,Obj):-isa(Obj,Type),same_ci(A,Type),!.
match_object(S,Obj):-   
   atoms_of(S,Atoms),must(Atoms\=[]),
   current_agent_or_var(P),must_make_object_string_list(P,Obj,WList),!,
   forall(member(A,Atoms),member_ci(A,WList)).


dmsg_parserm(D):-dmsg(D),!.
dmsg_parserm(D):-ignore((debugging(parser),dmsg(D))).
dmsg_parserm(F,A):-ignore((debugging(parser),dmsg(F,A))).


% ===========================================================
% PARSER
% ===========================================================
:-debug(parser).
:-nodebug(parser).

parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL):-
  dmsg(parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL)),
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

parse_agent_text_command(Agent,IVERB,ARGS,Agent,GOAL):- 
  ground(IVERB), string_to_atom(IVERB,VERB),GOAL=..[VERB|ARGS],!.

% try directly parsing first
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(agent_text_command(Agent,[SVERB|ARGS],NewAgent,GOAL)),nonvar(NewAgent),nonvar(GOAL),!.   

% try indirectly parsing
parse_agent_text_command_0(Agent,SVERB,ARGS,NewAgent,GOAL):- 
   call_no_cuts(agent_text_command(Agent,[VERB|ARGS],NewAgent,GOAL)),ground(GOAL),nonvar(VERB),
   verb_matches(SVERB,VERB).

parse_agent_text_command_0(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL).

parse_agent_text_command_0(Agent,IVERB,ARGS,NewAgent,GOAL):-
   verb_alias_to_verb(IVERB,SVERB), IVERB\=SVERB,!,
   parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,GOAL).

parse_agent_text_command_0(Agent,PROLOGTERM,[],Agent,prologCall(call_mpred(PROLOGTERM))):- compound(PROLOGTERM),functor(PROLOGTERM,F,_),mpred_prop(F,_),!.
parse_agent_text_command_0(Agent,PROLOGTERM,[],Agent,prologCall(req(PROLOGTERM))):- compound(PROLOGTERM),is_callable(PROLOGTERM),!.

:-swi_export(parse_agent_text_command_1/5).
% parses a verb phrase and retuns one interpretation (action)
parse_agent_text_command_1(Agent,SVERB,ARGS,Agent,GOAL):-
   parse_vp_real(Agent,SVERB,ARGS,GOALANDLEFTOVERS),
   dmsg_parserm(parserm("GOALANDLEFTOVERS"=GOALANDLEFTOVERS)),
   GOALANDLEFTOVERS \= [],
   must(chooseBestGoal(GOALANDLEFTOVERS,GOAL)),
   dmsg_parserm(parserm("chooseBestGoal"=GOAL)).


verb_alias('l','look').
verb_alias('lo','look').
verb_alias('s','move s').
verb_alias('go','go').
verb_alias('where is','where').

% pos_word_formula('infinitive',Verb,Formula):- 'infinitive'(TheWord, Verb, _, _G183), 'verbSemTrans'(TheWord, 0, 'TransitiveNPCompFrame', Formula, _, _).

verb_alias_to_verb(IVERB,SVERB):- verb_alias(L,Look),verb_matches(L,IVERB),SVERB=Look,!.
verb_alias_to_verb(IVERB,SVERB):- specifiedItemType(IVERB,verb,SVERB), IVERB \= SVERB.

subst_parser_vars(Agent,TYPEARGS,TYPEARGS_R):- subst(TYPEARGS,self,Agent,S1),where_atloc(Agent,Here),subst(S1,here,Here,TYPEARGS_R).

% verb_matches("go",VERB):-!,VERB=go.
verb_matches(SVERB,VERB):-samef(VERB,SVERB).

get_vp_templates(_Agent,SVERB,_ARGS,TEMPLATES):-
   findall([VERB|TYPEARGS],
    ((
      get_type_action_templates(TEMPL),
     %isa(Agent,What),
     %action_info(What,TEMPL,_),
     TEMPL=..[VERB|TYPEARGS],
     verb_matches(SVERB,VERB))),
     TEMPLATES_FA),
    % ( TEMPLATES_FA=[] -> (dmsg(noTemplates(Agent,SVERB,ARGS)),!,fail); true),
   sort(TEMPLATES_FA,TEMPLATES),!.
   
% parses a verb phrase and retuns multiple interps
parse_vp_real(Agent,SVERB,ARGS,Sorted):-
   get_vp_templates(Agent,SVERB,ARGS,TEMPLATES),   
   dmsg_parserm(("TEMPLATES"= (orig([SVERB|ARGS]) = TEMPLATES))),
   TEMPLATES \= [],
   findall(LeftOver-GOAL,
     (( 
      member([VERB|TYPEARGS],TEMPLATES),
      subst_parser_vars(Agent,TYPEARGS,TYPEARGS_R),
      subst_parser_vars(Agent,ARGS,ARGS_R),
      dmsg_parserm(("parseForTypes"=phrase_parseForTypes(TYPEARGS_R,ARGS_R,GOODARGS,LeftOver))),      
      phrase_parseForTypes(TYPEARGS_R,ARGS_R,GOODARGS,LeftOver),
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

term_specifier_text(Dir,dir):-member(Dir,[n,s,e,w,ne,nw,se,sw,u,d]).

term_specifier_text(Text,Subclass):- 
   not(memberchk(Subclass,[dir,'TemporallyExistingThing'])),
   once((isa_asserted(X,Subclass),
   arg_to_var(text,Text,TextVar),
   req(keyword(X,TextVar)),   
   same_arg(text,TextVar,Text))). % dmsg(todo(term_specifier_text(Text,Subclass))),subclass_backchaing(Subclass,spatialthing).


phrase_parseForTypes(TYPEARGS,ARGS,GOODARGS,LeftOver):-length(TYPEARGS,N),length(GOODARGS,N),!,
  to_word_list(ARGS,ARGSL),!,phrase_parseForTypes_0(TYPEARGS,ARGSL,GOODARGS,LeftOver).

string_append(A,[B1,B2],C,ABC):-append(A,[B1,B2|C],ABC).
string_append(A,[B],C,ABC):-append(A,[B|C],ABC).


is_counted_for_parse(I):-i_countable(I),not(excluded_in_parse(I)),!.

excluded_in_parse(apath(_, _)).
excluded_in_parse(I):-col(I).
excluded_in_parse(I):-formattype(I).
excluded_in_parse(I):-mpred_prop(_,argsIsaInList(I)).
excluded_in_parse(apath(_ = _)).

instance_for_parse(I):-is_counted_for_parse(I).
insttype_for_parse(I):-findall(C,(instance_for_parse(I),isa(I,C)),List),list_to_set(List,Set),member(I,Set).

optional_strings_opt.

% optimization for optional strings
phrase_parseForTypes_0(TYPEARGS,ARGS,GOODARGS,LeftOver):- optional_strings_opt,
      (string_append(T1,[optionalStr(Str)],T2,TYPEARGS),
      (StrT =[_] /*;StrT=[_,_]*/),
      string_append(A1,StrT,A2,ARGS),
      equals_icase(Str,StrT)),!,
      (phrase_parseForTypes_1(T1,A1,G1,[]),
         phrase_parseForTypes_9(T2,A2,G2,LeftOver),      
         string_append(G1,[Str],G2,GOODARGS)).
      
phrase_parseForTypes_0(TYPEARGS,ARGS,GOODARGS,LeftOver):-phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver).

phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver):- catchv(phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver),_,fail),!.    
phrase_parseForTypes_1(TYPEARGS,In,Out,[]):- length(TYPEARGS,L),between(1,4,L),length(In,L),must(Out=In),!,dmsg(fake_phrase_parseForTypes_l(foreach_isa(In,TYPEARGS))).
phrase_parseForTypes_1(TYPEARGS,ARGS,GOODARGS,LeftOver):- debugOnError(phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver)).    

phrase_parseForTypes_9(TYPEARGS,ARGS,GOODARGS,LeftOver):- (LeftOver=[];LeftOver=[_|_] ), phrase(parseForTypes(TYPEARGS,GOODARGS),ARGS,LeftOver).

parseForTypes([], [], A, A).
parseForTypes([TYPE|TYPES], [B|E], C, G) :-
        no_repeats(parseIsa_Call(TYPE, B, C, F)),
        parseForTypes(TYPES, E, F, G),!.


parseIsa_Call(FT, BO, CIn, D):- list_tail(CIn,D), to_word_list(CIn,C),!, parseIsa(FT, B, C, D),to_arg_value(B,BO).


% this parseIsa(T)-->parseIsa(T,_).
parseIsa(A, B, C) :- parseIsa(A, _, B, C).

is_parsable_type(T):-formattype(T).
is_parsable_type(T):-col(T).
is_parsable_type(vp).


%:- begin_tests(test_bad_verb).

test(test_bad_verb, [ true(
       not(phrase(parseIsa(verb,ff),[ff],[]))
       )] ).


test(food_is_a_droppable, [ true(
       parse_agent_text_command(explorer(player1),drop,[food],_D2,_E2))]).


%:- end_tests(test_bad_verb).

query_trans_subft(FT,Sub):-subft(FT,Sub).
query_trans_subft(FT,Sub):-subft(FT,A),subft(A,Sub).
query_trans_subft(FT,Sub):-subft(FT,A),subft(A,B),subft(B,Sub).


parseFmt_vp1(Agent, do(NewAgent,Goal),[SVERB|ARGS],[]):- parse_agent_text_command(Agent,SVERB,ARGS,NewAgent,Goal),!.
parseFmt_vp2(Agent,GOAL,[SVERB|ARGS],UNPARSED):- parse_vp_real(Agent,SVERB,ARGS,TRANSLATIONS),!,member(UNPARSED-GOAL,TRANSLATIONS).

to_arg_value(random(Type),Term):- nonvar(Type),!, random_instance(Type,Term,true).
to_arg_value(call(Call),TermO):-nonvar(Call),subst(Call,value,Term,NewCall),!,must(req(NewCall)),to_arg_value(Term,TermO).
to_arg_value(Term,Term).

/*

some tests

 phrase_parseForTypes([optional(agent, random(agent))], ['Crush'], A, B).

  phrase_parseForTypes([optional(agent, random(agent))], ['Crush'], A, B).

parser_imperative:phrase_parseForTypes_9([optional(and([obj, not(region)]), 'NpcCol1000-Geordi684'), optionalStr("to"), optional(region, random(region))], [food, 'Turbolift'], GOODARGS,[]).
parser_imperative:phrase_parseForTypes_9([optional(and([obj, not(region)]), 'NpcCol1000-Geordi684')], [food], GOODARGS,[]).
parser_imperative:phrase_parseForTypes_9([optional(region, random(region))], ['Turbolift'], GOODARGS,[]).


*/

parseIsa(_T, _, [AT|_], _):- var(AT),!,fail.
parseIsa(FT, B, C, D):- var(FT),trace_or_throw(var_parseIsa(FT, B, C, D)).
parseIsa(Str,A,B,C) :-string(Str),!, parseIsa(exact(Str),A,B,C).

% this parseIsa(not(T),Term) --> dcgAnd(dcgNot(parseIsa(T)),theText(Term)).
parseIsa(not(Type), Term, C, D) :- !, dcgAnd(dcgNot(parseIsa(Type)), theText(Term), C, D).

parseIsa(vp,Goal,Left,Right):-!,one_must(parseFmt_vp1(self,Goal,Left,Right),parseFmt_vp2(self,Goal,Left,Right)).

parseIsa(call(Call),TermV) --> {!,subst(Call,value,TermV,NewCall)},[TermT], {trace,req(NewCall),match_object(TermT,TermV)}.
parseIsa(exact(Str),Str) --> {!},[Atom],{equals_icase(Atom,Str),!}.
parseIsa(optionalStr(Str),Str) --> {not(optional_strings_opt)},[Atom],{equals_icase(Atom,Str),!}.
parseIsa(optionalStr(Str),missing(Str)) --> {!},[].
parseIsa(optionalStr(_Str),_) --> {!,fail}.
parseIsa(optional(_,Term),TermV) --> {to_arg_value(Term,TermV)}, [TermT], {samef(TermV,TermT)}.
parseIsa(optional(Type, _), Term, C, D) :- nonvar(Type),parseIsa(Type, Term, C, D).
parseIsa(optional(_Type,Default), DefaultV, D, D2):- !,D=D2,to_arg_value(Default,DefaultV).

%  parser_imperative:phrase_parseForTypes_9([optional(and([obj, not(region)]),'NpcCol1000-Geordi684'),optionalStr("to"),optional(region, random(region))], [food], GOODARGS,[]).
%  parser_imperative:phrase_parseForTypes_9([optional(and([obj, not(region)]),'NpcCol1000-Geordi684'),optionalStr("to"),optional(region, random(region))], [food,to,'Turbolift'], GOODARGS,[]).
%  parser_imperative:phrase_parseForTypes_9([region], ['Turbolift'], GOODARGS,[]).


parseIsa(string,String)--> {!}, theString(String).
parseIsa(FT, B, [AT|C], D) :- nonvar(AT),member_ci(AT,["the","a","an"]),parseIsa(FT, B, C, D).


parseIsa(or(List),Term) --> {!,member(E,List)},parseIsa(E,Term).


parseIsa(list(Type),[Term|List]) --> parseIsa(Type,Term),parseIsa(list(Type),List).
parseIsa(list(_Type),[]) --> {!},[].

parseIsa(countBetween(_Type,_,High),[]) --> {High==0,!}, [].
parseIsa(countBetween(Type,Low,High),[Term|List]) --> parseIsa(Type,Term),{!,Low2 is Low -1,High2 is High -1 },
   parseIsa(countBetween(Type,Low2,High2),List).
parseIsa(countBetween(_Type,Low,_),[]) --> {!, Low < 1}, [].

% parseIsa(and([L|List]),Term1) --> dcgAnd(parseIsa(L,Term1),parseIsa(and(List),Term2)),{ignore(Term1==Term2),!}.
parseIsa(and([L]),Term1) --> {!},parseIsa(L,Term1).
parseIsa(and([L|List]),Term) --> {!},dcgAnd(parseIsa(L,Term),parseIsa(and(List),Term)).

parseIsa(Type,Term)--> dcgAnd(dcgLenBetween(1,2),theText(String)),{specifiedItemType(String,Type,Term)}.

specifiedItemType(String,Type,Inst):- (var(String);var(Type)),trace_or_throw(var_specifiedItemType(String,Type,Inst)).
specifiedItemType([String],Type,Inst):-!,specifiedItemType(String,Type,Inst).
specifiedItemType(String,not(Type),Inst):-!,not(specifiedItemType(String,Type,Inst)).
specifiedItemType(String,Type,Inst2):- get_term_specifier_text(Inst,Type),equals_icase(Inst,String),!,must(Inst=Inst2).
specifiedItemType(String,Type,Inst):- formattype(Type),checkAnyType(assert(parse),String,Type,AAA),Inst=AAA.
specifiedItemType(String,Type,Longest) :- findall(Inst, (get_term_specifier_text(Inst,Type),equals_icase(Inst,String)), Possibles), sort_by_strlen(Possibles,[Longest|_]),!.
specifiedItemType(String,Type,Inst):- not(formattype(Type)),must(col(Type)),instances_of_type(Inst,Type),match_object(String,Inst).

instances_of_type(Inst,Type):- no_repeats(isa(Inst,Type)).

% instances_of_type(Inst,Type):- atom(Type), Term =..[Type,Inst], logOnError(req(Term)).
% longest_string(?Order, @Term1, @Term2)
longest_string(Order,TStr1,TStr2):-
   text_to_string(TStr1,Str1),string_length(Str1,L1),
   text_to_string(TStr2,Str2),string_length(Str2,L2),
   compare(Order,L2-Str2,L1-Str1).

get_term_specifier_text(Text,Type):- must((var(Text),nonvar(Type))), call_tabled_can(no_repeats(call_no_cuts(term_specifier_text(Text,Type)))).

:- include(logicmoo('vworld/moo_footer.pl')).


