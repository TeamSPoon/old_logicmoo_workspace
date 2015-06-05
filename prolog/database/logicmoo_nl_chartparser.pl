%% Mats Dahllöf
%% Dept of Linguistics and Philology,
%% Uppsala university, 2004.
%% See: http://stp.ling.uu.se/~matsd/prolog_chartparser/

:-module(chartparser,[parse/6,
                      edge_in_chart/2,
                      start_symbol/1,
                      agenda_size/2]).

:- use_module(grammars).

:- use_module(parser_interface,[start_recording_events/0,
				record_event/1]).

:-op(900,xfx,::).   %% Lexical entries: Word::Category.
:-op(900,xfx,--->). %% PS rules: Category ---> List_of_categories.

%%*********************************
%% SYNTACTIC TREE OPERATIONS
%%**********************************

% make_leaf(+Category,+Word,-Tree)
%   Makes a minimal complete tree, Tree, with a 
%   preterminal, Category, dominating a terminal node, Word.

make_leaf(Category,Word,Category-Word).

% make_tree_without_daughters(+Category,-Tree)
%   Makes a tree, Tree, without daughters, with Category
%   labelling the mother node.

make_tree_without_daughters(Category,Category/[]).

% add_daughter_to_tree(+Tree_in,+Daughter,-Tree_out)
%   Makes a new tree, Tree_out, formed by adding the
%   tree, Daughter, as a new rightmost daughter to the
%   tree Tree_in.

add_daughter_to_tree(Mother/Daughter_list,
                     New_daughter,
                     Mother/New_daughter_list):-
    append(Daughter_list,
           [New_daughter],
           New_daughter_list).

%%***********************************
%% CHART OPERATIONS
%% (The chart is a list of edges.)
%%***********************************

% empty_chart(?Chart)
%   Chart is the empty chart.

empty_chart([]).

% edges_not_already_in_chart(+Edges1,+Chart,-Edges2)
%   Finds those edges, Edges2, among the edges in the
%   list Edges1 which are not already found in Chart.

edges_not_already_in_chart([],_,[]).

edges_not_already_in_chart([Edge|Edges1],Chart,Edges2):-
        edge_in_chart(Edge,Chart),
        edges_not_already_in_chart(Edges1,Chart,Edges2).

edges_not_already_in_chart([Edge|Edges1],Chart,[Edge|Edges2]):-
        \+edge_in_chart(Edge,Chart),
        edges_not_already_in_chart(Edges1,Chart,Edges2).
   
% add_edge_to_chart(+Edge,+Chart_in,-Chart_out)
%   Chart_out is the chart formed by adding Edge
%   to Chart_in.   

add_edge_to_chart(Edge,Chart_in,Chart_out):-
    append(Chart_in,[Edge],Chart_out).

% edge_in_chart(?Edge,+Chart)
%   Edge is an member of Chart.

edge_in_chart(Edge,Chart):-
    member(Edge,Chart).

%%*********************************
%% AGENDA OPERATIONS
%% The agenda is a list of tasks.
%% select_agenda_style/1 selects a
%% queue or stack behaviour.
%%**********************************

% empty_agenda(?Agenda)
%   Agenda is the empty agenda.

empty_agenda([]).

% retrieve_task_from_agenda(-Task,+Agenda_in,-Agenda_out)
%   Task is the first Task on Agenda_in. Agenda_out
%   is the agenda that remains after Task have been removed
%   from Agenda_in. 

retrieve_task_from_agenda(Task,[Task|Agenda_out],Agenda_out).

% agenda_size(+Agenda,-Size)
%   Size is the number of tasks on Agenda.

agenda_size(Agenda,Size):-
    length(Agenda,Size).

% select_agenda_policy(+Policy) 
%   Selects the agenda policy Policy, Policy=queue 
%   or Policy=stack, by redefining the dynamic
%   predicate store_tasks_on_agenda/3. Both
%   policies are implemented by means of append/3,
%   the difference being in which end the new tasks
%   are appended.

select_agenda_policy(queue):-
    retractall(store_tasks_on_agenda(_,_,_)),
    assert((store_tasks_on_agenda(New_tasks,
                                  Agenda_in,
                                  Agenda_out):-
                append(Agenda_in,
                       New_tasks,
                       Agenda_out))).

select_agenda_policy(stack):-
    retractall(store_tasks_on_agenda(_,_,_)),
    assert((store_tasks_on_agenda(New_tasks,
                                  Agenda_in,
                                  Agenda_out):-
                append(New_tasks,
                       Agenda_in,
                       Agenda_out))).

% parse(+Input_string,+Grammar,+Strategy,+Agenda_policy,
%       -Number_of_words_in_input,-Final_chart)
%   Builds the final chart given an input string,
%   a grammar (name), a strategy, and an agenda policy.
%   It also counts the number of words in the input
%   string.

parse(Input_string,
      Grammar,
      Strategy,
      Agenda_policy,
      Number_of_words_in_input,
      Final_chart):-
        %% This is for the ''recording'' process:
    start_recording_events,
        %% Select grammar, strategy, and agenda policy.
    select_grammar(Grammar),
    select_strategy(Strategy),
    select_agenda_policy(Agenda_policy),
        %% Start from an empty agenda.
    empty_agenda(Fresh_agenda),
    record_event(nodes(Input_string)),
        %% Store the lexical look-up tasks on the agenda.
    lexical_tasks(Input_string,
                  0,
                  Number_of_words_in_input,
                  Lexical_tasks),
    store_tasks_on_agenda(Lexical_tasks,
                          Fresh_agenda,
                          Lexical_agenda),
       %% Store the initial prediction task on the agenda.
       %% (initial_prediction_tasks/1 is strategy-dependent.)
    initial_prediction_tasks(Prediction_tasks),
    store_tasks_on_agenda(Prediction_tasks,
                          Lexical_agenda,
                          Agenda_with_prediction),
       %% Process (and update) the successive agendas
       %% until we arrive at an empty one.
       %% Start from an empty chart.
    empty_chart(Fresh_chart),
    process_agenda(Agenda_with_prediction,
                   1,
                   Fresh_chart,
                   Final_chart).

% process_agenda(+Agenda,+Task_number,+Chart_in,-Chart_out)
%   Processes the successive agendas, starting from Agenda 
%   and the chart Chart_in, until an empty agenda is 
%   reached, producing the final chart Chart_out.
%   Task_number gives a number to the first task on the
%   agenda. It is only used for reporting.

process_agenda(Agenda,
               _Task_number,
               Chart,
               Chart):-
    empty_agenda(Agenda).

process_agenda(Agenda,
               Task_number,
               Chart_in,
               Chart_out):-
    retrieve_task_from_agenda(Task,
                              Agenda,
                              Agenda_mediating),
    execute_task(Task,
                 Task_number,
                 Chart_in,
                 Chart_mediating,
                 Agenda_mediating,
                 Agenda_out),
    Task_number_next is Task_number + 1,
    process_agenda(Agenda_out,
                   Task_number_next,
                   Chart_mediating,
                   Chart_out).

% add_edges_and_update_agenda(+Edges,+Agenda_in,-Agenda_out,
%                             +Chart_in,-Chart_out,+Origin)
%   Adds the edges in the list Edges to Chart_in giving us
%   Chart_out and adds the new tasks motivated by the edges
%   to Agenda_in giving us Agenda_out. Origin is a term
%   describing the origin of the edge. It is only used for
%   reporting purposes, and plays no role in the parsing
%   process.

add_edges_and_update_agenda([],Chart,Chart,Agenda,Agenda,_).

add_edges_and_update_agenda([Edge|Edges],
                            Chart_in,
                            Chart_out,
                            Agenda1,
                            Agenda4,
                            Origin):-
    add_edge_to_chart(Edge,
                      Chart_in,
                      Chart_mediating),
    new_fundamental_tasks(Edge,
                          Chart_in,
                          Fundamental_tasks),
        % Rule activation, depending on strategy:
    rule_activation_tasks(Edge,
                          Activation_tasks),
    store_tasks_on_agenda(Fundamental_tasks,
                          Agenda1,
                          Agenda2),
    store_tasks_on_agenda(Activation_tasks,
                          Agenda2,
                          Agenda3),
    record_event(edge_addition(Edge,
                               Origin,
                               Agenda1,
                               Agenda2,
                               Activation_tasks)),
    add_edges_and_update_agenda(Edges,
                                Chart_mediating,
                                Chart_out,
                                Agenda3,
                                Agenda4,
                                Origin).

% lexical_tasks(+String,+Current_position,-End_position,-Tasks)
%   Transforms the input String from Current_position into a 
%   set of lexical look-up Tasks. Current_position is the position
%   directly to the left of the head of String. End_position is 
%   the number of words read from position 0.

lexical_tasks([],
              Number_of_words,
              Number_of_words,
              []).

lexical_tasks([Word|Words],
              Current_position,
              Number_of_words,
              [look_up(Current_position,Word)|Tasks]):-
    Next_position is Current_position + 1,
    lexical_tasks(Words,
                  Next_position,
                  Number_of_words,
                  Tasks).

% new_fundamental_tasks(+New_edge,+Chart,-New_tasks)
%   Returns the list of fundamental tasks New_tasks 
%   formed by New_edge and the edges in the Chart. 

new_fundamental_tasks(Inactive_edge,
                      Chart,
                      New_tasks):-
    Inactive_edge=edge(Position,_,_,[],_),
    findall(fundamental_task(Active_edge,Inactive_edge),
            (Active_edge=edge(_,Position,_,[_|_],_),
                edge_in_chart(Active_edge,Chart)),
            New_tasks).

new_fundamental_tasks(Active_edge,
                      Chart,
                      New_tasks):-
    Active_edge=edge(_,Position,_,[_|_],_),
    findall(fundamental_task(Active_edge,Inactive_edge),
            (Inactive_edge=edge(Position,_,_,[],_),
                edge_in_chart(Inactive_edge,Chart)),
            New_tasks).

%  select_strategy(+Strategy),
%    Selects Strategy, where Strategy='top-down' or
%    Strategy='bottom-up' by redefining the dynamic
%    predicates initial_prediction_tasks/1 and
%    rule_activation_tasks/2.

select_strategy('top-down'):-
    retractall(initial_prediction_tasks(_)),
    retractall(rule_activation_tasks(_,_)),
        %% Predict the start symbol at position 0: 
    assert((initial_prediction_tasks([predict(0,Start_symbol)]):-
           start_symbol(Start_symbol))),
        %% Inactive edges do not trigger predictions:
    assert((rule_activation_tasks(edge(_,_,_,[],_),[]))),
        %% Do not predict lexical categories:
    assert((rule_activation_tasks(edge(Position1,Position2,_,[Category|_],_),[]):- 
           preterminals(Lexical_categories),
           member(Category,Lexical_categories))),
        %% Predict non-lexical categories:
    assert((rule_activation_tasks(edge(Position1,Position2,_,[Category|_],_),
                                  [predict(Position2,Category)]):-
           preterminals(Lexical_categories),
            \+member(Category,Lexical_categories))).

select_strategy('bottom-up'):-
    retractall(initial_prediction_tasks(_)),
    retractall(rule_activation_tasks(_,_)),
        %% Do not make any (initial) prediction:
    assert(initial_prediction_tasks([])),
        %% Active edges do not trigger bottom-up rule activation:
    assert(rule_activation_tasks(edge(_,_,_,[_|_],_),[])), 
        %% Inactive edges trigger bottom-up rule activation:
    assert(rule_activation_tasks(edge(Position1,Position2,Category,[],_),
                                 [bu_activate(Position1,Position2,Category)])).

% execute_task(+Task,+Number,+Chart_in,-Chart_out,
%              +Agenda_in,-Agenda_out,)
%   Executes the Task with the chart and the agenda 
%   being Chart_in and Agenda_in, giving us Chart_out
%   and Agenda_out.

%   This clause is for the execution of successful fundamental tasks.

execute_task(fundamental_task(Edge1,Edge2),
             Task_number,Chart_in,Chart_out,Agenda_in,Agenda_out):-
    Edge1=edge(Position1,Position2,Active_category,
               [First_to_find|Tail_to_find],Active_tree),
    Edge2=edge(Position2,Position3,Inactive_category,
               [],Inactive_tree),
    First_to_find=Inactive_category, %% succeeds
    add_daughter_to_tree(Active_tree,Inactive_tree,New_tree),
    edges_not_already_in_chart([edge(Position1,Position3,
                                     Active_category,Tail_to_find,
                                     New_tree)],
                               Chart_in,New_edges),
    record_event(fundamental_task(Edge1,Edge2,Task_number,
                                  Agenda_in,success,New_edges)),
    add_edges_and_update_agenda(New_edges,Chart_in,Chart_out,
                                Agenda_in,Agenda_out,task).

%   This clause is for the execution of failing fundamental tasks.
%   (The execute_task/6 call succeeds anyway.)

execute_task(fundamental_task(Edge1,Edge2),
             Task_number,Chart,Chart,Agenda,Agenda):-
    Edge1=edge(_,_,_,[First_to_find|_],_),
    Edge2=edge(_,_,Inactive_category,_,_),
    \+(First_to_find=Inactive_category),
    record_event(fundamental_task(Edge1,Edge2,Task_number,
                                  Agenda,failure,[])). 

%   This clause is for lexical look-up.

execute_task(look_up(Position,Word),Task_number,
             Chart_in,Chart_out,Agenda_in,Agenda_out):-
    Next_position is Position + 1,
    findall(edge(Position,Next_position,Category,[],Tree),
            (Category::Word,
             make_leaf(Category,Word,Tree)),
            Edges),
    edges_not_already_in_chart(Edges,Chart_in,New_edges),
    record_event(look_up(Position,Word,Task_number,
                         Edges,New_edges)),
    add_edges_and_update_agenda(New_edges,Chart_in,Chart_out,
                                Agenda_in,Agenda_out,
                                look_up(Position,Word)).

%   This clause is for rule prediction. It will only be put to
%   use under the top-down strategy.

execute_task(predict(Position,Category),Task_number,
             Chart_in,Chart_out,Agenda_in,Agenda_out):-
    findall(edge(Position,Position,Category,RHitems,Tree),
            (Category--->RHitems,
             make_tree_without_daughters(Category,Tree)),
            Edges),
    edges_not_already_in_chart(Edges,Chart_in,New_edges),
    record_event(predict(Position,Category,Task_number,
                         Edges,New_edges)),
    add_edges_and_update_agenda(New_edges,Chart_in,Chart_out,
                                Agenda_in,Agenda_out,
                                predict(Position,Category)).

%   This clause is for bottom-up rule activation. It will only 
%   be put to use under the bottom-up strategy.

execute_task(bu_activate(Position1,Position2,Category),
             Task_number,Chart_in,Chart_out,
             Agenda_in,Agenda_out):-
    findall(edge(Position1,Position1,LHitem,[Category|RHitems],
                 Tree),
            (LHitem--->[Category|RHitems],
             make_tree_without_daughters(LHitem,Tree)),
            Edges),
    edges_not_already_in_chart(Edges,Chart_in,New_edges),
    record_event(bu_activate(Position1,Position2,Category,
                             Task_number,Edges,New_edges)),
    add_edges_and_update_agenda(New_edges,Chart_in,Chart_out,
                                Agenda_in,Agenda_out,
                                bu_activate(Position1,Position2,
                                            Category)).

:- dynamic start_symbol/1.
:- dynamic preterminals/1.
:- dynamic ---> /2.
:- dynamic :: /2.

% select_grammar(+Name)
%  Retracts the dynamic predicates start_symbol/1,
%  preterminals/1, --->/2, and ::/2, if they are 
%  previously defined, and asserts the facts 
%  stated in terms of these four predicates
%  found in the grammar by the name Name,
%  as defined by the sample_grammar/2 predicate.

select_grammar(Name):-
    retractall(start_symbol(_)),
    retractall(preterminals(_)),
    retractall((_--->_)),
    retractall((_::_)),
    sample_grammar(Name,Clauses),
    assert_clauses(Clauses).

assert_clauses([]).

assert_clauses([Clause|Clauses]):-
    assertz(Clause),
    assert_clauses(Clauses).



