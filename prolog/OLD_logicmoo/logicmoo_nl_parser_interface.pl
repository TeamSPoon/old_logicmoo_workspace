%% Mats Dahllöf
%% Dept of Linguistics and Philology,
%% Uppsala university, 2004.
%% See: http://stp.ling.uu.se/~matsd/prolog_chartparser/

:-module(parser_interface,[dialog/0,
			   start_recording_events/0,
			   record_event/1,
			   test_parser/4]).

:- use_module(grammars).
:- use_module(chartparser).

:- dynamic current_event_data/2.
:- dynamic static_elements/2.
:- dynamic recorded_event/2.
:- dynamic event_number/0.
:- dynamic current_chartwindow/1.
:- dynamic distance_between_positions/1.
:- dynamic edge_counter/4.
:- dynamic edge_coordinates/7.
:- dynamic last_edge_shown/2.

dialog :-
        new(Dialog, dialog('Chart visualizer',size(1000,700))),
	send(Dialog, append, new(Menu1, menu('Grammar',cycle)),below),
	send(Dialog, append, new(Menu2, menu('Strategy')), right),
	send(Menu2, alignment, left),
        send(Dialog, append, new(Menu3, menu('Agenda')), right),
	send(Menu3, alignment, left),
        send(Dialog, append, new(T, text_item(input))),
	send(Dialog, append,new(B1, button('start stepwise',  message(@prolog,
					      start_visualize_stepwise,
					      T?selection,
					      Menu1?selection,
					      Menu2?selection,
					      Menu3?selection)))),
	send(B1, alignment, left),
	send(Dialog, append,
	     new(B2, button('step+1',  message(@prolog,
					       visualize_stepwise_forward)))),
	send(B2, alignment, left),
	send(Dialog, append,
	     new(B3, button('step-1',  message(@prolog,
					       visualize_stepwise_back)))),
	send(B3, alignment, left),
	send(Dialog, append,
	     new(B4, button(slow,  message(@prolog,
					   animate_events,
					   T?selection,
					   Menu1?selection,
					   Menu2?selection,
					   Menu3?selection,
					   0.4)))),
	send(B4, alignment, left),
	send(Dialog, append,
	     new(B4a, button('faster',  message(@prolog,
					   animate_events,
					   T?selection,
					   Menu1?selection,
					   Menu2?selection,
					   Menu3?selection,
					   0.1)))),
	send(B4a, alignment, left),
	send(Dialog, append,
	     new(B5, button(fast,  message(@prolog,
					   animate_events,
					   T?selection,
					   Menu1?selection,
					   Menu2?selection,
					   Menu3?selection,
					   0)))),
	send(B5, alignment, left),
	send(Dialog, append,
	     new(B6, button(tree, message(@prolog,show_tree)))),
	send(B6, alignment, left),
	send(Dialog, append,
	     new(B7, button(halt, message(@prolog,halt)))),
	send(B7, alignment, left),
	send(Dialog, append,
	     new(Window, picture('The Chart',size(1000,700)))), %% sizedata
        findall(Name,sample_grammar(Name,_),Names),
	send_list(Menu1,append,Names),
	send_list(Menu2,append,['top-down','bottom-up']),
	send_list(Menu3,append,[stack,queue]),
	send(Window,background,colour('grey95')), %% coldef
	send(Window, open),
	send(Window, flush),
	assert_current_chartwindow(Window),
	send(Dialog, open).

retract_dynamic_predicates:-
    retractall(recorded_event(_,_)),
    retractall(event_number(_)),
    retractall(current_event_data(_,_)),
    retractall(static_elements(_,_)),
    retractall(distance_between_positions(_)),
    retractall(edge_counter(_,_,_,_)),
    retractall(edge_coordinates(_,_,_,_,_,_,_)).

start_recording_events:-
    retractall(recorded_event(_,_)),
    retractall(event_number(_)),
    assert(event_number(0)).

next_event_number(Event_number_next):-
    event_number(Event_number),
    Event_number_next is Event_number + 1,
    retractall(event_number(_)),
    assert(event_number(Event_number_next)).
    
record_event(edge_addition(Edge,Origin,Agenda_in,
			   Agenda_out,Activation_tasks)):-
    !,
    agenda_size(Agenda_in,Size1),
    agenda_size(Agenda_out,Size2),
    next_event_number(Event_number_next),
    assertz(recorded_event(Event_number_next,
			   edge_addition(Edge,Origin,Size1,Size2,
					 Activation_tasks))).

record_event(look_up(Position,Word,Task_number,Edges,New_edges)):-
    !,
    next_event_number(Event_number_next),
    length(Edges,Result1),
    length(New_edges,Result2),
    assertz(recorded_event(Event_number_next,
			   look_up(Position,Word,Task_number,
				   Result1,Result2))).

record_event(bu_activate(Position1,Position2,Category,
				Task_number,Edges,New_edges)):-
    !,
    next_event_number(Event_number_next),
    length(Edges,Result1),
    length(New_edges,Result2),
    assertz(recorded_event(Event_number_next,
			   bu_activate(Position1,Position2,Category,
				       Task_number,Result1,Result2))).

record_event(predict(Position,Category,
		     Task_number,Edges,New_edges)):-
    !,
    next_event_number(Event_number_next),
    length(Edges,Result1),
    length(New_edges,Result2),
    assertz(recorded_event(Event_number_next,
			   predict(Position,Category,
				Task_number,Result1,Result2))).

record_event(fundamental_task(Edge1,Edge2,Task_number,Agenda,
			      Status,New_edges)):-
    !,
    next_event_number(Event_number_next),
    agenda_size(Agenda,Agenda_size),
    length(New_edges,Result),
    assertz(recorded_event(Event_number_next,
			   fundamental_task(Edge1,Edge2,Task_number,
					    Agenda_size,Status,Result))).
record_event(Event):-
    next_event_number(Event_number_next),
    assertz(recorded_event(Event_number_next,Event)).

clear_area:-
    current_chartwindow(Window),
    send(Window, clear),
    send(Window, flush).
  
parse_from_window_data('',Grammar,Strategy,Agenda_style):-
    !,
    parse_from_window_data('the dog watched the cat',
			   Grammar,Strategy,Agenda_style).

parse_from_window_data(Atom,Grammar,Strategy,Agenda_style):-
    retract_dynamic_predicates,
    clear_area,
    make_word_list_from_atom(Atom,String),
    parse_and_retrieve_result(String,Grammar,Strategy,Agenda_style),
    !,
    compute_all_edge_coordinates(1),
    recorded_event(1,Event),
    visualize(Event,Elements,Static_elements),
    assert(static_elements(2,Static_elements)),
    assert(current_event_data(2,Elements)).

parse_and_retrieve_result(Input_string,
                          Grammar,
                          Strategy,
                          Agenda_policy):-
    parse(Input_string,
          Grammar,
          Strategy,
          Agenda_policy,
          Number_of_words_in_input,
          Chart_final),
                                %     Find the set of complete analyses
                                %     and print them.
    findall(edge(Position1,
                 Position2,
                 Category,
                 [], %% i.e. inactive
                 Tree),
            edge_in_chart(edge(Position1,
                               Position2,
                               Category,
                               [], %% i.e. inactive
                               Tree),
                          Chart_final),
            Inactive_edges),
    length(Inactive_edges,No_of_Inactive_edges),
    findall(edge(Position1,
                 Position2,
                 Category,
                 [Cat|Cats], %% i.e. active
                 Tree),
            edge_in_chart(edge(Position1,
                               Position2,
                               Category,
                               [Cat|Cats], %% i.e. active
                               Tree),
                          Chart_final),
            Active_edges),
    length(Active_edges,No_of_Active_edges),
    start_symbol(Start_symbol),
    findall(Tree,edge_in_chart(edge(0,
                                    Number_of_words_in_input,
                                    Start_symbol,
                                    [],        %% i.e. inactive
                                    Tree),
                               Chart_final),
            Analyses),
    record_event(end(Analyses,
                     No_of_Inactive_edges,
                     No_of_Active_edges)).

start_visualize_stepwise(Atom,Grammar,Strategy,Agenda_style):-
    parse_from_window_data(Atom,Grammar,Strategy,Agenda_style).

visualize_stepwise_forward:-
    current_event_data(Event_number,Elements),
    recorded_event(Event_number,Event),
    !,
    destroy_elements(Elements),
    visualize(Event,Elements2,Static_elements),
    Event_number_next is Event_number + 1,
    assert(static_elements(Event_number_next,Static_elements)),
    retractall(current_event_data(_,_)),
    assert(current_event_data(Event_number_next,Elements2)).

visualize_stepwise_forward:-
    !.
    
visualize_stepwise_back:-
    current_event_data(Event_number,Elements),
    Event_number > 2,
    !,
    destroy_elements(Elements),
    retractall(current_event_data(_,_)),
    static_elements(Event_number,Static_elements),
    retractall(static_elements(Event_number,_)),
    destroy_elements(Static_elements),
    
    Event_number_previous is Event_number - 1,
    static_elements(Event_number_previous,Static_elements1),
    retractall(static_elements(Event_number_previous,_)),
    destroy_elements(Static_elements1),

    Event_number_previous2 is Event_number - 2,
    recorded_event(Event_number_previous2,Event),
    visualize(Event,Elements2,Static_elements2),
    assert(static_elements(Event_number_previous,Static_elements2)),
    assert(current_event_data(Event_number_previous,Elements2)).

visualize_stepwise_back:-
    !.
    
animate_events(Atom,Grammar,Strategy,Agenda_style,Sleep_time):-
    parse_from_window_data(Atom,Grammar,Strategy,Agenda_style),
    current_event_data(Event_number,Elements),
    recorded_event(Event_number,Event),
    !,
    destroy_elements(Elements),
    visualize(Event,Elements2,_),
    Event_number_next is Event_number + 1,
    retractall(current_event_data(_,_)),
    assert(current_event_data(Event_number_next,Elements2)),
    animate_events_aux(Sleep_time).

animate_events_aux(Sleep_time):-
    current_event_data(Event_number,Elements),
    recorded_event(Event_number,Event),
    !,
    destroy_elements(Elements),
    visualize(Event,Elements2,_),
    Event_number_next is Event_number + 1,
    retractall(current_event_data(_,_)),
    assert(current_event_data(Event_number_next,Elements2)),
    sleep(Sleep_time),
    animate_events_aux(Sleep_time).

animate_events_aux(_).
   
make_word_list_from_atom(Atom,Word_list):-
	name(Atom,Chars),
	tokenize(Chars,Char_lists),
	turn_into_atoms(Char_lists,Word_list).

tokenize([],[[]]).
	 
tokenize([32|Chars],[[]|Char_lists]):-
	!,
	tokenize(Chars,Char_lists).

tokenize([Char|Chars],[[Char|Char_list]|Char_lists]):-
	!,
	tokenize(Chars,[Char_list|Char_lists]).

turn_into_atoms([],[]).

turn_into_atoms([[]|T],TA):-
	!,
	turn_into_atoms(T,TA).

turn_into_atoms([H|T],[HA|TA]):-
	name(HA,H),
	turn_into_atoms(T,TA).

/**************************************************
 *** BASIC XPCE GRAPHICS PROCEDURES             ***
 **************************************************/

assert_current_chartwindow(Window):-
    retractall(current_chartwindow(_)),
    assert(current_chartwindow(Window)).

destroy_elements([]):-
    current_chartwindow(Window),
    send(Window, flush).

destroy_elements([H|T]):-
    send(H,destroy),
    destroy_elements(T).

xpce_draw_node(X,Y,Size,Colour,Node):-
    current_chartwindow(Window),
    send(Window,
	 display,
	 new(Node,circle(Size)),
	 point(X,Y)),
    send(Node,colour,Colour),
    send(Node,
	 fill_pattern,
	 colour(Colour)),  
    send(Window, flush).

xpce_write_text(Word,X,Y):-
    current_chartwindow(Window),
    send(Window,
	 display,
	 new(_, text(Word, font := boldlarge)),
	 point(X,Y)).
    
xpce_text_label(Text,X,Y,[Text_element]):-
    current_chartwindow(Window),
    Xtext is X + 10,
    Ytext is Y + 5,
    send(Window,
	 display,
	 new(Text_element, text(Text, font := boldlarge)),
	 point(Xtext,Ytext)),
    send(Window,flush).

xpce_text_box(Text,X,Y,Length,Colour,[Box,Text_element]):-
    current_chartwindow(Window),
    send(Window,
	 display,
	 new(Box, box(Length,25)),
	 point(X,Y)),
    send(Box,
	 colour,
	 colour(Colour)),
    send(Box,
	 fill_pattern,
	 colour(Colour)),
    Xtext is X + 10,
    Ytext is Y + 5,
    send(Window,
	 display,
	 new(Text_element, text(Text, font := boldlarge)),
	 point(Xtext,Ytext)),
    send(Window,flush).

xpce_text_ellipse(Text,X,Y,Length,Colour,[Box,Text_element]):-
    current_chartwindow(Window),
    send(Window,
	 display,
	 new(Box, ellipse(Length,25)),
	 point(X,Y)),
    send(Box,
	 colour,
	 colour(Colour)),
    send(Box,
	 fill_pattern,
	 colour(Colour)),
    Xtext is X + Length/2 -40,
    Ytext is Y + 5,
    send(Window,
	 display,
	 new(Text_element, text(Text, font := boldlarge)),
	 point(Xtext,Ytext)),
    send(Window,flush).

xpce_draw_bezier_curve(X1,Y1,X2,Y2,X3,Y3,Colour,Thickness,Curve):-
    current_chartwindow(Window),
    send(Window, 
	 display,
	 new(Curve, bezier_curve(point(X1,Y1),
				 point(X2,Y2),
				 point(X3,Y3)))),
    send(Curve,colour,Colour),
    send(Curve,pen,Thickness),
    send(Window,flush).
    
xpce_highlight_zero_span_edge(X,Y,Y_top,Colour,Box):-
    current_chartwindow(Window),
    Xbox is X - 3,
    Ybox is (Y_top - Y)/2,
    send(Window,
	 display,
	 new(Box, box(6,Ybox)),point(Xbox,Y)),
    send(Box,colour,Colour),
    send(Box, fill_pattern, colour(Colour)),
    send(Window,flush).
    
/**************************************************
 *** ''VISUALIZATION'' PROCEDURES               ***
 **************************************************/

visualize(nodes(Input_string),[],[]):-
    length(Input_string,N),
    N1 is N+1,
    draw_positions(N1).

visualize(end(Analyses,
	      No_of_inactive_edges,
	      No_of_active_edges),[],[]):-
    length(Analyses,No_of_analyses),
    concatenate_atoms(['Terminates. Analyses: ', No_of_analyses,
		       '. Inactive: ', No_of_inactive_edges,
		       '. Active: ', No_of_active_edges, '.'],Desc),
    make_boxed_info(Desc,lightblue,_), %% coldef
    report_analyses(Analyses).

visualize(edge_addition(edge(Position1,Position2,Cat,RHside,Tree),
			Origin,Size1,Size2,Activation_tasks),
	  Elements,Edge_elements):-
    origin_description(Origin,Desc,Brief),
    make_picture_of_rule_or_category(Cat,RHside,RuleAtom,Tree,Kind),
    concatenate_atoms([RuleAtom, ' ', ' [', Position1,',',
		       Position2, '] ', '(EDGE ', Kind, ', ' ,Desc, ').'],
		      Atom),
    retractall(last_edge_shown(_,_)),
    assert(last_edge_shown(edge(Position1,Position2,Cat,RHside,Tree),
			   Atom)),
    make_boxed_info(Atom,'lemon chiffon',Elements1), %% coldef
    concatenate_atoms(['Agenda: ', Size1, ', then ', Size2],Agenda_data),
    xpce_text_box(Agenda_data,710,10,200,linen,Elements2),
    display_activation_tasks(Activation_tasks,Elements3),
    draw_edge(edge(Position1,Position2,Cat,RHside,Tree),Brief,
	      Elements4,Edge_elements),
    append(Elements1,Elements2,Elements12),
    append(Elements3,Elements4,Elements34),
    append(Elements12,Elements34,Elements).

visualize(look_up(Position1,Word,Task_number,Result1,Result2),
	  [Box,Text,Box2,T2],[]):-    
    concatenate_atoms(['TASK (#', Task_number, ')',
		       ' lex. look-up fr. ', Position1, ' (hits: ',
		       Result1,', new: ', Result2,')'],Desc),
    position_coords(Position1,X,Y),
    Position2 is Position1 + 1,
    position_coords(Position2,X2,_),
    Length is X2-X,
    Y_below is Y+40,
    concatenate_atoms(['look-up: ', Word,
		       ' (',Result1,'/', Result2,')'],Label),
    xpce_text_ellipse(Label,X,Y_below,Length,'azure',[Box,Text]), %%coldef
    Y_words is Y+200,
    X_word is X+20,
    xpce_write_text(Word,X_word,Y_words),
    make_boxed_info(Desc,'azure',[Box2,T2]). %%coldef

visualize(predict(Position,Category,Task_number,Result1,Result2),
	  [Box,Text,Box2,T2],[]):-
    !,
    concatenate_atoms(['Prediction from ',
		       Position, ' of \'',Category,'\' (hits: ',
		       Result1,', new: ', Result2,') (TASK #',
		       Task_number, ')'],Desc),
    position_coords(Position,X,Y),
    Y_below is Y+40,
    Xellipse is X-40,
    concatenate_atoms(['predic: ',Category,
		       ' (',Result1,'/',Result2,')'],Label),
    xpce_text_ellipse(Label,Xellipse,Y_below,80,'LightYellow',[Box,Text]),
    make_boxed_info(Desc,'LightYellow',[Box2,T2]).

visualize(bu_activate(Position1,Position2,Category,
		      Task_number,Result1,Result2),
	  [Box,Text,Box2,T2],[]):-
    concatenate_atoms(['Bottom-up act. from ',
		       Position1, ' by \'',Category,'\'. (hits: ',
		       Result1,', New: ', Result2, ') (TASK #',
		       Task_number, ')'],Desc),
    position_coords(Position1,X,Y),
    position_coords(Position2,X2,_),
    Length is X2-X,
    Y_below is Y+40,
    concatenate_atoms(['bottom-up ', Category, ' (',Result1,'/',Result2,')'],
		      Label),
    xpce_text_ellipse(Label,X,Y_below,Length,'LightYellow',[Box,Text]), %% coldef
    make_boxed_info(Desc,'LightYellow',[Box2,T2]). %% coldef

visualize(fundamental_task(Edge1,Edge2,Task_number,
			   Agenda_size,Status,Result),
	  [N1,N2,N3,HE1,HE2,HEE1,HEE2,Box,T1|Elements],[]):-
	edge_coordinates(Edge1,
			 X1,Y1,X2,Y2,X_top1,Y_top1),
	highlight_edge(X1,Y1,X2,Y2,X_top1,Y_top1,[HE1,HE2],'act',
		       gold),%% coldef
	edge_coordinates(Edge2,
			 X2,Y2,X3,Y3,X_top2,Y_top2),
	highlight_edge(X2,Y2,X3,Y3,X_top2,Y_top2,[HEE1,HEE2],'inact',
		       'blue'), %% coldef
    Edge1=edge(Position1,Position2,_,_,_),
    Edge2=edge(_,Position3,_,_,_),
    highlight_position(Position1,N1,gold),
    task_status_colour(Status,Colour),
    highlight_position(Position2,N2,Colour),
    highlight_position(Position3,N3,'blue'), %% coldef
    concatenate_atoms(['Agenda: ', Agenda_size],Agenda_data),
    xpce_text_box(Agenda_data,710,10,200,linen,Elements),
    make_task_description(Edge1,Edge2,
			  Task_number,Status,Result,Desc),
    make_boxed_info(Desc,Colour,[Box,T1]).

task_status_colour(success,'spring green').%% coldef

task_status_colour(failure,'lightpink').%% coldef

display_activation_tasks([],[]).

display_activation_tasks([predict(Position,Cat)],Elements):-
    concatenate_atoms(['To predict: \'', Cat, '\' at ', Position],
		      Atom),
    xpce_text_box(Atom,710,33,200,linen,Elements). %% coldef
    
display_activation_tasks([bu_activate(Position1,_Position2,Cat)],Elements):-
    concatenate_atoms(['To b.u. act. from \'', Cat, '\' at ', Position1],
		      Atom),
    xpce_text_box(Atom,710,33,200,linen,Elements). %% coldef
    
/**************************************************
 *** LAYOUT PARAMETERS                          ***
 **************************************************/

position_coords(N,X,Y):-
    distance_between_positions(Distance_between_positions),
    X is N*Distance_between_positions+50,
    Y is 300.

edge_top_coordinate(inactive,Number,Span,Y):-
    Y is 300-(30*Number)-0.2*Span.

edge_top_coordinate(active,Number,Span,Y):-
    Y is 300+(30*Number)+0.2*Span.

number_of_edges(Kind,Position1,Position2,1):-
    \+edge_counter(Kind,Position1,Position2,_),
    !,
    assert(edge_counter(Kind,Position1,Position2,1)).

number_of_edges(Kind,Position1,Position2,Nr1):-
    edge_counter(Kind,Position1,Position2,Nr),
    !,
    Nr1 is Nr + 1,
    retractall(edge_counter(Kind,Position1,Position2,_)),
    assert(edge_counter(Kind,Position1,Position2,Nr1)).

/**************************************************
 *** TEXT STUFF                                 ***
 **************************************************/

make_task_description(edge(Position1,Position2,Cat1,Catlist1,Active_tree),
		      edge(Position2,Position3,Cat2,[],_),
		      Task_number,Status,_Result,Desc):-
    %result_atom(Result,Result_atom),
    find_before_dot_categories(Active_tree,Catlist2),
    make_dotted_rule_atom(Cat1,Catlist2,Catlist1,Dotted),
    concatenate_atoms([Dotted,
		       ' [', Position1, ',', Position2,'] + ',
		       Cat2,
		       ' [', Position2, ',', Position3, '] '
		       ,'(TASK #', Task_number, '/',
		       Status, ') ', '.'],
		      Desc).

result_atom(0,' (no new edge)').
result_atom(1,' (one new edge)').

find_before_dot_categories(_/[],[]).

find_before_dot_categories(Mother/[(Cat/_)|Subtrees],[Cat|Cats]):-
    find_before_dot_categories(Mother/Subtrees,Cats).

find_before_dot_categories(Mother/[(Cat-_)|Subtrees],[Cat|Cats]):-
    find_before_dot_categories(Mother/Subtrees,Cats).

make_dotted_rule_atom(Cat1,Catlist2,Catlist1,Dotted):-
    make_atom_picture_of_list(Catlist1,Atom1),
    make_atom_picture_of_list(Catlist2,Atom2),
    concatenate_atoms([Cat1, '--->', Atom2,'¤', Atom1],Dotted).

make_picture_of_rule_or_category(Cat,[],Atom,_,inactive):-
    concatenate_atoms([Cat],Atom).

make_picture_of_rule_or_category(Cat,RHside,Atom,Tree,active):-
    find_before_dot_categories(Tree,Catlist),
    make_dotted_rule_atom(Cat,Catlist,RHside,Atom).

origin_description(predict(Position,Cat),Desc,'pred'):-
    concatenate_atoms(['pred. by \'', Cat, '\' at ',
		       Position],Desc).

origin_description(bu_activate(Position1,Position2,Cat),Desc,'bu act'):-
    concatenate_atoms(['b.u. activation by \'', Cat, '\' at ',
		       Position1, '-', Position2],Desc).

origin_description(look_up(Position,_Word),Desc,'lexical'):-
    concatenate_atoms(['lex. fr. ', Position],Desc).

origin_description(task,'fr. task eval.','eval.').

/**************************************************
 *** DRAWING PROCEDURES                         ***
 **************************************************/

draw_positions(N):-
    draw_positions(0,N).

draw_positions(N,N):-
    !.
    
draw_positions(Nr,N):-
    draw_position(Nr),
    Nr2 is Nr + 1,
    draw_positions(Nr2,N).

draw_position(Nr):-
    position_coords(Nr,X,Y),
    XX is X - 4,
    YY is Y - 4,
    xpce_draw_node(XX,YY,8,brown,_).%%% coldef

highlight_position(Position,Node,Colour):-
    position_coords(Position,X,Y),
    XX is X - 7,
    YY is Y - 7,
    xpce_draw_node(XX,YY,14,Colour,Node).
    
make_boxed_info(Desc,Colour,[Box,Text_element]):-
    xpce_text_box(Desc,10,10,690,Colour,[Box,Text_element]).

compute_all_edge_coordinates(Event_number):-
    recorded_event(Event_number,Event),
    !,
    Event_number_next is Event_number + 1,
    compute_edge_coordinates(Event),
    compute_all_edge_coordinates(Event_number_next).

compute_all_edge_coordinates(_).
    
compute_edge_coordinates(nodes(Input_string)):-
    !,
    length(Input_string,N),
    Distance_between_positions is truncate(900/N), %% sizedata
    assert(distance_between_positions(Distance_between_positions)).

compute_edge_coordinates(edge_addition(edge(Position1,Position2,Cat,RH_list,Tree),
				       _,_,_,_)):-
    !,
    kind_of_edge(RH_list,Kind),
    position_coords(Position1,X1,Y1),
    position_coords(Position2,X2,Y2),
    X is (X1+X2)/2,
    number_of_edges(Kind,Position1,Position2,Number),
    Span is X2 - X1,
    edge_top_coordinate(Kind,Number,Span,Y),
    assert(edge_coordinates(edge(Position1,Position2,Cat,RH_list,Tree),
			    X1,Y1,X2,Y2,X,Y)).

compute_edge_coordinates(_).

kind_of_edge([_|_],active).
kind_of_edge([],inactive).

draw_edge(edge(Position1,Position2,Cat,RH_list,Tree),Info,
	  [N1,N2,HE1,HE2],[Curve]):-
    edge_coordinates(edge(Position1,Position2,Cat,RH_list,Tree),
		     X1,Y1,X2,Y2,X,Y),
    xpce_draw_bezier_curve(X1,Y1,X2,Y2,X,Y,black,2,Curve),
    highlight_position(Position1,N1,yellow), %% coldef
    highlight_position(Position2,N2,yellow), %% coldef
    highlight_edge(X1,Y1,X2,Y2,X,Y,[HE1,HE2],Info,yellow). %% coldef %% coldef
 
highlight_edge(X,Y1,X,_Y2,X_top1,Y_top1,[T1,B2],Text,Colour1):-
    !,
    xpce_highlight_zero_span_edge(X,Y1,Y_top1,Colour1,B2),
    Ylabel is Y1+(Y_top1 - Y1)/2,
    xpce_text_label(Text,X_top1,Ylabel,[T1]).

highlight_edge(X1,Y1,X2,Y2,X_top1,Y_top1,[T1,B2],Text,Colour1):-
    xpce_draw_bezier_curve(X1,Y1,X2,Y2,X_top1,Y_top1,Colour1,6,B2),
    Ylabel is Y1+(Y_top1 - Y1)/2,
    xpce_text_label(Text,X_top1,Ylabel,[T1]).

/**************************************************
 *** PROCEDURES FOR COMPILING ATOMS FOR TEXT    ***
 **************************************************/

concatenate_atoms(List,Atom):-
    character_list_from_atom_list(List,CharList),
    name(Atom,CharList).

character_list_from_atom_list([H|T],CharList):-
   name(H,Hlist),
   character_list_from_atom_list(T,Tlist),
   append(Hlist,Tlist,CharList).

character_list_from_atom_list([],[]).

make_atom_picture_of_list(List,Atom):-
    make_character_sequence_of_list(List,Chars),
    name(Atom,Chars).

make_character_sequence_of_list([],[]).

make_character_sequence_of_list([X],Chars):-
    !,
    name(X,Chars).

make_character_sequence_of_list([X|Tail],Chars):-
    name(X,XChars),
    make_character_sequence_of_list(Tail,TailChars),
    append(XChars,[32|TailChars],Chars).

%%************************************
%% report_analyses/1
%%************************************

report_analyses(Analyses):-
    length(Analyses,Number),
    write('=== Found '),
    write(Number),
    write(' complete syntax tree(s): '), nl,
    report_analyses_aux(Analyses).
    
report_analyses_aux([]).

report_analyses_aux([Item|Tail]):-
    write(Item),
    nl,
    report_analyses_aux(Tail).

test_parser(Input_string,
	    Grammar,
	    Strategy,
	    Agenda_policy):-
    parse(Input_string,
          Grammar,
          Strategy,
          Agenda_policy,
          Number_of_words_in_input,
          Chart_final),
                                %     Find the set of complete analyses
                                %     and print them.
    findall(edge(Position1,
                 Position2,
                 Category,
                 [], %% i.e. inactive
                 Tree),
            edge_in_chart(edge(Position1,
                               Position2,
                               Category,
                               [], %% i.e. inactive
                               Tree),
                          Chart_final),
            Inactive_edges),
    length(Inactive_edges,No_of_Inactive_edges),
    findall(edge(Position1,
                 Position2,
                 Category,
                 [Cat|Cats], %% i.e. active
                 Tree),
            edge_in_chart(edge(Position1,
                               Position2,
                               Category,
                               [Cat|Cats], %% i.e. active
                               Tree),
                          Chart_final),
            Active_edges),
    length(Active_edges,No_of_Active_edges),
    start_symbol(Start_symbol),
    findall(Tree,edge_in_chart(edge(0,
                                    Number_of_words_in_input,
                                    Start_symbol,
                                    [],        %% i.e. inactive
                                    Tree),
                               Chart_final),
            Analyses),
    write('Number of active edges: '), write(No_of_Active_edges), nl,
    write('Number of inactive edges: '), write(No_of_Inactive_edges), nl,
    write('Analyses: '), nl,
    write(Analyses).

show_tree:-
    last_edge_shown(edge(_Position1,_Position2,_Cat,_RHside,Tree),
		    Atom),
    !,
    show_tree(Tree,Atom).

show_tree.
	 
show_tree(Tree,Atom):-
    new(Dialog, dialog('Tree',size(800,600))),
    send_list(Dialog, append, 
	      [new(Window,
		   picture('The Chart',
			   size(800,600)))]),
    send(Dialog,
	 append,
	 new(_, button(exit, message(Dialog,
				     destroy)))),
    send(Window,background,colour(white)), %% coldef
    send(Window, open),
    assert_current_tree_window(Window),
    send(Window,
	 display,
	 new(_, text(Atom,font := boldhuge)),
	 point(5,10)),
    draw_a_tree(Tree),
    send(Window, flush),
    send(Dialog, open).

assert_current_tree_window(Window):-
    retractall(current_tree_window(_)),
    assert(current_tree_window(Window)).


draw_a_tree(Tree):-
    compute_x_coordinates(Tree,5,_,Annotated_tree),
    draw_tree_from_top(Annotated_tree,20).

draw_a_tree('no parse'):-
    show_message('problem: no parse').

draw_a_tree(Output):-
    \+(Output='problem: no parse'),
    \+tree_structure(Output),
    show_message('problem: wrong kind of parser output').

show_message(Text):-
    current_tree_window(Window),
    send(Window,
	 display,
	 new(Text_element, text(Text,font := boldhuge)),
	 point(50,50)),
    remember_picture_element(Text_element).

remember_picture_element(_).

compute_x_coordinates((Category-Word),X,X,l(Category,Word,X)).

compute_x_coordinates((Category/Trees),X_left,X_right,
		      p(Category,Annotated_trees,X)):-
    compute_x_coordinates(Trees,X_left,X_right,Annotated_trees),
    X is round((X_left+X_right)/2).

compute_x_coordinates([],X,X,[]).

compute_x_coordinates([Tree|Trees],
		      X_left,
		      X_right,
		      [Annotated_tree|Annotated_trees]):-
    compute_x_coordinates(Tree,X_left,X_next,Annotated_tree),
    (\+(Trees=[]) -> X_next2 is X_next + 15;
	             X_next2=X_next), 
    compute_x_coordinates(Trees,X_next2,X_right,Annotated_trees).
    
draw_tree_from_top(l(Category,Word,X),Y):-
    draw_node(X,Y,Category,Word).

draw_tree_from_top(p(Category,Annotated_trees,X),Y):-
    draw_node(X,Y,Category),
    Y_below is Y + 10,
    draw_tree_aux(Annotated_trees,Y_below,X,Y).

draw_tree_aux(l(Category,Trees,X),Y,X_mother,Y_mother):-
    connect_node(X,Y,X_mother,Y_mother),
    draw_node(X,Y,Category,Trees).

draw_tree_aux(p(Category,Annotated_trees,X),Y,X_mother,Y_mother):-
    connect_node(X,Y,X_mother,Y_mother),
    draw_node(X,Y,Category),
    Y_below is Y + 10,
    draw_tree_aux(Annotated_trees,Y_below,X,Y).

draw_tree_aux([],_,_,_).

draw_tree_aux([Tree|Trees],Y,X_mother,Y_mother):-
    draw_tree_aux(Tree,Y,X_mother,Y_mother),
    draw_tree_aux(Trees,Y,X_mother,Y_mother).

stretch_picture(X,Y,X2,Y2):-
    X2 is X*4,
    Y2 is Y*5.

draw_node(X,Y,Category,Word):-
    stretch_picture(X,Y,X_actual,Y_actual),
    current_tree_window(Window),
    X_actual_category is X_actual - 4,
    Y_actual_category is Y_actual + 1,
    send(Window,
	 display,
	 new(Text1, text(Category, font := boldlarge)),
	 point(X_actual_category,Y_actual_category)),
    Y_actual_word is Y_actual_category + 15,
    remember_picture_element(Text1),
    send(Window,
	 display,
	 new(Text2, text(Word, font := large)),
	 point(X_actual_category,Y_actual_word)),
    remember_picture_element(Text2),
    send(Window, flush),
    sleep(0.1).

draw_node(X,Y,Category):-
    stretch_picture(X,Y,X_actual,Y_actual),
    current_tree_window(Window),
    X_actual_category is X_actual - 4,
    Y_actual_category is Y_actual - 4,
    send(Window,
	 display,
	 new(Text, text(Category, font := boldlarge)),
	 point(X_actual_category,Y_actual_category)),
    remember_picture_element(Text),
    send(Text,colour,red),
    send(Window, flush),
    sleep(0.1),
    send(Text,colour,blue),
    send(Window, flush).

    
connect_node(X,Y,XM,YM):-
    stretch_picture(X,Y,X2,Y2),
    stretch_picture(XM,YM,XM2,YM2),
    current_tree_window(Window),
    YMa is YM2 + 16,
    Ya is Y2 - 2,
    send(Window,
	 display,
	 new(Line, line(X2,Ya,XM2,YMa))),
    remember_picture_element(Line),
    send(Line,pen,2),
    send(Line,colour,grey),
    send(Window, flush).



/*    
    show_tree(s/[np/[det-a, n_bar/[n_indef-dog]],
		 vp/[tv-watched, np/[det-a, n_bar/[n_indef-cat]]]]).
*/

:-dialog.