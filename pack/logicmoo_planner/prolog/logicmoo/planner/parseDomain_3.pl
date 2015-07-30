%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% parseDomain.pl
%%   Simple parser of PDDL domain file into prolog syntax.
%% Author: Robert Sasak, Charles University in Prague
%%
%% Example: 
%% ?-parseDomain('blocks_world.pddl', O).
%%   O = domain(blocks,
%%        [strips, typing, 'action-costs'],
%%        [block],
%%        _G4108,
%%        [ on(block(?x), block(?y)),
%%	         ontable(block(?x)),
%%	         clear(block(?x)),
%%	         handempty,
%%	         holding(block(?x)) ],
%%        [number(f('total-cost', []))],
%%        _G4108,
%%        [ action('pick-up', [block(?x)],       %parameters
%%		      [clear(?x), ontable(?x), handempty], %preconditions
%%		      [holding(?x)],                       %positiv effects
%%          [ontable(?x), clear(?x), handempty], %negativ effects
%%          [increase('total-cost', 2)]),        %numeric effects
%%         ...],
%%       ...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- expects_dialect(sicstus).


% parseDomain(+File, -Output).
%
% Parse PDDL domain File and return it rewritten prolog syntax.   
%
parseDomain(F, O):- parseDomain(F, O, _).


% parseDomain(+File, -Output, -RestOfFile)
%
% The same as above and also return rest of file. Can be useful when domain and problem are in one file.
%
parseDomain(File, Output, R) :-
    read_file(File, List),!,
    domainBNF(Output, List, R),!.

can_pddl_30.

pddl_3_0 --> {can_pddl_30}, [],!.
pddl_3_0(_Feature) --> {fail, can_pddl_30}, [],!.
pddl_3_0_e(_Feature) --> {fail, can_pddl_30}, [],!.

% Support for reading file as a list.
:-[readFile].

% Defining operator ?. It is a syntax sugar for marking variables: ?x
:-op(300, fy, ?).


% domainBNF(domain(N, R, T, C, P, F, C, S, Slack))
%
%   DCG rules describing structure of domain file in language PDDL.
% BNF description was obtain from http://www.cs.yale.edu/homes/dvm/papers/pddl-bnf.pdf
% This parser do not fully NOT support PDDL 3.0
% However you will find comment out lines ready for futher development.
%
domainBNF(domain(N, R, T, C, P, F, C, S, Slack))
			--> ['(','define', '(','domain'], name(N), [')'], 
                        dcgMust(domainBNF_rest( R, T, C, P, F, C, S, Slack)).
domainBNF_rest( R, T, C, P, F, C, S, Slack) --> 
                             dcgMust(dcgOptionalGreedy(require_def(R))),
                             dcgMust(dcgOptionalGreedy(types_def(T))),!, %:typing
                             dcgMust(constants_def(C)  ; []),
                             dcgMust(predicates_def(P) ; []),
                             dcgMust(functions_def(F)  ; []), %:fluents
%                            dcgMust(constraints(C)   ; []),    %:constraints
                             dcgMust(zeroOrMore(structure_def, S)),
                             zeroOrMore(anythings, Slack),
                             dcgMust([')']).

require_def(R)          --> ['(',':','requirements'], dcgMust((oneOrMore(require_key, R), [')'])).
require_key(strips)								--> [':strips'].
require_key(typing)								--> [':typing'].
require_key('action-costs')                             --> [':action-costs'].
require_key('goal-utilities')                             --> [':goal-utilities'].
%require_key('negative-preconditions')		--> [':negative-preconditions'].
%require_key('disjunctive-preconditions')	--> [':disjunctive-preconditions'].
require_key(equality)							--> [':equality'].
require_key('existential-preconditions')	--> [':existential-preconditions'].
require_key('universal-preconditions')		--> [':universal-preconditions'].
require_key('quantified-preconditions')	--> [':quantified-preconditions'].
require_key('conditional-effects')			--> [':conditional-effects'].
require_key(fluents)								--> [':fluents'].
require_key(adl)									--> [':adl'].
require_key('durative-actions')				--> [':durative-actions'].
require_key('derived-predicates')			--> [':derived-predicates'].
require_key('timed-initial-literals')		--> [':timed-initial-literals'].
require_key(preferences)						--> [':preferences'].
require_key(constraints)						--> [':constraints'].
% Universal requirements
require_key(R)		--> [':', R].
require_key(A)		--> [R],{atom_concat(':',A,R)}.
require_key(_)		--> [')'],!,{fail}.
require_key(R)		--> [R],{R\=')',trace}.

anythings(R)		--> [R],{R\=')',trace}.

types_def(L)			--> ['(',':',types],      typed_list(name, L), [')'].
constants_def(L)		--> ['(',':',constants],  typed_list(name, L), [')'].
predicates_def(P)		--> ['(',':',predicates], oneOrMore(atomic_formula_skeleton, P), [')'].

atomic_formula_skeleton(F)
				--> ['('], predicate(P), typed_list(variable, L), [')'], {F =.. [P|L]}.
predicate(P)			--> name(P).

variable(V)			--> ['?'], name(N), {V =.. [?, N]}.
atomic_function_skeleton(f(S, L))
				--> ['('], function_symbol(S), typed_list(variable, L), [')'].
function_symbol(S)		--> name(S).
functions_def(F)		--> ['(',':',functions], function_typed_list(atomic_function_skeleton, F), [')'].	%:fluents
constraints(C)                  --> pddl_3_0, ['(',':',constraints], con_GD(C), [')'].                                                   %:constraints
structure_def(A)		--> action_def(A).
%structure_def(D)		--> durative_action_def(D).	%:durativeactions
%structure_def(D)		--> derived_def(D).		%:derivedpredicates
%typed_list(W, G)		--> oneOrMore(W, N), ['-'], type(T), {(atom(T)-> G =.. [T,N] ; G = isa(N,T))}.
typed_list(W, [G|Ns])		--> oneOrMore(W, N), ['-'], type(T), !, typed_list(W, Ns), {(atom(T)-> G =.. [T|N] ; G = holds(T,N))}.
typed_list(W, N)		--> zeroOrMore(W, N).

effected_typed_list(W, [G|Ns])           --> oneOrMore(W, N), ['-'], effect(T), !, effected_typed_list(W, Ns), {G =.. [T,N]}.
effected_typed_list(W, N)                --> zeroOrMore(W, N).

primitive_type(N)		--> name(N).
type(either(PT))                --> ['(',either], !, dcgMust(( oneOrMore(primitive_type, PT), [')'])).
type(PT)			--> primitive_type(PT).
function_typed_list(W, [F|Ls])
				--> oneOrMore(W, L), ['-'], !, function_type(T), function_typed_list(W, Ls), {F =.. [T|L]}.	%:typing
function_typed_list(W, L)	--> zeroOrMore(W, L).

function_type(number)		--> [number].
emptyOr(_)			--> ['(',')'].
emptyOr(W)			--> W.

% Actions definitons
action_def(action(S, L, Precon, Pos, Neg, Assign))
                                --> ['(',':',action],
                                    dcgMust(action_symbol(S)),
                                    dcgMust(([':',parameters,'('], typed_list(variable, L), [')'])),{!},
                                    dcgMust((action_def_body(Precon, Pos, Neg, Assign))),
					[')'].
action_symbol(N)		--> name(N).

% % 2 ?- phrase(emptyOr(pre_GD(P)),['(',accessible,?,x,')','(','no-inventory-object',?,x,')','(','has-location',?,x,?,y,')'],X).
% % P = accessible(?x),
% % X = ['(', 'no-inventory-object', ?, x, ')', '(', 'has-location', ?, x|...] .

% PDDL 3.0 Way?
action_def_body(P, Pos, Neg, Assign)
				--> (([':',precondition], zeroOrMore(pre_GD,P)) /*	; []*/),
				    (([':',effect],      (emptyOr(effect(Pos, Neg, Assign))))	; []).


%% [1] 2 ?- pre_GD(X,['(',accessible,?,x,')'],[]).
%% X = accessible(?x) .

pre_GD(_)			--> [:,effect],{!,fail}.
% PDDL 2.x 
% pre_GD([F])                     --> atomic_formula(term, F), !.
pre_GD(P)			--> pref_GD(P).
% PDDL 2.x
% pre_GD(and(P))                  --> ['(',and],   zeroOrMore(pre_GD ,P), [')'].       
% PDDL 3.0 Way
pre_GD(P)                       --> ['(',and], dcgMust((pre_GD(P), [')'])).
pre_GD(forall(L, P))           -->  ['(',forall,'('],  dcgMust(((typed_list(variable, L), [')'], pre_GD(P), [')']))).         %:universal-preconditions
pref_GD(preference(N, P))      -->  ['(', preference], dcgOptionalGreedy(pref_name(N)),  dcgMust(gd(P)), dcgMust([')']).                         %:preferences
pref_GD(P)			--> gd(P).
pref_name(N)			--> name(N).
gd(L)				--> literal(term, L).								%:negative-preconditions

gd(F)				--> atomic_formula(term, F).	%: this option is covered by gd(L)
gd(P)				--> ['(',and],  zeroOrMore(gd, P), [')'].
%gd(or(P))                      --> pddl_3_0_e(gd), ['(',or],   zeroOrMore(gd ,P), [')'].                                       %:disjuctive-preconditions
%gd(not(P))                     --> pddl_3_0, ['(',not],  gd(P), [')'].                                                   %:disjuctive-preconditions
%gd(imply(P1, P2))              --> pddl_3_0_e(gd), ['(',imply], gd(P1), gd(P2), [')'].                                         %:disjuctive-preconditions
%gd(exists(L, P))               --> pddl_3_0_e(gd), ['(',exists,'('], typed_list(variable, L), [')'], gd(P), [')'].             %:existential-preconditions
%gd(forall(L, P))               --> pddl_3_0_e(gd), ['(',forall,'('], typed_list(variable, L), [')'], gd(P), [')'].             %:universal-preconditions
gd(F)				--> f_comp(F).	%:fluents
f_comp(compare(C, E1, E2))	--> ['('], binary_comp(C), f_exp(E1), f_exp(E2), [')'].
literal(T, F)			--> atomic_formula(T, F).
literal(T, not(F))		--> ['(',not], atomic_formula(T, F), [')'].
atomic_formula(_, F)		--> ['('], predicate(P), zeroOrMore(term, T), [')'], {F =.. [P|T]}.		% cheating, maybe wrong


term(N)				--> name(N).
term(V)				--> variable(V).
f_exp(N)                        --> number_sas(N).
f_exp(op(O, E1, E2))		--> ['('],binary_op(O), f_exp(E1), f_exp(E2), [')'].
f_exp('-'(E))			--> ['(','-'], f_exp(E), [')'].
f_exp(H)			--> f_head(H).
f_head(F)			--> ['('], function_symbol(S), zeroOrMore(term, T), [')'], { F =.. [S|T] }.
f_head(S)				--> function_symbol(S).
binary_op(O)			--> multi_op(O).
% binary_op(45)                   --> [45]. % 45 = minus = '-'  (TODO - WHY IS THIS HERE?)
binary_op('-')			--> ['−'].
binary_op('-')                  --> ['-'].
binary_op('/')			--> ['/'].
multi_op('*')			--> ['*'].
multi_op('+')			--> ['+'].
binary_comp('>')		--> ['>'].
binary_comp('<')		--> ['<'].
binary_comp('=')		--> ['='].
binary_comp('>=')		--> ['>='].
binary_comp('<=')		--> ['<='].
number_sas(N)                       --> [N],!, {number(N)}.
%number_sas(N)                       --> [N], {integer(N)}.
%number_sas(N)                       --> [N], {float(N)}.
effect(P, N, A)			--> ['(',and], c_effect(P, N, A), [')'].
effect(P, N, A)			--> c_effect(P, N, A).
%c_effect(forall(E))            --> pddl_3_0, ['(',forall,'('], effected_typed_list(variable,Es), [')', ')'].    %:conditional-effects
%c_effect(when(P, E))           --> pddl_3_0, ['(',when], gd(P), cond_effect(E), [')'].                   %:conditional-effects
c_effect(P, N, A)		--> p_effect(P, N, A).
p_effect([], [], [])		--> [].
p_effect(Ps, Ns, [F|As])
				--> ['('], assign_op(O), f_head(H), f_exp(E), [')'], p_effect(Ps, Ns, As), {F =.. [O, H, E]}.
p_effect(Ps, [F|Ns], As)	--> ['(',not], atomic_formula(term,F), [')'], p_effect(Ps, Ns, As).
p_effect([F|Ps], Ns, As)	--> atomic_formula(term, F), p_effect(Ps, Ns, As).
%p_effect(op(O, H, E))          --> pddl_3_0(op/3), ['('], assign_op(O), dcgMust((f_head(H), f_exp(E), [')'])).            %:fluents , What is difference between rule 3 lines above???
%cond_effect(E)		--> ['(',and], zeroOrMore(p_effect, E), [')'].				%:conditional-effects
%cond_effect(E)			--> p_effect(E).						%:conditional-effects
assign_op(assign)		--> [assign].
assign_op(scale_up)		--> [scale_up].
assign_op(scale_down)		--> [scale_down].
assign_op(increase)		--> [increase].
assign_op(decrease)		--> [decrease].


% BNF description include operator <term>+ to mark zero or more replacements.
% This DCG extension to overcome this. 
oneOrMore(W, [R|Rs], A, C) :- F =.. [W, R, A, B], F, (
                                                         oneOrMore(W, Rs, B, C)
                                                         ;
					(Rs = [] , C = B) 
				).
% BNF operator <term>*
zeroOrMore(W, R)		--> oneOrMore(W, R).
zeroOrMore(_, [])		--> [].

% Name is everything that is not number, bracket or question mark.
% Those rules are not necessary, but rapidly speed up parsing process.
name(N,S,E):-notrace(name0(N,S,E)).
name0(N)                         --> [N], {number(N), !, fail}.
name0(N)                         --> [N], {N=')', !, fail}.
name0(N)                         --> [N], {N='(', !, fail}.
name0(N)                         --> [N], {N='?', !, fail}.
name0(N)                         --> [N], {N='-', !, fail}.
%name0 NEVER (N)                 --> [N], {N=':', !, fail}.
name0(N)                         --> [N].


dcgMust(X)--> X.
dcgMust(X)--> {trace}, X.
