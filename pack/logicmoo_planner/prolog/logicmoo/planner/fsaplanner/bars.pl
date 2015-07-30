%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  The setup: 12 identical bars, except one that is heavier or lighter.
%  A balance scale with bars on each side can say which side is heavier 
%  The problem: in just 3 weighings, find the odd bar and if heavy or light
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- include(fsaplanner).
:- max_state(40).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

prim_fluent(odd_bar).	             % unknown pair (num,weight)
prim_fluent(answer_announced).       % false until answer is reported

prim_action(say(_,_),[ok]).          % announce the bar and its weight
prim_action(weigh(_,_),	[left_heavy,right_heavy,even_weight]).  % sense

poss(weigh(_,_),true).               % can weigh any 2 lists of bars     
poss(say(B,W),odd_bar=(B,W)).        % can only say when solution is known

causes(say(_,_),answer_announced,true,true).

% What each weighing tells us:  some of the pairs are rejected.
% Auxiliary predicates uneven/4 and even/4 are used to calculate them

rejects(weigh(X,Y),left_heavy,odd_bar,(B,W),true) :- uneven(X,Y,B,W).
rejects(weigh(X,Y),right_heavy,odd_bar,(B,W),true) :- uneven(Y,X,B,W).
rejects(weigh(X,Y),even_weight,odd_bar,(B,W),true) :- even(X,Y,B,W).

% Start with all 24 combinations for odd_bar.  Need to narrow this to 1.
init(odd_bar,(B,W)) :- inbetween(1,B,12), (W=heavy;W=light). 
init(answer_announced,false).

% The top level goal is to announce the answer
top :- kplan(answer_announced).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% This is a problem where the combinatorics are too high without filtering

% Only consider very few of the logically possible weighings via scale_set/3
% Roughly: 2 lists of the same size, with representatives of equiv classes.

:- good_action(weigh(X,Y),scale_set(X,Y,m(odd_bar))). 

% If N actions are left (so N-1 weighings), there are must be no more
% than 3^(N-1) possibilities left to disambiguate. Anything else is hopeless.

:- good_state(N,fewer_options(N,m(odd_bar))).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fewer_options(N,L) :- length(L,M), M =< 3^(N-1).

% inbetween(+I,-J,+K) is I <= J <= K.
inbetween(_,M,M).
inbetween(N,K,M) :- N<M, M1 is M-1, inbetween(N,K,M1).

% getting X heavy, Y light, rejects odd bar being num B with weight W.
uneven(_,Y,B,light) :- \+ member(B,Y). 
uneven(X,_,B,heavy) :- \+ member(B,X). 

% getting X and Y of even weight, rejects odd bar being B with weight W.
even(X,Y,B,_) :- member(B,X) ; member(B,Y).

% To manage combinatorics, (74,316 weighings/step = 4*10^14 total)
% will only consider weigh(X,Y) where X,Y are representatives from 
% equivalence classes according to CL, current possible values for odd_bar

scale_set(X,Y,CL) :- setof(B,inbetween(1,B,12),BL),
	equivs(BL,CL,N,H,L,B),		%  partition bars into N,H,L,B
	inbetween(1,K,6),		%  how many on each side of scale?
	appendn(XH,HR,H,0,K,X1),	%  bars from H on X side
	appendn(YH,_,HR,0,K,Y1),	%  bars from H on Y side
	appendn(XL,LR,L,0,X1,X2),	%  bars from L on X side
	appendn(YL,_,LR,0,Y1,Y2),	%  bars from L on Y side
	appendn(XB,BR,B,0,X2,X3),	%  bars from B on X side
	appendn(YB,_,BR,Y2,Y2,0),	%  bars from B on Y side
	appendn(XN,_,N,X3,X3,0),	%  bars from N on X side only
	concat(XN,XB,XL,XH,X),		%  cat them all to get X
	concat([],YB,YL,YH,Y).		%  cat them all to get Y

concat(A,B,C,D,X) :- append(C,D,X1), append(B,X1,X2), append(A,X2,X).
	
appendn([],X,X,L,U,U) :- L =< 0.
appendn([A|X],Y,[A|Z],L,U,R) :- 
	U > 0, L1 is L-1, U1 is U-1, appendn(X,Y,Z,L1,U1,R). 

% equivs(BL,P,N,H,L,B) partitions the bars in BL using P into
%   four lists:  N (neither) H (heavy only) L (light only) B (both)

equivs([],_,[],[],[],[]).
equivs([Bar|BL],P,N,H,L,B) :- 
	( member((Bar,heavy),P) ->
		( member((Bar,light),P) -> 
			(N=N1, H=H1, L=L1, B=[Bar|B1]) ;
			(N=N1, H=[Bar|H1], L=L1, B=B1) ) ;
		( member((Bar,light),P) -> 
			(N=N1, H=H1, L=[Bar|L1], B=B1) ;
			(N=[Bar|N1], H=H1, L=L1, B=B1) ) ),
	equivs(BL,P,N1,H1,L1,B1).
