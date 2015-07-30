
introduce(X) :- compile(X).
% introduce(X) :- consult(X).

bfile :- tell(freds),write('  domain '), domain_name(XX),  write(XX),nl,
         bfile(1,0,0,0,  T1,N1,S1),
         tell(freds), nl,nl, 
         write(S1),write('  '),write(N1),write('  '),write(T1),nl, 
         tell(fred),told,tell(freds),told,tell(user),!.
bfile(I,T,N,S,  T1,N1,S1) :- 
          tell(fred100),write('**** test '),write(I),nl,
          test(I),
          tell(freds),nl,write(I),write('  '),
            retract(time_taken(TIM)),
            retract(nodes_taken(ZZ)),
            retract(soln_size(SIZE)),
            write(SIZE),write('  '),write(ZZ),write('  '),write(TIM),nl,
            T2 is T + TIM, S2 is S + SIZE, N2 is N + ZZ,
          tell(user),write('**** test '),write(I),nl,

          J is I+1,bfile(J,T2,N2,S2,  T1,N1,S1),!.

bfile(_,T,N,S,  T,N,S) :- 
         write('++++++++++++++++++++++++++++++++++++++++++++'),!.

nodes_trace_file(fred).
results_file(fred100).
node_trace(off).

:- introduce('tests.pl').
:- introduce('../code.pl').
:- introduce('../../utilities/ob_utils').
:- introduce('sdomx4.pl').
:- change_op_representation, make_all_sort_levels.

% :- spy level_abstract, spy replaceH_steps, test1.
% :- spy take_out_ids_from_Left, test1.
% :- spy prove_them_all_and_put_them_back,test1.
% :-  spy prove_them_all_and_put_them_back, test2.
% :- spy expand_filter_in_hp,test1.
% :- spy prove_all_precons,spy get_sort_objects,test2.

