:- module(prueba_strips,_,_).

:- use_module(blockworld_pddl).

pre(A,B):- preconditions(A,B).

ach(C,D):- achieves(C,D).

del(E,F):- deletes(E,F).