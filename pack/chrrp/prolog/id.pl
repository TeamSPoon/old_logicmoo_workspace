% Identifier Generation Module
%
% Part of the CHR-rp runtime system.
%
% Author: Leslie De Koninck, K.U.Leuven, Belgium

:- module(id,[gen_id/1]).


:- initialization nb_setval(id,0).

gen_id(Id) :- nb_getval(id,Id), NextId is Id + 1, b_setval(id,NextId).
