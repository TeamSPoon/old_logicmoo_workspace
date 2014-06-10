/** <module> 
% Test worlds for predator world can be defined by a grid like the
% one below. The grid must be rectangular (ie. same number of
% columns for each row). It can be of any size.
%
% To look at the world, use the show_world/0 command. 
%
% The two letter codes used below are defined in map.objects.pl
%
% predator.map.pl
% July 10, 1996
% John Eikenberry
%
%
% Dec 13, 2035
% Douglas Miles
%
*/


dyn:grid_key(ed='edge of world').
dyn:grid_key(tr=tree).
dyn:grid_key( rk=rock).
dyn:grid_key( nt=nut).
%


dyn:type_grid(forest,1, [ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).
dyn:type_grid(forest,2, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
dyn:type_grid(forest,3, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
dyn:type_grid(forest,4, [ed,--,tr,--,nt,--,--,--,tr,tr,--,--,--,--,--,nt,nt,--,--,ed]).
dyn:type_grid(forest,5, [ed,--,--,tr,--,--,--,--,tr,--,--,--,--,nt,nt,nt,--,--,--,ed]).
dyn:type_grid(forest,6, [ed,--,--,--,--,--,tr,tr,tr,tr,tr,tr,--,--,nt,nt,--,--,--,ed]).
dyn:type_grid(forest,7, [ed,--,--,--,--,--,tr,--,--,--,--,--,--,--,--,nt,--,--,--,ed]).
dyn:type_grid(forest,8, [ed,--,--,--,--,--,--,--,--,--,--,--,tr,tr,tr,tr,tr,tr,--,ed]).
dyn:type_grid(forest,9, [ed,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
dyn:type_grid(forest,10,[ed,--,--,--,nt,--,--,--,--,--,--,--,--,--,--,--,--,--,--,ed]).
dyn:type_grid(forest,11,[ed,--,--,--,nt,--,--,--,--,--,tr,tr,--,--,--,--,--,--,--,ed]).
dyn:type_grid(forest,12,[ed,--,--,tr,tr,nt,nt,--,--,tr,tr,nt,nt,nt,nt,nt,nt,--,--,ed]).
dyn:type_grid(forest,13,[ed,--,--,--,--,--,nt,--,--,tr,--,--,nt,nt,nt,nt,nt,--,--,ed]).
dyn:type_grid(forest,14,[ed,--,--,--,--,--,--,--,tr,tr,--,--,--,--,nt,nt,--,--,--,ed]).
dyn:type_grid(forest,15,[ed,--,--,--,--,tr,tr,tr,tr,tr,--,--,--,nt,nt,--,--,--,--,ed]).
dyn:type_grid(forest,16,[ed,--,--,--,--,--,--,--,--,--,--,--,nt,nt,--,--,tr,--,--,ed]).
dyn:type_grid(forest,17,[ed,--,--,--,--,--,--,--,--,tr,--,nt,nt,--,tr,--,tr,--,--,ed]).
dyn:type_grid(forest,18,[ed,--,--,--,--,--,--,--,--,--,nt,nt,--,--,tr,--,--,--,--,ed]).
dyn:type_grid(forest,19,[ed,--,--,--,--,--,--,--,--,--,--,--,--,--,tr,--,--,--,--,ed]).
dyn:type_grid(forest,20,[ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed,ed]).


:- include(logicmoo(vworld/moo_footer)).


