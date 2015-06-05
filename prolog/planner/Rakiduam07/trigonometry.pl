:- module(trigonometry,[atan2/3,ajuste_angulo/2,radians_to_degrees/2]).
%Ver Casos especiales que evitan divición por 0
atan2(Dy,0,90):-Dy>0.
atan2(Dy,0,-90):-Dy=<0.

atan2(Dy,Dx,X):- %segundo cuadrante
	Dx < 0, Dy >0, X is truncate(180/3.14159265 * atan(Dy/Dx)+180).
atan2(Dy,Dx,X):- %tercer cuadrante
	Dx < 0, Dy < 0, X is truncate(180/3.14159265 * atan(Dy/Dx)-180).	
atan2(Dy,Dx,X):- %primero y cuarto cuadrante se dejan como estan
	 Dx>0, X is truncate(180/3.14159265 * atan(Dy/Dx)).

%ajusta el ángulo T en T2 para que este dentro del rango [-180,180]

ajuste_angulo(T,T2):-
	T =< 180,
	ajuste_angulo2(T,T2),!.
ajuste_angulo(T,T2):-
	T >180,
	T3 is T - 360,
	ajuste_angulo(T3,T2),!.
ajuste_angulo2(T,T):- (T > -180).
ajuste_angulo2(T,T2):-
	T =< 180,
	T3 is T + 360,
	ajuste_angulo2(T3,T2).


radians_to_degrees(Rads,Degrees) :-
	Degrees is Rads * 180/3.14159265.



