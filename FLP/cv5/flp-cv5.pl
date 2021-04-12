
subbags([], [[]]).
%subbags([X|XS], P) :- ..

addOneToAll(_, [], []).
%addOneToAll(E, [L|LS], [[E|L]|T]) :- ...





:- dynamic robot/2, dira/1.

obsazeno(P) :- robot(_,P).
obsazeno(P) :- dira(P).
vytvor(I, P) :- not(obsazeno(P)), assertz(robot(I,P)).
vytvor(P) :- not(obsazeno(P)), assertz(dira(P)).

odstran(P) :- obsazeno(P), retract(robot(_,P)).
odstran(P) :- obsazeno(P), retract(dira(P)).

% pomocny predikat pro spojeni seznamu obsazenych pozic roboty a dirami
spoj([],L2,L2).
spoj(L2,[],L2).
spoj(L2,L2,[]).

spoj([H|R],[H|L1],L2) :- spoj(R,L1,L2),!.
spoj([H|L1],L2,[H|R]) :- spoj(L1,L2,R),!.

obsazene_pozice(X) :- obsazene_diry(L1), obsazene_roboty(L2), spoj(L1,L2,X).
obsazene_diry(X) :- bagof(P, dira(P), X).
obsazene_roboty(X) :- bagof(P, robot(_,P), X).



inkrementuj(X,Y) :- Y is X+1.
dekrementuj(X,Y) :- Y is X-1.
doleva(I) :- pohni(I, dekrementuj).
doprava(I) :- pohni(I, inkrementuj).
% robot se posune, pokud misto kam se chce posunout neni obsazeno
pohni(I, Operace) :- Operace = 'doleva', robot(I,P), dekrementuj(P,Y), not(obsazeno(Y)), odstran(P), vytvor(I,Y).
pohni(I, Operace) :- Operace = 'doprava', robot(I,P), inkrementuj(P,Y), not(obsazeno(Y)), odstran(P), vytvor(I,Y).
% robot spadne do diry a zmizi
pohni(I, Operace) :- Operace = 'doleva', robot(I,P), dekrementuj(P,Y), dira(Y), odstran(P).
pohni(I, Operace) :- Operace = 'doprava', robot(I,P), inkrementuj(P,Y), dira(Y), odstran(P).

armageddon :- obsazene_roboty(X), forall(member(P, X), vybuch(P)).
vybuch(P) :- odstran(P), vytvor(P).




g_size(3).


g_test(X:Y) :- X > 0, X < 4, Y > 0, Y < 4.



g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).

g_one(X:Y, Len, L, Res) :- reverse([X:Y|L], Res), length(Res, Len).
g_one(X:Y, Len, L, Res) :- g_move(X:Y, Xn:Yn), \+ memberchk(Xn:Yn, L), g_one(Xn:Yn, Len, [X:Y|L], Res).


g_all(R, Len) :- g_size(S), between(0,S,L1), between(0,S,L2), g_one(L1:L2, Len, [], R).

g_allLength(R) :- g_size(S), between(0,S,Len), between(0,S,L1), between(0,S,L2), g_one(L1:L2, Len, [], R).
g_allLength(R, Len) :- g_size(S), between(0,S,L1), between(0,S,L2), g_one(L1:L2, Len, [], R).
