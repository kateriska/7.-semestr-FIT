
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
pohni(I, Operace) :- call(Operace, I).

armageddon :- obsazene_roboty(X), forall(member(P, X), vybuch(P)).
vybuch(P) :- odstran(P), vytvor(P).









g_size(3).

/*
g_test(X:Y) :-
*/

/*
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 - 1, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 0, Y2 is Y1 + 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 - 1, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 0, g_test(X2:Y2).
g_move(X1:Y1, X2:Y2) :- X2 is X1 + 1, Y2 is Y1 + 1, g_test(X2:Y2).
*/

/*
g_one(X:Y, Len, L, R) :-
g_one(X:Y, Len, L, R) :-
*/

/*
g_all(R, Len) :-
g_all(R, Len) :-
*/

/*
g_allLength(R) :-
g_allLength(R, Len) :-
g_allLength(R, Len) :-
*/
