:- dynamic velikost/2, pozice/2.


prvek(H, [H|_]) :- !.
prvek(H, [_|T]) :- prvek(H, T).



rozdil([], _, []).
rozdil([H|T], S, R) :- prvek(H,S), rozdil(T, S, R).
rozdil([H|T], S, [H|P]) :- not(prvek(H,S)), rozdil(T,S,P).


sequence(0, []) :- !.
sequence(N, [N|T]) :- M is N - 1, sequence(M, T).


queens(Solution) :- queens(8, Solution).
queens(N, Solution) :- sequence(N, Seq), permutation(Seq, Solution), test(Solution).


%test(Solution).


test([]) :- !.
% pocatecni pozice v ktere chci testovat, aktualni pozici a vsechny damy na prave strane
test([H|T]) :- test(H,1,T).
test(_, _, []) :- !.
test(Pos, Dist, [H|T]) :- Pos \= H, Sub is H - Pos,  Pos \= Sub, Distn is Dist + 1, test(Pos, Distn, T).




cesty(XR, YR, XS, YS, XE, YE, N) :- XR > 0, YR > 1,
                                    assertz(velikost(XR, YR)),
                                    findall(P, cesta(XS,YS,XE,YE,P), Res),
                                    length(Res, N).




testPoz(X,Y) :- velikost(XR,YR),
                X > 0,
                X =< XR,
                Y > 0,
                Y =< YR.



skok(X,Y,XN,YN) :- XN is X + 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y + 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 2, YN is Y - 1, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X + 1, YN is Y - 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y + 2, testPoz(XN, YN).
skok(X,Y,XN,YN) :- XN is X - 1, YN is Y - 2, testPoz(XN, YN).



% pocatecni souradnice, koncove souradnice, vysledek co hledam (seznam sourdenic)
cesta(X,Y,X,Y,[X:Y]) :- !.
cesta(X,Y,XE,YE,[X:Y|T]) :- assertz(pozice(X,Y)), skok(X,Y, XN, YN), \+ pozice(XN, YN), cesta(XN,YN,XE,YE,T).


% nemuzu skocit - backtrack, ale nutnost odstraneni pozice pri backtrackingu, nemuzu jet dal, chci selhat
cesta(X, Y, _, _, _) :- retract(pozice(X,Y)), !, fail.









/*
% kontroly
slovnik(D, _, _) :-
slovnik(_, K, V) :-
% vyhledani hodnoty
slovnik(D, K, V) :-
% vyhledani klicu
slovnik(D, K, V) :-
% modifikace
slovnik(D, K, V) :-
% vlozeni
slovnik(D, K, V) :-
*/
