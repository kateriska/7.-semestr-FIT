%/9b/ Napisat predikat search(PocatecniPozice, SeznamCest), ktory najde vsechny cesty z dane pozice zpet do teto pozice,
%delky 20 az 22 kroku (netrapit se tim, jestli vracet prvni/posledni prvek ci ne). Kazdy prvok je mozne nastivit len jeden krat vyjma prveho (== posledneho).
%Definicia pozicie je neznama, napiste funkci nextStep(Pos, NewPos) nad neznamym a NEKONECNYM stavovym priestorom. Mozno pouzit retract*, assert*, bagof, setof, length.

:- dynamic pos/1.

search(Start,End,Res) :- findall(Path, searchOnePath(Start,End,0,Path), Res).
searchOnePath(Start,End,Length,[Start]) :- Length > 6, !.
searchOnePath(Start,End,Length,[Start|Result]) :-
  assertz(pos(Start)),
  findPossiblePositions(Start, NewStart),
  NewLength is Length + 1,
  searchOnePath(NewStart,End,NewLength,Result).

searchOnePath(Start,End,Length,Result) :-
  retractall(pos(Start)).





findPossiblePositions(p(X,Y), p(XX,Y)) :- XX is X + 1.
findPossiblePositions(p(X,Y), p(XX,Y)) :- XX is X - 1.
findPossiblePositions(p(X,Y), p(X,YY)) :- YY is Y + 1.
findPossiblePositions(p(X,Y), p(X,YY)) :- YY is Y - 1.


/*
:- dynamic pos/2.

search(Start,End) :- findall(Path, searchOnePath(Start,1,0,Path), Res).

searchOnePath(Start,End,Steps,[Start]) :- Steps =< 6, Steps >= 4, !.
searchOnePath(Start,End,Steps,R) :- Steps > 6, fail, !.
searchOnePath(Start, End, Steps, [Start|R]) :-
  assertz(pos(Start, End)),
  nextStep(Start,End,NewStart,NewEnd),
  NewSteps is Steps + 1,
  searchOnePath(NewStart,NewEnd,NewSteps,R).

searchOnePath(Start,End,Steps,R):-
  retractall(pos(Start,End)).


nextStep(X1,Y1,X2,Y2) :- X2 is X1 - 1, Y2 is Y1 - 1.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 - 1, Y2 is Y1 + 0.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 - 1, Y2 is Y1 + 1.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 + 0, Y2 is Y1 - 1.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 + 0, Y2 is Y1 + 1.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 + 1, Y2 is Y1 - 1.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 + 1, Y2 is Y1 + 0.
nextStep(X1,Y1,X2,Y2) :- X2 is X1 + 1, Y2 is Y1 + 1.



search(P,Res) :-
    setof(Path,s(P,P,0,Path),Res).

s(P,P,N,[P]) :- N =< 22, N >= 20, !.
s(_,_,N,_) :- N > 22, !, fail.
s(P,P,N,_) :- N \= 0, !, fail.
s(A,P,N,[A|R]) :-
    assertz(pos(A)),
    NN is N+1,
    nextStep(A,AA),
    ( not(pos(AA)) ; AA=P ) ,
    s(AA,P,NN,R).
s(A,_,_,_) :-
    pos(A),
    retract(pos(A)),
    !,fail.

nextStep(p(X,Y),p(XX,Y)) :- XX is X+1.
nextStep(p(X,Y),p(XX,Y)) :- XX is X-1.
nextStep(p(X,Y),p(X,YY)) :- YY is Y+1.
nextStep(p(X,Y),p(X,YY)) :- YY is Y-1.

pos/1.
search(S,E,Res) :-
    retractall(pos(_)),
    steptry(S,E,0,Res).

steptry(S,E,N,Res) :-
    s(S,E,N,Res), !.
steptry(S,E,N,Res) :-
    NN is N+1,
    steptry(S,E,NN,Res).

s(E,E,0,[E]) :- !.
s(_,_,N,_) :- N < 0, !, fail.
s(A,E,N,[A|R]) :-
    assertz(pos(A)),
    NN is N-1,
    nextStep(A,AA),
    not(pos(AA)) ,
    s(AA,E,NN,R).
s(A,_,_,_) :-
    pos(A),
    retract(pos(A)),
    !,fail.

nextStep(p(X,Y),p(XX,Y)) :- XX is X+1.
nextStep(p(X,Y),p(XX,Y)) :- XX is X-1.
nextStep(p(X,Y),p(X,YY)) :- YY is Y+1.
nextStep(p(X,Y),p(X,YY)) :- YY is Y-1.
*/
