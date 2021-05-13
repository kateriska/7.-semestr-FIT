%/6b/ Flatten seznamu - vytvorit predikat e, ktery bere 2 argumenty. Prvni je seznam libovolne zanorenych seznamu (i prazdnych), napr. [[], [1, 2], [[[[]]],[atom, atom]]]. Druhy argument je vysledny seznam bez zanoreni.
%/7b/ Funkce XOR, ktera vraci symterickou diferenci dvou mnozin (sjednoceni mnozin bez jejich pruseciku). Bere prvni a druhy parametr mnozinu reprezentovanou seznamem, treti parametr je vysledna mnozina reprezentovana seznamem.

e([],[]).
e([[]|R],Res) :- e(R,Res), !.
e([[X|XS]|YS],Res) :- e([X,XS|YS],Res), !.
e([V|XS],[V|Res]) :- e(XS,Res).

xor(L,R,Res) :- sub(L,R,L1), sub(R,L,R1), app(L1,R1,Res).

sub([],_,[]).
sub([X|XS],YS,RS) :- elem(X,YS), !, sub(XS,YS,RS).
sub([X|XS],YS,[X|RS]) :- sub(XS,YS,RS).

elem(X,[X|_]) :- !.
elem(X,[_|XS]) :- elem(X,XS).

app([],L,L).
app([X|XS],L,[X|RS]) :- app(XS,L,RS).


%Holý Prolog. Napsat predikát mkTrans(ListOfLists,ListOfLists), která dostane v 1. argumentu matici, kterou transponovanou unifikuje do 2. argumentu.
%Holý Prolog. Napsat predikát subseq, který v 1. argumentu dostane seznam a do 2. argumentu unifikuje seznam všech jeho podseznamů, tedy jde tam o prefix a suffix matching.

pos/1.
search(P, Res) :- setof(Path,s(P,P,0,Path),Res).

s(P,P,N,[P]) :- N =< 22, N >= 20, !.
s(P,P,N,[P]) :- N > 22, !, fail.

s(P,P,N,[P|R]):- assertz(pos(P)), NN is N + 1, nextStep(P, P2), s(P2,P,NN,R).


nextStep(pos(X,Y),pos(XX,Y)) :- XX is X + 1.
nextStep(pos(X,Y),pos(XX,Y)) :- XX is X - 1.
nextStep(pos(X,Y),pos(X,YY)) :- YY is Y + 1.
nextStep(pos(X,Y),pos(X,YY)) :- XX is Y + 1.


searchShortestPath(Start, End, Shortest) :- possiblePath(Start, End, 0, Paths).

possiblePath(Start, End, Cost, [Start|Paths]) :-
  assertz(pos(Start)),
  nextStep(Start,NewStart),
  NewCost is Cost + 1,
  possiblePath(NewStart, End, Paths).

gcd(N,N,N) :- !.
gcd(N,M,M) :-
    N > M,
    NN is mod(N,M),
    NN==0, !.
gcd(N,M,D) :-
    N > M, !,
    NN is mod(N,M),
    gcd(M,NN,D).
gcd(N,M,N) :-
    M > N,
    MM is mod(M,N),
    MM is 0, !.
gcd(N,M,D) :-
    MM is mod(M,N),
    gcd(N,MM,D).

ev(op('+',L,R),rac(NN,DD)) :-
    ev(L,rac(LN,LD)),
    ev(R,rac(RN,RD)),
    N1 is LN*RD + RN*LD,
    D1 is LD*RD,
    norm(N1,D1,NN,DD).
ev(op('*',L,R),rac(NN,DD)) :-
    ev(L,rac(LN,LD)),
    ev(R,rac(RN,RD)),
    N1 is LN*RN,
    D1 is LD*RD,
    norm(N1,D1,NN,DD).
ev(rac(X,Y),rac(X,Y)).

norm(N,D,NN,DD) :-
    gcd(N,D,G), G>1,!,
    NN is div(N,G),
    DD is div(D,G).
norm(N,D,N,D).

merge([],L,L).
merge(L,[], L).
merge([H1|T1], [H2|T2],[H1|TT]) :-
  H1 =< H2,
  merge(T1,[H2|T2],TT).
merge([H1|T1], [H2|T2], [H2|TT]) :-
  H1 > H2,
  merge([H1|T1],T2,TT).

mergesort([],[]).
mergesort([V],[V]).
mergesort([A,B|List],Result) :-
  divide(List, Half1, Half2),
  mergesort([A|Half1], Result1),
  mergesort([B|Half2],Result2),
  merge(Result1,Result2,Result).


divide([],[],[]).
divide([V],[V],[]).
divide([A,B|T],[A|X],[B|Y]) :- divide(T,X,Y).


er([],[]).
er([[H1|T1]|T2], R) :- er([H1,T1|T2],R).
er([[]|T],R) :- er(T,R).
er([H|T],[H|R]) :- er(T,R).

list_concat([],L,L).
list_concat([X1|L1],L2,[X1|L3]) :- list_concat(L1,L2,L3).

list_delete(X, [X], []).
list_delete(X,[X|L1], L1).
list_delete(X, [Y|L2], [Y|L1]) :- list_delete(X,L2,L1).

% sub - je v prvni, neni v druhe
sub2([],_,[]).
% je v druhe - neni v vysledku
sub2([H|XS],YS,Res) :-elem2(H,YS), sub2(XS,YS,Res).
% neni v druhe - je ve vysledku - pridame do nej
sub2([H|XS],YS,[H|Res]) :- sub2(XS,YS,Res).

elem2(V,[V|T]) :- !.
elem2(V,[_|T]) :- elem2(V,T).

merge2([],[],[]).
merge2(L,[],L).
merge2([],L,L).
merge2([A|Half1],[B|Half2], [A|Res]) :-
  A =< B,
  merge2(Half1, [B|Half2], Res).

merge2([A|Half1],[B|Half2],[B|Res]) :-
  A > B,
  merge2([A|Half1],Half2,Res).




mergesort2([],[]).
mergesort2(List, Result) :-
  divide2(List, Half1, Half2),
  mergesort2(Half1,Result1),
  mergesort2(Half2,Result2).

divide2([],[],[]).
divide2([A,B|XS],[A|Half1],[B|Half2]) :-
  divide2(XS,Half1,Half2).


trp([[]|_],[]) :- !.
trp(XSS,[L|LS]) :- heads(XSS,L,YSS), trp(YSS,LS).

heads([[H|TS]|XSS],[H|T],[TS|YSS]) :- heads(XSS,T,YSS).
heads([],[],[]).

mkTrans([],[]).
mkTrans([[]|_],[]).
mkTrans(LS,[HS|HHS]) :-
    trans(LS,HS,TS),
    mkTrans(TS,HHS).

trans([],[],[]).
trans([[H|T]|LS],[H|HS],[T|TS]) :-
    trans(LS,HS,TS).



transpose_col([],[],[]).
transpose_col([[H|T]|Rows],[H|Row],[T|Ts]) :- transpose_col(Rows,Row, Ts).

blocks([],[]).
blocks([[],[],[],[]|Rest],Res) :- blocks(Rest,Res).
blocks([[A|XS],[B|YS],[C|ZS],[D|QS]|Rest], [[A,B,C,D]|MS]) :- blocks([XS,YS,ZS,QS|Rest],MS).
