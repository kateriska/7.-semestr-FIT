% FLP CVICENI 4 - PROLOG 1 - UVOD

% ukazka predikatu pro vypocet funkce faktorial
factorial( 0, 1 ).
factorial( N, Value ) :-
     N > 0,
     Prev is N - 1,
     factorial( Prev, Prevfact ),
     Value is Prevfact * N.

% databaze rodinnych vztahu
muz(jan).
muz(pavel).
muz(robert).
muz(tomas).
muz(petr).

zena(marie).
zena(jana).
zena(linda).
zena(eva).

otec(tomas,jan).
otec(jan,robert).
otec(jan,jana).
otec(pavel,linda).
otec(pavel,eva).

matka(marie,robert).
matka(linda,jana).
matka(eva,petr).

% Implementujte nasledujici predikaty:

rodic(R,X) :- otec(R,X) ; matka(R,X).
% Stejni rodice a sourozenci si nejsou rovni
sourozenec(X,Y) :- rodic(R,X), rodic(R,Y) , X \= Y.
sestra(X,Y) :- sourozenec(X,Y), zena(X).
% otec rodice
deda(X,Y) :- rodic(R,Y), otec(X,R).
je_matka(X) :- matka(X,_).
% sestra rodice
teta(X,Y) :- rodic(R,Y), sestra(X,R).

% Nehomogenni Seznamy:
neprazdny([_|_]) :- true.
hlavicka([H|_], H).
posledni([H], H) :- !.
posledni([_|T], Res) :- posledni(T, Res).


% Dalsi ukoly:
spoj([],L2,L2).
spoj(L2,[],L2).
spoj(L2,L2,[]).

spoj([H|R],[H|L1],L2) :- spoj(R,L1,L2),!.
spoj([H|L1],L2,[H|R]) :- spoj(L1,L2,R),!.

obrat([],[]).
% reverzace tail a konkatenace s head
obrat([H|T], Res) :- obrat(T,ResT), spoj(ResT,[H],Res).

% Vytvorte predikat sluc/3, ktery sloucı dva serazene seznamy do
%seznamu tretıho.
%Porovnanı:
%• aritmeticke: ´ <, =<, >, >=
%• pro jakekoliv atomy: ´ @<, @=<, @>, @>=
sluc([],[],[]).
sluc(L,[],L).
sluc([],L,L).
sluc([X|XS],[Y|YS],[X|T]) :- X =< Y, !, sluc(XS,[Y|YS],T).
sluc([X|XS],[Y|YS],[Y|T]) :- sluc(YS,[X|XS],T).

serad([], []).
serad([H|T], SL) :- serad(T, Z) , sluc([H],Z,SL).

% mezera ma oddelovac ' '
split([], [[]]).
split([' '|T], [[]|R1]) :- split(T, R1).
split([H|T], [[H|R1]|R2]) :- H \= ' ', split(T, [R1|R2]).

plus(X,Y,Z) :- Z is X + Y.

zipWith(_,[],[],[]).
zipWith(_,[],_,[]).
zipWith(_,_,[],[]).
zipWith(P,[X|T],[Y|T1],[Z|T2]):- call(P,X,Y,Z), zipWith(P,T,T1,T2).
