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
% R = L2 - Result is L2
spoj([],L2,L2).
%spoj(List,[1,2],[333,4]).
spoj(L2,[],L2).
spoj(L2,L2,[]).

spoj([H|R],[H|L1],L2) :- spoj(R,L1,L2).
spoj([H|L1],L2,[H|R]) :- spoj(L1,L2,R).



%spoj(List,[1,2],[333,4]).
%spoj([H|L1],[H|C],L2) :- spoj(L1,C,L2).
%spoj([H|C],[H|L1],L2) :- spoj(C,L1,L2)!.
%spoj(X,[H|Y],[H|Z]) :- spoj(X,Y,Z).
%spoj([H|X],[H|Z],Y) :- spoj(Z,Y,X).

obrat([],[]).
% reverzace tail a konkatenace s head
obrat([H|T], Res) :- obrat(T,ResT), spoj(ResT,[H],Res).

% Vytvoˇrte predikat´ sluc/3, ktery slou ´ cˇ´ı dva seˇrazene seznamy do ´
%seznamu tˇret´ıho.
%Porovnan´ ´ı:
%• aritmeticke: ´ <, =<, >, >=
%• pro jakekoliv atomy: ´ @<, @=<, @>, @>=
sluc(L, [], L).
sluc([], L, L).
sluc([X|XS], [Y|YS], [X|T]) :- porovnej([X|XS]),
                               porovnej([Y|YS]),
                               spoj([X|XS],[Y|YS],[X|T]).
sluc([X|XS], [Y|YS], [Y|T]) :- porovnej([X|XS]),
                               porovnej([Y|YS]),
                               spoj([X|XS],[Y|YS],[Y|T]).
porovnej([]).
porovnej([_]).
porovnej([R1,R2|RS]) :- R1 < R2, porovnej([R2|RS]).
%sluc(  ,  ,  ) :- ...

serad([], []).
serad([H|T], SL) :- sluc(H,T,SL).




plus(X,Y,Z) :- Z is X + Y.
