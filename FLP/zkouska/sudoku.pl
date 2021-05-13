elem(E,[E|T]) :- !.
elem(E,[_|T]) :- elem(E,T).

% remove([4,5,1,6,3], [1,4,7], L).
remove([],_,[]).
remove([E|Elements],L,Res) :- elem(E,L), !, remove(Elements,L,Res).
remove([E|Elements],L,[E|Res]) :- remove(Elements,L,Res).

% [[a,b,c,j],[d,e,f,y],[g,h,i,m],[l,p,k,x]]
transpose([],[]).
transpose([[]|_],[]).
transpose(Matrix,[Row|Rows]) :- transpose_col(Matrix,Row,RestMatrix), transpose(RestMatrix,Rows).

% bere se head z daneho radku
transpose_col([],[],[]).
transpose_col([[H|T]|Rows], [H|Row], [T|RestMatrix]) :- transpose_col(Rows,Row,RestMatrix).

blocks([],[]).
blocks([[],[],[]|Rest], RestResult) :- blocks(Rest, RestResult).
blocks([[A,B,C|XS],[D,E,F|YS],[G,H,I|ZS]|Rest], [[A,B,C,D,E,F,G,H,I]|RestResult]) :- blocks([XS,YS,ZS|Rest], RestResult).

/* co bylo k dispozici jako hotove, pro uplnost */
newRC(R,C,R,CC) :-
    C<9, !, CC is C+1.
newRC(R,9,RR,1) :-
    R<9, RR is R+1.

getIth(1,[X|_],X) :- !.
getIth(N,[_|XS],X) :- N1 is N-1, getIth(N1,XS,X).

getRC(R,C,M,V) :-
    getIth(R,M,L),
    getIth(C,L,V).


validValues(X,Y,Matrix,Res) :-
  getIth(X,M,L),
  remove([1,2,3,4,5,6,7,8,9],L,ForRow),
  transpose(M,TransMatrix),
  getIth(Y,TransMatrix,L2),
  remove(ForRow,L2,ForColumn).
