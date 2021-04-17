/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).

split_lines([],[]).
split_lines([[Vertex1, ' ', Vertex2]|Ls],[H|T]) :- sort([Vertex1, Vertex2], H), split_lines(Ls, T).



start :-
		prompt(_, ''),
		read_lines(LL),
    split_lines(LL,SplittedLines),
    sort(SplittedLines, SortedSplittedLines),
    write(SortedSplittedLines),
		halt.
