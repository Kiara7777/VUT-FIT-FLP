/** FLP 2017/2018
Pojekt: Rubikova kostka
Autor: Sara Skutova, xskuto00@stud.fit.vutbr.cz

preklad: swipl -q -g start -o flp18-log -c kostka.pl
*/

:- use_module(library(lists)).

%Reads line from stdin, terminates on LF or EOF.
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).

%Tests if character is EOF or LF.
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).

read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).



% rozdeli radek na podseznamy
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1

% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).

%prevede seznam seznamu jenom na jeden seznam, at se s tim dopre pracuje
%kazde jedno policko je jedna promenna
mapElems([
		[[X1,X2,X3]],
		[[X4,X5,X6]],
		[[X7,X8,X9]],
		[[X10,X11,X12],[X13,X14,X15],[X16,X17,X18],[X19,X20,X21]],
		[[X22,X23,X24],[X25,X26,X27],[X28,X29,X30],[X31,X32,X33]],
		[[X34,X35,X36],[X37,X38,X39],[X40,X41,X42],[X43,X44,X45]],
		[[X46,X47,X48]],
		[[X49,X50,X51]],
		[[X52,X53,X54]]
],[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,
		X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,
		X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,
		X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,
		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
]).


elemsToList([
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,
		X10,X11,X12,X13,X14,X15,X16,X17,X18,X19,X20,X21,
		X22,X23,X24,X25,X26,X27,X28,X29,X30,X31,X32,X33,
		X34,X35,X36,X37,X38,X39,X40,X41,X42,X43,X44,X45,
		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
	[X1,X2,X3],
	[X4,X5,X6],
	[X7,X8,X9],
	[X10,X11,X12,' ',X13,X14,X15,' ',X16,X17,X18,' ',X19,X20,X21],
	[X22,X23,X24,' ',X25,X26,X27,' ',X28,X29,X30,' ',X31,X32,X33],
	[X34,X35,X36,' ',X37,X38,X39,' ',X40,X41,X42,' ',X43,X44,X45],
	[X46,X47,X48],
	[X49,X50,X51],
	[X52,X53,X54]
]
).


%pomoc pro vypis kostky, vypisuje prijaty seznam
myWrite([]).
myWrite([H|T]) :-
	write(H),
	myWrite(T).


%vypis konstky
printCube([]).
printCube([H|T]) :-
		myWrite(H), nl,
		printCube(T).
		
		
myPrint(L) :- elemsToList(L, NL), printCube(NL), nl, nl.

% B
% Z C M O
% Z

%https://www.youcandothecube.com/solve-it/3-x-3-solution

%pretoceni R, mozna udealt i Ri, ale ono to defakto to samo, jenom na druhou stranu
otocR(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X1,X2,X12,
		X4,X5,X24,
		X7,X8,X36,

		X10,X11,X48,  X37,X25,X13,  X9,X17,X18,  X19,X20,X21,
		X22,X23,X51,  X38,X26,X14,  X6,X29,X30,  X31,X32,X33,
		X34,X35,X54,  X39,X27,X15,  X3,X41,X42,  X43,X44,X45,

		X46,X47,X40,
		X49,X50,X28,
		X52,X53,X16
]
).

%pretoceni Ri
otocRi(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X1,X2,X40,
		X4,X5,X28,
		X7,X8,X16,

		X10,X11,X3,  X15,X27,X39,  X54,X17,X18,  X19,X20,X21,
		X22,X23,X6,  X14,X26,X38,  X51,X29,X30,  X31,X32,X33,
		X34,X35,X9,  X13,X25,X37,  X48,X41,X42,  X43,X44,X45,

		X46,X47,X12,
		X49,X50,X24,
		X52,X53,X36
]
).

%pretoceni L
otocL(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X42,X2,X3,
		X30,X5,X6,
		X18,X8,X9,

		X1,X11,X12,  X13,X14,X15,  X16,X17,X52,  X43,X31,X19,
		X4,X23,X24,  X25,X26,X27,  X28,X29,X49,  X44,X32,X20,
		X7,X35,X36,  X37,X38,X39,  X40,X41,X46,  X45,X33,X21,

		X10,X47,X48,
		X22,X50,X51,
		X34,X53,X54
]
).

%pretoceni Li
otocLi(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X10,X2,X3,
		X22,X5,X6,
		X34,X8,X9,

		X46,X11,X12,  X13,X14,X15,  X16,X17,X7,  X21,X33,X45,
		X49,X23,X24,  X25,X26,X27,  X28,X29,X4,  X20,X32,X44,
		X52,X35,X36,  X37,X38,X39,  X40,X41,X1,  X19,X31,X43,

		X42,X47,X48,
		X30,X50,X51,
		X18,X53,X54
]
).


%pretoceni U
otocU(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X7,X4,X1,
		X8,X5,X2,
		X9,X6,X3,

		X13,X14,X15,  X16,X17,X18,  X19,X20,X21,  X10,X11,X12,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
]
).

%pretoceni Ui
otocUi(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X3,X6,X9,
		X2,X5,X8,
		X1,X4,X7,

		X19,X20,X21,  X10,X11,X12,  X13,X14,X15,  X16,X17,X18,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
]
).

%pretoceni D
otocD(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X43,X44,X45,  X34,X35,X36,  X37,X38,X39,  X40,X41,X42,

		X52,X49,X46,
		X53,X50,X47,
		X54,X51,X48
]
).

%pretoceni Di
otocDi(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X37,X38,X39,  X40,X41,X42,  X43,X44,X45,  X34,X35,X36,

		X48,X51,X54,
		X47,X50,X53,
		X46,X49,X52
]
).


%pretoceni F
otocF(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X1,X2,X3,
		X4,X5,X6,
		X45,X33,X21,

		X34,X22,X10,  X7,X14,X15,  X16,X17,X18,  X19,X20,X46,
		X35,X23,X11,  X8,X26,X27,  X28,X29,X30,  X31,X32,X47,
		X36,X24,X12,  X9,X38,X39,  X40,X41,X42,  X43,X44,X48,

		X37,X25,X13,
		X49,X50,X51,
		X52,X53,X54
]
).


%pretoceni Fi
otocFi(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X1,X2,X3,
		X4,X5,X6,
		X13,X25,X37,

		X12,X24,X36,  X48,X14,X15,  X16,X17,X18,  X19,X20,X9,
		X11,X23,X35,  X47,X26,X27,  X28,X29,X30,  X31,X32,X8,
		X10,X22,X34,  X46,X38,X39,  X40,X41,X42,  X43,X44,X7,

		X21,X33,X45,
		X49,X50,X51,
		X52,X53,X54
]
).


%pretoceni B
otocB(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X15,X27,X39,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X54,  X40,X28,X16,  X3,X20,X21,
		X22,X23,X24,  X25,X26,X53,  X41,X29,X17,  X2,X32,X33,
		X34,X35,X36,  X37,X38,X52,  X42,X30,X18,  X1,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X19,X31,X43
]
).

%pretoceni Bi
otocBi(
[
		X1,X2,X3,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X15,  X16,X17,X18,  X19,X20,X21,
		X22,X23,X24,  X25,X26,X27,  X28,X29,X30,  X31,X32,X33,
		X34,X35,X36,  X37,X38,X39,  X40,X41,X42,  X43,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X52,X53,X54
],
[
		X43,X31,X19,
		X4,X5,X6,
		X7,X8,X9,

		X10,X11,X12,  X13,X14,X1,  X18,X30,X42,  X52,X20,X21,
		X22,X23,X24,  X25,X26,X2,  X17,X29,X41,  X53,X32,X33,
		X34,X35,X36,  X37,X38,X3,  X16,X28,X40,  X54,X44,X45,

		X46,X47,X48,
		X49,X50,X51,
		X39,X27,X15
]
).


cil(
[
		5,5,5,
		5,5,5,
		5,5,5,

		1,1,1,  2,2,2,  3,3,3,  4,4,4,
		1,1,1,  2,2,2,  3,3,3,  4,4,4,
		1,1,1,  2,2,2,  3,3,3,  4,4,4,

		6,6,6,
		6,6,6,
		6,6,6
]).

/** NEDOKONCENO IDS+DLS, problem u member, z nejakeho duvodu to nechce uznat za clena
https://www.fit.vutbr.cz/study/courses/IZU/private/prolog-priklady.zip
vyuziti algortimu DFS, tery je skoro totozny z DLS

dls(Start, Goal, Depth) :- path([[Start]], [Goal], Depth).

path([],_,_) :- !.		%zasobnik se prazdny, NEUSPECH

path(Open, Goal,_) :-	%vyberu vrchol zasobniku - JE TO CIL, USPECH,
		pop([State|Ancestors], Open, _),
		member(State, Goal),	
		write('Nasel'),!.

path(Open, Goal, Depth) :-
		write('Jsem tu'), nl,
		Depth > 0,
		pop([State|Ancestors], Open, Rest_open),
		get_cubes(State, Ancestors, Rest_open, Children),
		append(Children,Rest_open,New_open),
		D1 is Depth - 1,
		path(New_open,Goal, D1).

pop(Top,[Top|Stack],Stack).

get_cubes(State, Ancestors, Rest_open, Children) :-
		bagof(Child,moves(State,Ancestors,Rest_open,Child),Children).
get_cubes(_,_,_,[]).

moves(State,Ancestors,Rest_open,[Next,State|Ancestors]) :-
		otoc(State, Next),
		not(member(Next, Ancestors)),
		not(member(Next, Rest_open)).
		

ids(L, G, D) :- dls(L, G, D).
ids(L, G, D) :- 
		D1 is D + 1,
		ids(L, G, D1).
*/		
		

start :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		mapElems(S, L),
		
		%original
		myPrint(L),
		
		%kostkaZadani
		%otocR(L, R),
		%myPrint(R),
		%otocUi(R, UI),
		%myPrint(UI),
		
		%kostka1
		otocD(L, D),
		myPrint(D),
		otocFi(D, Fi),
		myPrint(Fi),
		otocL(Fi, NL),
		myPrint(NL),

		%kostka2
		otocD(L, D1),
		myPrint(D1),
		otocLi(D1, L1),
		myPrint(L1),
		otocB(L1, B),
		myPrint(B),
		otocUi(B,U1),
		myPrint(U1),
		otocR(U1, R1),
		myPrint(R1),
		otocFi(R1, FF),
		myPrint(FF),
		
		
		
		

		
		
		
		
		






		halt.
