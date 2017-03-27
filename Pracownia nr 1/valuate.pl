:- module(aleksandra_sikora, [solve/2]).


:- op(200, fx, ~).
:- op(500, xfy, v).


%predykat, sprawdzajacy czy X jest literalem
literal(X) :- atom(X).
literal(~ X) :- atom(X). 

%predykat zwracajacy pierwszy literal z alternatywy
getFirstLiteral(C v _,C) :- literal(C).
getFirstLiteral(C v _,C) :- literal(C).

%predykat zwracajacy pierwszy alternatywe bez pierwszego elementu
getClauseTail(C v D, D) :- literal(C).


%zamiana klauzuli na liste literalow
clauseToList(X,[X]) :- literal(X), !.
clauseToList(X,[H|T]) :- 
	getFirstLiteral(X,H), 
	getClauseTail(X,Tail), 
	clauseToList(Tail,T). 

%zamiana klauzuli na zbior literalow
clauseToSet(X,Y) :- 
	clauseToList(X,Y1),
	listToSet(Y1,Y).

%zamiana listy na zbior
listToSet([] , []).
listToSet([E|Es] , Set ) :-
   atom(E),
   member(E, Es),
   listToSet(Es , Set),!.
listToSet([E|Es] , [E|Set] ) :-
   maplist(dif(E), Es),
   listToSet(Es , Set).

%zamiana zbioru klauzul na liste list klauzul, tzn dla [p v q, ~r] wynikiem bedzie [[p,q],[~r]] 
clausesLists([X],[Y]) :- clauseToSet(X,Y), !.
clausesLists([H|T],[H3|T3]) :- 
	delete([H|T],[],[H1|T1]),
	delete([H1|T1],C v ~C,[H2|T2]),
	clauseToSet(H2,H3), 
	clausesLists(T2,T3),!.


% V jest wartociowaniem zmiennej X w liscie podanej jako drugi argument predykatu
value(~ X, [(X, Val)|_], V) :- Val \= t, V = t,!.
value(~ X, [(X, Val)|_], V) :- Val \= f, V = f,!.
value(~ X, [_|T], V) :- value(~X, T, V),!.

value(X, [(X, V)|_], V) :- !.
value(X, [_|T], V) :- value(X, T, V),!.



% predykat nadajacy wartosciowania literalow
valuate([],[]).
valuate([H|T], [H1|T1]) :- (H1 = (H,t); H1 = (H,f)), valuate(T,T1).


% predykat zwracajacy zbior literalow, bez powtorzen, bez negacji, tzn dla [[p,~q], [q]] zwroci [p,q]
listOfAtoms([],[]).
listOfAtoms([H|T], Set) :- 
	setof(A1, (member(~ A1, H), atom(A1)), R1), 
	setof(A2, (member(A2 ,H), atom(A2)), R2),  
	listOfAtoms(T, N), 
	append(R2, N, S1), 
	append(S1, R1, S2), 
	setof(X ,member(X, S2),Set), !.
listOfAtoms([H|T], Set) :- 
	setof(A, (member(A,H), atom(A)), R),  
	listOfAtoms(T, N), append(R,N,S), 
	setof(X,member(X,S),Set), !.
listOfAtoms([H|T], Set) :- 
	setof(A, (member(~ A,H), atom(A)), R),  
	listOfAtoms(T, N), 
	append(R,N,S), 
	setof(X,member(X,S),Set), !.


% zwraca takia liste wartosciowan Val, zeby po ich zaplikowaniu do literalow z klauzul z [H|T] wartosciowanie calosci bylo zgodne z V
% np.: is_true([[p,q],[r]],Y,V). zwroci Y = [(p, t),  (q, t),  (r, t)], V = t 

isTrue([H|T], Val, V) :- 
	listOfAtoms([H|T],Set), 
	valuate(Set, Val), 
	isT([H|T], Val, V).
isT([], _, t).
% zbior klazul jest spelniony, gdy wszystkie klauzule są spelnione
isT([H|T], Val, t) :- 
	isTrueA(H, Val, t), 
	isT(T, Val, t),!.
% zbior klauzul nie jest spelniony, gdy ktorakolwiek z klauzul nie jest
isT([H|T], Val, f) :- 
	isTrueA(H, Val, f); 
	isT(T, Val, f),!.


% predykat is_true_a przyjmuje jako pierwszy argument pojedyncza klauzule i sprawdza czy jest spelniona 
% przy wartosciowaniach z listy Val

% klauzula jest spelniona gdy pierwszy element alternatywy ma wartosciowanie true lub ktorys kolejny
isTrueA([H|T], Val, t) :- 
	value(H,Val,t); 
	isTrueA(T, Val, t),!.
% klazula nie jest spelniona gdy wszystkie literaly maja wartosciowanie false
isTrueA([H|T], Vs, f) :- 
	value(H,Vs,f), 	
	isTrueA(T,Vs,f),!.


% predykat preSolve dla danej listy list klauzul zwraca takie możliwe wartosciowania, dla ktorych zbior klauzul jest spelniony
preSolve(Clauses, Solution) :- isTrue(Clauses, Solution, t).

% predykat solve: zamienia zbior klauzul na liste list; wyznacza zbior z wygenerowanych mozliwych wartosciowan
solve([],[]):-!.
solve(Clauses, Solution) :- 
	clausesLists(Clauses,Lists), 
	setof(V, preSolve(Lists, V), Val), 
	select(Solution, Val, _).


