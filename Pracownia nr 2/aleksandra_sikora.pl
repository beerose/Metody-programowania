% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(aleksandra_sikora, [resolve/4, prove/2]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Szukanie rezolwenty.

resolve(_,_,[],[]):-!.
resolve(X,X,~X,[]):-!.
resolve(X,C1,C2,R) :- 
	clauseToSet(C1,C11), 
	clauseToSet(C2, C22), 
	member(X,C11), 
	member(~X,C22), 
	delete(C11,X,C111), 
	delete(C22,~X,C222),
	append(C222,C111,R1), 
	listToSet(R1,R2),
	listToClause(R2,R),!.




%predykat sprawdzajacy czy X jest literales
literal(X) :- atom(X).
literal(~ X) :- atom(X). 

%predykat zwracajacy pierwszy literal z alternatywy
getFirstLiteral(C v _,C) :- literal(C).
getFirstLiteral(C v _,C) :- literal(C).

%predykat zwracajacy pierwszy alternatywe bez pierwszego elementu
getClauseTail(C v D, D) :- literal(C).

%predykat zamieniajacy klauzule na liste literalow
clauseToList(X,[X]) :- literal(X), !.
clauseToList(X,[H|T]) :- 
	getFirstLiteral(X,H), 
	getClauseTail(X,Tail), 
	clauseToList(Tail,T). 

%zamiana klauzuli na zbior literalow
clauseToSet(X,Y) :- 
	clauseToList(X,Y1),
	listToSet(Y1,Y),!.

%zamiana listy na zbior
listToSet([] , []).
listToSet([E|Es] , Set ) :-
   literal(E),
   member(E, Es),
   listToSet(Es , Set),!.
listToSet([E|Es] , [E|Set] ) :-
   maplist(dif(E), Es),
   listToSet(Es , Set).


%predykat zamieniajacy zbior klauzul na liste list
clausesLists([X],[Y]) :- clauseToSet(X,Y), !.
clausesLists([H|T],[H3|T3]) :- 
	delete([H|T],[],[H1|T1]),
	delete([H1|T1],C v ~C,[H2|T2]),
	clauseToSet(H2,H3), 
	clausesLists(T2,T3),!.


%predykat zamieniajacy liste na klauzule
listToClause([],[]).
listToClause([X], X).
listToClause([X,Y], X v Y):-!.

listToClause([H,G|T], Clause ) :- 
	Clause = H v G v Clause1,  
	listToClause(T, Clause1),!.


%negowanie 
neq(~X,X) :- !.
neq(X,~X).


%predykat find znajduje w liscie list (Lista) taki atom X, ktory wystepuje w liscie Pos, 
%a jego zaprzeczenie wystepuje w liscie Neg
%predykaty pomocnicze: findpos, findneq
%predsort sortuje liste list w kolejnosci rosnacej wzgledem dlugosci list
find(X, Lista, [Pos,Neq]) :-
	predsort(order,Lista,L),
	findpos(X,L,Pos),
	neq(X,X1),
	delete(L,Pos,L1),
	findpos(X1, L1, Neq).

findpos(H, [H1|T1], H1):-
	member(H,H1);
	findpos(H,T1,H1).
findpos(H, [_|T1], Res) :- 
	findpos(H, T1, Res).

findneq(H, [H1|_], H1):-
	member(H,H1),!.
findneq(H, [_|T1], Res) :- 
	findneq(H, T1, Res).

order(<, L1, L2) :-
	length(L1, X),
	length(L2, Y),
	X < Y.
order(>, L1, L2) :-
	length(L1, X),
	length(L2, Y),
	X >= Y.



%predykat getNumber znajduje w liscie z ponumerowanymi listami numer danej listy
getNumber(X, [(X, V)|_], V) :- !.
getNumber(X, [_|T], V) :- getNumber(X, T, V),!.


%addTail dodaje element na koniec listy
addTail([],X,[X]).
addTail([H|T],X,[H|L]):-addTail(T,X,L).



%predykat solve(Lista+, X-, Pos-, Neq-, Y-, Res-) dla danej listy Lista, znajduje atom X, 
%ktory wystepuje w liscie Pos 
%i zanegowany w liscie Neq, wykonuje rezolucje i zwraca rezolwente oraz liste Y bedaca: 
%Y = Res U Lista\{Pos,Neq}

solve(Lista, X, Pos, Neq, Y, Res) :- 
	find(X, Lista, [Pos,Neq]),
	resolv(X,Pos,Neq,Res),
	delete(Lista, Pos, Lista1),
	delete(Lista1, Neq, Lista2),
	append([Res],Lista2,Y).


%predykat zwraca rezolwente dwoch klauzul w postaci list literalow
resolv(X,[X],[~X],[]):-!.
resolv(X,C11,C22,R) :- 
	member(X,C11), 
	member(~X,C22), 
	delete(C11,X,C111), 
	delete(C22,~X,C222),
	append(C222,C111,R1), 
	listToSet(R1,R),!.


% addNumber przypisuje numer kazdemu elementowi listy
addNumber([X],[(X,Number)],Number).
addNumber([H|T], [(H,Number)|T1], Number) :- N1 is Number +1, addNumber(T,T1,N1). 



%glowny predykat; dla danej listy w postaci listy list literalow zwraca 
%rezolucyjny dowod sprzecznosci



preprove(Lista, Wynik) :- 
	preprove1(Lista, Wynik, 1, [], Lista, Lista).


preprove1([[]|_], Acc, _, Acc, _, _) :-!.
preprove1(Lista, Wynik, Number, Acc, ListaA, Lista1):-
	addNumber(Lista1, ListaNum, Number),
	solve(Lista, X, Pos ,Neq , Y, Res),
	listToClause(Res, Res1),
	getNumber(Pos, ListaNum, B),
	getNumber(Neq, ListaNum, A),
	W = [(Res1,(X,A,B))],
	ifAxiom(Pos,Neq,Lista,Output),
	append(Output,W,W2),
	reverse(W2,W3),
	append(W3,Acc,A1),
	addTail( Lista1,Res, ListaNew),
	preprove1(Y, Wynik, Number , A1, ListaA, ListaNew).

%ifAxiom sprawdza czy dana klauzula jest axiomem
ifAxiom(Pos,Neq,Lista,Output):-
	member(Pos,Lista),
	member(Neq,Lista),
	listToClause(Neq, Neq1),
	listToClause(Pos, Pos1),
	Output = [(Pos1,axiom),(Neq1,axiom)],!.
ifAxiom(Pos,_,Lista,Output):-
	member(Pos,Lista),
	listToClause(Pos, Pos1),
	Output = [(Pos1,axiom)],!.
ifAxiom(_,Neq,Lista,Output):-
	member(Neq,Lista),
	listToClause(Neq, Neq1),
	Output = [(Neq1,axiom)],!.
ifAxiom(_,_,_,[]):-!.



prove([],[]).
prove([[]],[]).
prove(Clauses, Proof) :-
	clausesLists(Clauses, Lists),
	preprove(Lists, Pr), 
	reverse(Pr, Proof),!.

