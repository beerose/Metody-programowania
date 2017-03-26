% Zadanie 1

lengthb(Lista,Length) :- length1(Lista,Length,0).

length1([],Acc,Acc).
length1([H|T], L, Acc) :- Acc1 is Acc +1, length1(T,L,Acc1),!.

% Zadanie 2

connection(a,b).
connection(b,c).
connection(c,d).
connection(d,b).
connection(d,e).

trip(X,Y,Res) :- tripp(X, Y, Res, [X]).

tripp(X, X, Res, Acc) :- reverse(Acc, Res).
tripp(X, Y, Res, Acc) :-  connection(X,New), \+ member(New, Acc), Acc1=[New|Acc], tripp(New, Y, Res, Acc1),!. 


% Zadanie 3


to_list([_]).
to_list([_|Y]):- to_list(Y).

fill([]).
fill([0|Xs]):- fill(Xs).
fill([1|Xs]):- fill(Xs).

bin([0]).
bin([1]).
bin([1|X]):- to_list(X), fill(X).


rfill([1]).
rfill([0|Xs]):- rfill(Xs).
rfill([1|Xs]):- rfill(Xs).

rbin([0]).
rbin(X):- to_list(X), rfill(X).


% Zadanie 4

mirror(leaf, leaf).
mirror(node(L,E,R),X):- mirror(L,R1), mirror(R,L1), X=node(L1,E,R1).


flatten(leaf,[]).
flatten(node(L,E,R),X) :- flatten(L,X1), flatten(R,X2), append(X1,[E|X2],X).


% Zadanie 5

insert(X,leaf, node(leaf,X,leaf)).
insert(X,node(L,Y,R),node(L,Y,R1)) :- X >= Y, insert(X,R,R1).
insert(X,node(L,Y,R), node(L1,Y,R)) :- X < Y, insert(X,L,L1). 

treesort(L,SortedL) :- treesort(L,SortedT,[]), flatten(SortedT,SortedL).
treesort([],Acc,Acc).
treesort([H|T],SortedT,Acc) :- insert(H, Acc, Acc1), treesort(T,SortedT,Acc1). 

% Zadanie 6


% Zadanie 7

revall(X,R) :- revall(X,R,[]).
revall([],R,R):- !.
revall([H|T],X,Ac) :- revall(H,H1),!, revall(T,X,[H1|Ac]).
revall([H|T],X,Ac) :- revall(T,X,[H|Ac]).