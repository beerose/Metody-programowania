% Zadanie 3
%1
append(X,X,Y).
%2
select(X,[a,b,c,d],[a,c,d]).
%3
append([a,b,c],X,[a,b,c,d,e]).


% Zadanie 4

%1
even([]).
even([_,_|X]):- even(X).
%2
palindrom(X):- X = Y, reverse(X,Y).
%3
singleton([_]).


%Zadanie 5

%1
head(H,[H|_]).
%2
last(L,H):-reverse(L,Y),head(H,Y).
%3
tail(T,[_|T]).
%4
init([],[_]).
init([H|X],[H|T]):- init(X,T).
%5
prefix(P,L):-  append(P,_,L).
%6
suffix(S,L):-  append(_,S,L).


% Zadanie 6

sublist(_,[]).
sublist([X|Y], [X|Z]):- sublist(Y,Z).
sublist([_|X],Y):- sublist(X,Y).


% Zadanie 7

perm([],[]).
perm(L, [X|Y]):- select(X,L,Z), perm(Z, Y).


