% zadanie 1

kot(myCat).
przyjaciel(me, myCat).

lubi(X,Y) :- ptak(X), dzdzownica(Y).
lubi(X,Y) :- kot(X), ryba(Y).

lubi(X,Y) :- przyjaciel(X,Y).
lubi(X,Y) :- przyjaciel(Y,X).

je(myCat,X) :- lubi(myCat,X).


% zadanie 2

nie_smok(X) :- mieszka_w_zoo(X), szczesliwy(X).
styka_sie(X,Y) :- zwierze(X), czlowiek(Y), mily(Y), odwiedza_zoo(Y).
mily(X) :- czlowiek(X), odwiedza_zoo(X).
szczesliwe(X) :- zwierze(X), styka_sie(X,Y), czlowiek(Y), odwiedza_zoo(Y).

% brakujace zalozenia: 
% 1. smok to zwierze
% 2. odwiedzajacy stykaja sie ze wszystkimi zwierzetami


% zadanie 3

czlowiek(sokrates).
smiertelny(X) :- czlowiek(X).


% zadanie 4

sibling(X,Y) :- parent(Z,X), parent(Z,Y).
sister(X,Y) :-  sibling(X,Y), female(X).
grandson(X,Y) :- male(X), parent(Y,Z), parent(Z,X).
cousin(X,Y) :- parent(A,X), sibling(A,B), parent(B,Y).

descendant(X,Y) :- parent(Y,X).
descendant(X,Y) :- parent(A,X), descendant(A,Y).

is_mother(A) :- female(A), parent(A,_).
is_father(B) :- male(B), parent(B,_).


% zadanie 5

female(eve).
female(anna).
female(ivonne).
female(helen).
male(john).
male(mark).
male(joshua).
male(david).
male(adam).

parent(adam, helen).
parent(adam, ivonne).
parent(adam, anna).
parent(eve, helen).
parent(eve, ivonne).
parent(eve, anna).

parent(john,joshua).
parent(helen, joshua).
parent(ivonne,david).
parent(mark,david).


% zadanie 6

connected(wro,wa).
connected(wro,krk).
connected(wro,sz).
connected(sz,lub).
connected(sz,gni).
connected(wa,kato).
connected(gni,gli).
connected(lub,gli).

connection(X,Y) :- connected(X,Y).
connection(X,Y) :- connected(A,Y), connection(X,A).







