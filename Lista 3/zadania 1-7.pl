% Zadanie 1

perm([],[]).
perm([H|T], T1) :-  select(H, T1, P), perm(T,P).


% Zadanie 2

filter([],[]).
filter([H|T], [H|P]) :- H > 0, filter(T, P),!.
filter([_|T], P) :- filter(T, P).



count(Elem,[],0).
count(Elem, [H|T], N) :- count(Elem, T, N1), Elem = H, N is N1 +1,!.
count(Elem, [_|T], N) :- count(Elem, T, N).


exp(A,1,A).
exp(A,B,Res) :- B1 is B-1, exp(A,B1,Res1),  Res is Res1*A, !.

pow(A, B, R) :- R is A**B.

% Zadanie 3

%a)
factorial(0,1). 
factorial(N,F) :-  N>0, N1 is N-1, factorial(N1,F1), F is N * F1.

%b)
concat_number(Digits, Num):- concat_number(Digits, Num, 0).

concat_number([], Acc, Acc).
concat_number([H|T],  Res, Acc):- Acc1 is (Acc * 10) + H, concat_number(T, Res, Acc1).

%c)
decimal(Num, Digits):- decimal(Num, Digits, []).

decimal(0, Acc, Acc) :- !.

decimal(Num, Res, Acc):- Digit is Num mod 10, Num1 is Num // 10, decimal(Num1, Res, [Digit|Acc]).


% Zadanie 4 
% select_max + select_min, bo tak, plus dodatkowa wersja bez akumulatora.

select_max([],Min, Rest) :- false.
select_max([H|T], Y, Z) :- sm2(T,Y , Z, H).

sm2([],Acc,[],Acc).
sm2([H|T], Min, [Acc|Rest], Acc) :- H > Acc, sm2(T,Min, Rest, H), !.
sm2([H|T], Min, [H|Rest], Acc) :- sm2(T, Min, Rest, Acc).


%select_min
select_min([],Min, Rest) :- false.
select_min([H|T], Y, Z) :- sm(T ,Y , Z, H).

sm([],Acc,[],Acc).
sm([H|T], Min, [Acc|Rest], Acc) :- H < Acc, sm(T,Min, Rest, H), !.
sm([H|T], Min, [H|Rest], Acc) :- sm(T, Min, Rest, Acc).

%select_min bez akumulatora
select_min_p([], _, []) :- false.
select_min_p([H],H,[]).
select_min_p([H|T],Min ,Rest) :- select_min_p(T, TM, TR), (H < TM, H = Min, Rest = [TM|TR] ;
														   TM = Min, Rest = [H|TR]),!.


% Zadanie 5


insert(H,[],[H]).
insert(Elem,[H|L], Res):- Elem=<H, Res=[Elem,H|L].
insert(Elem,[H|L], Res):- insert(Elem,L,Res1), Res=[H|Res1],!.

ins_sort([],[]).
ins_sort([H|L],S):- ins_sort(L,S1), insert(H, S1,S),!.


% Zadanie 6

reverse(X, Y) :- reverse(X, [], Y).

reverse([], A, A).
reverse([H|T], A, Y) :- reverse(T, [H|A], Y).


reverse_poprawiony(X, Y) :- reverse_poprawiony(X, Y, []).

reverse_poprawiony([], A, A).
reverse_poprawiony([H|T], Y, Acc) :- reverse_poprawiony(T, Y, [H|Acc]),!.


% Zadanie 7

perm_poprawiony([],[]).
perm_poprawiony([H|T], T1) :-  select(H, T1, P), perm_poprawiony(T,P).