% Zadanie 1

appn([],[]).
appn([X],X).

appn([H1,H2|T],X) :-
	append(H1,H2,Res).
	appn([Res|T],X).


% Zadanie 2

is_list([_|_]).
flatten_list([X],X).

flatten([X],X).
flatten([H1|T],Res):-
	flatten([H1|T],Res,[]).

flatten([],Res,Res).
flatten([H1|T],Res,Acc):-
	\+is_list(H1),
	append([H1],Acc,Acc2),
	flatten(T,Res,Acc2).
flatten([H1|T],Res,Acc):-
	is_list(H1),
	append(H1,Acc,Acc2),
	flatten(T,Res,Acc2).


% Zadanie 3

halve(List,Left,Right):-
	halve(List,List,Left,Right).

halve(Right,[],[],Right).
halve(Right,[_],[],Right).

halve([H|T],[_,_|Rest],[H|Left],Right):-
	halve(T,Rest,Left,Right).


% Zadanie 4

merge([],[],[]).
merge(T,[],T).
merge([],T,T).

merge([H1|T1],[H2|T2],[H1|Res]):-
	H1 < H2,
	merge(T1,[H2|T2],Res),!.

merge([H1|T1],[H2|T2],[H2|Res]):-
	H1 >= H2,
	merge([H1|T1],T2,Res),!.


merge_sort([],[]):-!.
merge_sort([X],[X]):-!.
merge_sort(Lista,Wynik):-
	halve(Lista,L,R),
	merge_sort(L,W1),
	merge_sort(R,W2),
	merge(W1,W2,Wynik).


% Zadanie 5


tail(X,0,X):-!.
tail([_|Lista],N,Tail):-
	N1 is N-1,
	tail(Lista,N1,Tail).
	

merge_s([],0,[]):-!.
merge_s([X|_],1,[X]):-!.
merge_s([X],_,[X]):-!.

merge_s(Lista,N,Wynik):-
	N1 is N div 2,
	N2 is N - N1,
	merge_s(Lista,N1,W1),
	tail(Lista,N1,L1),
	merge_s(L1,N2,W2),
	merge(W1,W2,Wynik).

% Zadanie 6

listToLists(Lista,Wynik):-
	l2l(Lista,Wynik,[]).


l2l([],Acc,Acc).
l2l([H|T],Wynik,Acc):-
	append([[H]],Acc,Acc1),
	l2l(T,Wynik,Acc1).

ms(Lista,Wynik):-
	listToLists(Lista,L1),
	merge_s1(L1,Wynik).


merge_s1(Lista,Wynik):-
	merge_s1(Lista,Wynik,[]).

merge_s1([],X,[X]):-!.
merge_s1([X],W,Acc):-
	merge_s1([X|Acc],W,[]).
merge_s1([],W,Acc):-
	merge_s1(Acc,W,[]).

merge_s1([X1,X2|T],Wynik,Acc):-
	merge(X1,X2,W1),
	merge_s1(T,Wynik,[W1|Acc]).



% Zadanie 7

split([],_,[],[]):-!.
split([X],Med,[X],[]):- X < Med,!.
split([X],Med,[],[X]):- X >= Med,!.

split([H|List],Med,[H|Small],Big):-
	H < Med,
	split(List,Med,Small,Big),!.


split([H|List],Med,Small,[H|Big]):-
	H >= Med,
	split(List,Med,Small,Big),!.


qsort(List,Wynik):-
	qsort(List,Wynik,[]).

qsort([],A,A).

qsort([H|T],Wynik,Acc):-	
	split(T,H,S,B),
	qsort(B,Acc1,Acc),
	qsort(S,Wynik,[H|Acc1]).


% Zadanie 9


sum(X, Y, Suma):-

    X is Suma - Y,
    Y is Suma - X,
        Suma is X + Y.
sum(X, Y, Suma):-
    integer(X),
    integer(Suma),
    !,
    Y is Suma - X.
sum(X, Y, Suma):-
    integer(Y),
    integer(Suma),
    !,
    X is Suma - Y.

