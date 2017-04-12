% Zadanie 1

% stack

put(E,S,[E|S]).

get([H|T],H,T).

empty([]).

addall(E,G,S,R) :-
	findall(E,G,REs),
	append(Res,S,R).

% queue

put_q(E,S-X,S-Y):- X =[E|Y].

get_q([H|T]-X,H,T-X).

empty_q(X-X).


% findall/4: As findall/3, but returns the result as the difference list Bag-Tail.
addall_q(E, G ,S - B, S-T):- 
	findall(E,G,B,T).


% Zadanie 4

% [[c,s,c], [c,c], [c,c,c], [s,c,s,c], [c,s,s,s,c]]

fit(X,Y) :-X = c, (Y is 1; Y = 3; Y = 5; Y = 7; Y = 9).
fit(X,Y) :-X = s, (Y is 2; Y = 4; Y = 6; Y = 8; Y = 0).



fit_list(List, Fitted) :- fit_list(List, Fitted, []).
fit_list([],Acc,Acc).

fit_list([H|T], Fitted, Acc) :- fit(H,Y), append([(H,Y)], Acc, Acc1), fit_list(T, Fitted, Acc1).