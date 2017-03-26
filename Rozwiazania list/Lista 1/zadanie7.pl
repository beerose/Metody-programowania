istnieje(Dom, domy(Dom,_,_,_,_)).
istnieje(Dom, domy(_,Dom,_,_,_)).
istnieje(Dom, domy(_,_,Dom,_,_)).
istnieje(Dom, domy(_,_,_,Dom,_)).
istnieje(Dom, domy(_,_,_,_,Dom)).


pierwszy(Dom, domy(Dom,_,_,_,_)).


srodkowy(Dom, domy(_,_,Dom,_,_)).


lewy_sasiad(L,P, domy(L,P,_,_,_)).
lewy_sasiad(L,P, domy(_,L,P,_,_)).
lewy_sasiad(L,P, domy(_,_,L,P,_)).
lewy_sasiad(L,P, domy(_,_,_,L,P)).

prawy_sasiad(P,L,X) :- lewy_sasiad(L,P,X).


sasiad(A,B,X) :- lewy_sasiad(A,B,X).
sasiad(A,B,X) :- prawy_sasiad(A,B,X).

zagadka(Wodka,Slon) :-
	istnieje(dom(anglik,_,_,_,czerwony),X),
	istnieje(dom(hiszpan,pies,_,_,_),X),
	istnieje(dom(_,_,<,kawa,zielony),X),
	istnieje(dom(ukrainiec,_,_,herbata,_),X),
	sasiad(dom(_,_,_,_,zielony),dom(_,_,_,_,bialy),X),
	istnieje(dom(_,waz,winstony,_,_),X),
	istnieje(dom(_,_,koola,_,zolty),X),
	srodkowy(dom(_,_,_,mleko,_),X),
	pierwszy(dom(norweg,_,_,_,_),X),
	sasiad(dom(_,_,chesterfield,_,_),X),
	sasiad(dom(_,_,koola,_,_), dom(_,lis,_,_,_), X),
	istnieje(dom(_,_,lucky,sok,_),X),
	istnieje(dom(japonczyk,_,kenty,_,_),X),
	sasiad(dom(norweg,_,_,_,_), dom(_,_,_,_niebieski),X),
	istnieje(dom(Wodka,_,_,wodka,_),X),
	istnieje(dom(Slon,slon,_,_,_),X).