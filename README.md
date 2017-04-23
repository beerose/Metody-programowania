# Metody-programowania

## Pracownia nr 1
* program szukający wartościowań spełniających zadany zbiór klauzul

* główny predykat rozwiązujący zadanie: solve/2

* przyklad: ?solve([p v ~q], X). X = [(p,t),(q,t)]; X = [(p,f), (q,f)]; X = [(p,f), (q,f)].

## Pracownia nr 2

* program przeprowadzający rezolucyjny dowód sprzecznośći

* główne predykaty:

     1. resolve(X+,Pos+,Neq+,Res-) -- znajduje rezolwentę Res, względem literału X, który wsytępuje w liście Pos i zanegowany w liście Neq. 

         Przykład: 

         ?resolve(p, p v g, ~p v o, X). X = g v o.

     2. prove(Clauses,Solution) -- przeprowadza rezolucyjny dowód sprzeczności i zwraca jego wynik

         Przykład: 

         prove([p v q v ~r, ~p v q, r v q, ~q, p],Solution).


         Solution = [(p, axiom),  (~p v q, axiom),  (q, p, 2, 5),  (q, axiom),  (~q, axiom),  ([], q, 4, 6)] 

## Pracownia nr 3
* moduł eksportujący predykat parse/3 parsujący programy w jezyku HDML
* weryfikowanie poprawności układów cyfrowych
