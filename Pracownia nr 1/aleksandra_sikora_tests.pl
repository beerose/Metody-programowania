
% Definiujemy moduł zawierający testy.
% Należy zmienić nazwę modułu na {imie}_{nazwisko}_tests gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez znaków diakrytycznych
:- module(aleksandra_sikora_tests, [tests/5]).

% definiujemy operatory ~/1 oraz v/2
:- op(200, fx, ~).
:- op(500, xfy, v).

% Zbiór faktów definiujących testy
% Należy zdefiniować swoje testy


tests(excluded_middle, validity, [p v ~p], 500, solution([(p,t)])).

tests(simple_test1, validity, [a v b, u, i, o, p, k, i , j], 500, solution([(a, t),  (b, t),  (i, t),  (j, t),  (k, t),  (o, t),  (p, t),  (u, t)])).
tests(simple_test2, validity, [p,~p], 500, count(0)).


tests(klauzulaZPowtorzeniami1, validity, [p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r],500, solution([(p, f),  (q, f),  (r, t)])).
tests(klauzulaZPowtorzeniami2, validity, [p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p], 500, solution([(p,t)])).
tests(klauzulaZPowtorzeniami3, validity, [p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p,p], 500, solution([(p,t)])).
tests(klauzulaZPowtorzeniamiNegacji, validity, [~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p,~p], 500, solution([(p,f)])).



tests(klauzulaPusta, validity, [ ], 500, solution([])).

tests(klauzulaZListaPusta, validity,  [p v q, [], p], 500, solution([(p, t),  (q, f)])).



tests(dlugaLista1, validity, [ p,q,s,z,f, z v q v q v q v q v f v t v p v q v f v q v p v s v q v q v s v t v f v s v p v p v t v f v t v f v q v s v q v f v q v s v q v f v s v p v f v t v p v p v f v f v f v t v s v t v t v p v q v q v p v p v s v p v t v t v f v p v t v s v q v s v t v s v q v f v f v q v f v s v f v t v q v f v s v p v p v p v q v q v s v f v t v s v f v t v p v q v s v q, t v q v s v t v q v p v f v f v f v f v q v t v p v f v q v s v q v f v t v s v t v t v s v f v s v q v f v f v p v q v t v f v q v f v s v t v f v s v s v p v s v q v q v q, s v t v p v q, q v f, q v f v f v t v s v p v p v q v q v q v f v s v p v q v p v f v s v t v q v t v q v s v t v q v p v q v q v p v s v t v p v q v q v q v f v s v f v p v q v p v s v p v f v s v p v s v f v p v s v s v t v p v q v s v t v t v q v q v s v t v f v t v q v p v s v f v f v q v t v p v s v t v f, t v f v s v t v t v s v f v q v t v q v f v p v q v t v t v p v f v p v f v f v f v t v p v t v p v t v t v p v t v q v f v q v p v q v q v s v q v p v t v s v q v s v f v t v q v q v s v f v t v f v s v s v s v t v f v p v s v t v s v s v s v s v f v p v p v q v f v q v f v f v t v p v s v s v q v s v s v q v f, f v f v q v t v f v q v q v t v t, q v p v p v q v p v p v s v p v q v q v q v p v t v q v s v t v t v q v f v p v s v q v t v t v t v q v s v s v f v p v p v f v s v t v p v t v p v s v s v t v s v s v t v p v t v f v q v s v f v s v f v f v s v t v s v q v q v s v f v t v t v t v p v q v q v p v s v p v s v s v p v t v t v s v f v f v t v f v q v t v q v s v t v q v f v t v f v p v f v f, q v p v t v f v s v q v s v s v f v p v s v p v t v s v s v t v p v q v q v s v t v p v t v f v s v t, p v p v t v p v p v q v q v p v t v s v q v t v p v f v f v q v f v s v p v p v f v q v p v s v p v s v s v s v p v q v f v t v p v q v q v s, t v s v f v t v s v t v p v q v q v p v t v s v p v p v t v p v f v s v s v s v t v s v s v t v q v q v s v s v t v p v p v p v p v p v f v f v q v f v t v t v p v t v s v f v p v f v s v p v t v t v s v p v f v s v f v t v f v s v s v q v q v f v f v f v p v q v q v f v s v q v f v f v t v q v t v p v f v q v f v q v s v s v s v p v p v s v t v f v q v t v t v q v p]
,500, solution([ (t, t), (z, t), (s, t), (q, t), (p, t), (f, t)])).


tests(inneDane1, validity, [v v v], 500, count(1)).
tests(inneDane2, validity, [!,~,ania], 500, solution([(!, t),  (ania, t),  ((~), t)])).



tests(alterantywaNegacji, validity, [~p v ~b v ~n v ~m v ~a v ~d], 500, solution( [(a, f),  (b, f),  (d, f),  (m, f),  (n, f),  (p, f)])).
tests(pojedynczaAlternatywaNegacji, validity, [p v ~q], 500, count(3)).
tests(pojedynczaNegacja, validity, [~p], 500, solution([(p,f)])).

tests(alternatywa, validity, [a v b v c v d], 500, count(15)).



tests(testWydajnosciowy1,performance, [p v q v r v p v r v f, f v b v b v m, m, q, w, u], 500,solution([(b, f),  (f, f),  (m, t),  (p, f),  (q, t),  (r, f),  (u, t),  (w, t)])).
tests(testWydajnosciowy3, performance,[a v b v c v d v m v n], 1000, count(63) ).
tests(testWydajnosciowy4, performance,  [p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r,p v q v r],1000, solution([(p, f),  (q, f),  (r, t)])).