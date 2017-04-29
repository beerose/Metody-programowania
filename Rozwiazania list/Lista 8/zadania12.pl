% Zadanie 1

tree(leaf) --> "*", {L = '*'}.

tree(node(A,B)) --> "(", tree(A), tree(B),  ")".


% Zadanie 2

expresion(E) --> simple_expresion(E).
expresion(E) --> expresion(E1), "*", simple_expresion(E2), {E = E1*E2}.

simple_expresion(E) --> "a".
simple_expresion(E) --> "b".
simple_expresion(E) --> "(", expresion(E), ")".