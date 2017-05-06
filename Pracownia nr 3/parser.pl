% Definiujemy moduł zawierający rozwiązanie.
% Należy zmienić nazwę modułu na {imie}_{nazwisko} gdzie za
% {imie} i {nazwisko} należy podstawić odpowiednio swoje imię
% i nazwisko bez wielkich liter oraz znaków diakrytycznych
:- module(aleksandra_sikora, [parse/3]).

% Główny predykat rozwiązujący zadanie.
% UWAGA: to nie jest jeszcze rozwiązanie; należy zmienić jego
% definicję.
parse(_Path,CharCodeList, Res) :-
   phrase(lexer(Lex), CharCodeList),
   phrase(program(Res), Lex).



% Lexer

lexer(Tokens) -->
   blank_sign,
   (  (  
        "(",       !, { Token = tokLParen }
      ;  ")",       !, { Token = tokRParen }
      ;  "[",       !, { Token = tokLParenK }
      ;  "]",       !, { Token = tokRParenK }
      ;  "..",		!, { Token = tokDots}
      ;  "+",       !, { Token = tokAdd }
      ;  "-",       !, { Token = tokSub }
      ;  "*",       !, { Token = tokMul }
      ;  "=",       !, { Token = tokE }
      ;  "<>",      !, { Token = tokD }
      ;  "<=",      !, { Token = tokLE }
      
      ;  ">=",      !, { Token = tokGE }
      ;  "<",       !, { Token = tokL } 
      ;  ">",       !, { Token = tokG }
      ;  ",",       !, { Token = tokComma } 
      ;	 "&", 		!, { Token = tokEnd }
      ;  "/",       !, { Token = tokDiv}
      ;  "|",		!, { Token = tokOr}
      ;  "^",		!, { Token = tokPow}
      ;  "%", 		!, { Token = tokMod}
      ;  "@",		!, { Token = tokAt}
      ;  "~",		!, { Token = tokTylda}
      ;  "#",		!, { Token = tokHash}
      ;  "_",   !, { Token = tokUnd}
      ;  digit(D),  !,
            number(D, N),
            { Token = tokNumber(N) }
      ;  letter(L), !, identifier(L,Id),
            {  member((Id, Token), [ (def, tokDef),
                                     (else, tokElse),
                                     (if, tokIf),
                                     (in, tokIn),
                                     (let, tokLet),
                                     (then, tokThen),
                                     ('_', tokUnd) ]),
               !
            ;  Token = tokVar(Id)
            }
      ;  [_],
            { Token = tokUnknown }
      ),
      !,
         { Tokens = [Token | TokList] },
      lexer(TokList)
   ;  [],
         { Tokens = [] }
   ).



blank_sign -->
   [Char], { code_type(Char, space) }, !, blank_sign.
blank_sign -->
    "(*", !, comment.
blank_sign -->
   [].
comment -->
      "*)", !, blank_sign.
comment -->
      "(*",!;
      [_], comment.
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) --> 
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) ; A = 39 ; A = 95 }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) --> 
   alphanum(As),
      { atom_codes(Id, [L|As]) }.

 

 % Parser


program([]) --> [].
program(Asc) --> definicje(Asc).

%〈definicje〉 ::= 〈puste〉 | 〈definicja〉 〈definicje〉

definicje(Asc) --> definicja(A), {Asc = [A|Rest]}, definicje(Rest).
definicje([]) --> [].



%〈definicja〉 ::= def 〈identyfikator〉 ( 〈wzorzec〉 ) = 〈wyra˙zenie〉
definicja(Asc) --> [tokDef], [tokVar(Program_id)],
 [tokLParen], wzorzec(C), 
 [tokRParen], [tokE], wyrazenie(W), 
 {Asc = def(Program_id, C, W)}.


% 〈wzorzec〉 ::= _ | 〈zmienna〉 | ( 〈wzorzec〉 ) | 〈wzorzec〉 , 〈wzorzec〉
wzorzec(W) -->  [tokUnd], [tokComma],wzorzec(Y), {W = pair(no,wildcard(no),Y)}
  ; [tokUnd], {W = wildcard(no)},!.
wzorzec(W) --> zmienna(Z), [tokComma],wzorzec(Y), {W = pair(no,var(no,Z),Y)}
  ; zmienna(Z), {W = var(no,Z)},!.
wzorzec(W) --> [tokLParen], wzorzec(W), [tokRParen],!.

%〈wyra˙zenie〉 ::= if 〈wyra˙zenie〉 then 〈wyra˙zenie〉 else 〈wyra˙zenie〉
%| let 〈wzorzec〉 = 〈wyra˙zenie〉 in 〈wyra˙zenie〉
%| 〈wyra˙zenie op.〉

wyrazenie(E) --> [tokIf], wyrazenie(E1), [tokThen], wyrazenie(E2), [tokElse], wyrazenie(E3), {E = if(no,E1,E2,E3)}.
wyrazenie(E) --> [tokLet], wzorzec(W), [tokE], wyrazenie(E1), [tokIn], wyrazenie(E2), {E = let(no,W,E1,E2)}.
wyrazenie(E) --> wyrazenie_op(E).

%〈wyra˙zenie op.〉 ::= 〈wyra˙zenie op.〉 〈operator binarny〉 〈wyra˙zenie op.〉
%| 〈operator unarny〉 〈wyra˙zenie op.〉
%| 〈wyra˙zenie proste〉


wyrazenie_op(W) --> wyrazenie_op2(W1), [tokComma], wyrazenie_op(W2), {W = pair(no,W1,W2)}.
wyrazenie_op(W) --> wyrazenie_op2(W).

wyrazenie_op2(W) --> wyrazenie_op3(W1), operator_binarny2(Op), wyrazenie_op3(W2), {W = op(no,Op,W1,W2)}.
wyrazenie_op2(W) --> wyrazenie_op3(W).


wyrazenie_op3(W) --> wyrazenie_op4(W1), operator_binarny3(Op), wyrazenie_op3(W2), {W = op(no,Op,W1,W2)}.
wyrazenie_op3(W) --> wyrazenie_op4(W).


wyrazenie_op4(W) --> wyrazenie_op5(Acc), wyrazenie_op4_pom(W, Acc).
wyrazenie_op4_pom(W, Acc) --> operator_binarny4(Op), wyrazenie_op5(Y), {Acc1 = op(no, Op, Acc, Y)}, wyrazenie_op4_pom(W, Acc1).
wyrazenie_op4_pom(W, W) --> [].


wyrazenie_op5(W) --> wyrazenie_unarne(Acc), wyrazenie_op5_pom(W, Acc).
wyrazenie_op5_pom(W, Acc) --> operator_binarny5(Op), wyrazenie_unarne(Y), {Acc1 = op(no, Op, Acc, Y)}, wyrazenie_op5_pom(W, Acc1).
wyrazenie_op5_pom(W, W) --> [].


wyrazenie_unarne(W) --> operator_unarny(Op), wyrazenie_unarne(W1), {W = op(no, Op, W1)}.
wyrazenie_unarne(W) --> wyrazenie_proste(W).

% 〈operator binarny〉 ::= , | = | <> | < | > | <= | >= | ^ | | | + | - | & | * | / | % | @
%operator_binarny(,) --> [tokComma], !.
operator_binarny2('=') --> [tokE], !. 
operator_binarny2('<>') --> [tokD], !.
operator_binarny2('<') --> [tokL], !.
operator_binarny2('>') --> [tokG], !.
operator_binarny2('<=') --> [tokLE], !.
operator_binarny2('=>') --> [tokGE], !.
operator_binarny4('^') --> [tokPow], !.
operator_binarny4('|') --> [tokOr], !.
operator_binarny4('+') --> [tokAdd], !.
operator_binarny4('-') --> [tokSub], !.
operator_binarny5('&') --> [tokEnd], !.
operator_binarny5('*') --> [tokMul], !.
operator_binarny5('/') --> [tokDiv], !.
operator_binarny5('%') --> [tokMod], !.
operator_binarny3('@') --> [tokAt], !.


%〈operator unarny〉 ::= - | # | ~
operator_unarny('-') --> [tokSub], !.
operator_unarny('#') --> [tokHash], !.
operator_unarny('~') --> [tokTylda].

%〈wyra˙zenie proste〉 ::= ( 〈wyra˙zenie〉 )
%| 〈wybór bitu〉
%| 〈wybór bitów〉
%| 〈wyra˙zenie atomowe〉

wyrazenie_proste(W) --> wyrazenie_atomowe(W).

wyrazenie_proste(W) --> [tokLParenK], wyrazenie(W), [tokRParenK],!.
wyrazenie_proste(W) --> wybor_bitu(W),!.



% <wybór bitu〉 ::= 〈wyra˙zenie proste〉 [ 〈wyra˙zenie〉 ]
% 〈wybór bitów〉 ::= 〈wyra˙zenie proste〉 [ 〈wyra˙zenie〉 .. 〈wyra˙zenie〉 ]

wybor_bitu(W) --> (wyrazenie_atomowe(Acc); [tokLParen], wyrazenie(Acc), [tokRParen]),  wybor_bitu_pom(W, Acc).

wybor_bitu_pom(W, Acc) --> [tokLParenK], wyrazenie(W2), [tokRParenK], {Acc1 = bitsel(no, Acc, W2)}, wybor_bitu_pom(W, Acc1)

      ; [tokLParenK], wyrazenie(W2), [tokDots], wyrazenie(W3), [tokRParenK], {Acc1 = bitsel(no, Acc, W2, W3)}, wybor_bitu_pom(W, Acc1).

wybor_bitu_pom(W,W) --> [],!.


%〈wyra˙zenie atomowe〉 ::= 〈zmienna〉
%| 〈wywołanie funkcji〉
%| 〈literał całkowitoliczbowy〉
%| 〈pusty wektor〉
%| 〈pojedynczy bit〉
wyrazenie_atomowe(W) --> zmienna(Z), {W = var(no,Z)}.
wyrazenie_atomowe(W) --> wywolanie_funkcji(W).
wyrazenie_atomowe(W) --> [tokNumber(L)], {W = num(no, L)}.
wyrazenie_atomowe(W) --> pusty_wektor(P), {W = empty(no)},!.
wyrazenie_atomowe(W) --> pojedynczy_bit(B), {W = bit(no, B)}.

%〈zmienna〉 ::= 〈identyfikator〉
zmienna(Z) --> [tokVar(Zmienna)],{Z = Zmienna}.

%〈wywołanie funkcji〉 ::= 〈identyfikator〉 ( 〈wyra˙zenie〉 )
wywolanie_funkcji(F) --> [tokVar(I)], [tokLParen], wyrazenie(W), [tokRParen], {F = call(no, I, W)}.

% 〈pusty wektor〉 ::= [ ]
pusty_wektor(P) --> [tokLParenK], [tokRParenK].

%〈pojedynczy bit〉 ::= [ 〈wyra˙zenie〉 ]
pojedynczy_bit(B) --> [tokLParenK], wyrazenie(W), [tokRParenK], {B = W}.

