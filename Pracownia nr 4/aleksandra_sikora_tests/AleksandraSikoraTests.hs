-- Wymagamy, by moduł zawierał tylko bezpieczne funkcje
{-# LANGUAGE Safe #-}
-- Definiujemy moduł zawierający testy.
-- Należy zmienić nazwę modułu na {Imie}{Nazwisko}Tests gdzie za {Imie}
-- i {Nazwisko} należy podstawić odpowiednio swoje imię i nazwisko
-- zaczynające się wielką literą oraz bez znaków diakrytycznych.
module AleksandraSikoraTests(tests) where

-- Importujemy moduł zawierający typy danych potrzebne w zadaniu
import DataTypes

-- Lista testów do zadania
-- Należy uzupełnić jej definicję swoimi testami
tests :: [Test]
tests =
  [ 
      Test "inc"      (SrcString "input x in x + 1") (Eval [42] (Value 43))
    , Test "undefVar" (SrcString "x")                TypeError
    , Test "wrongTypes" (SrcString "2 + true") TypeError
    , Test "testEnum" (SrcString "5 = true") TypeError
    , Test "testUnaryNot" (SrcString "not 5") TypeError
    , Test "testUnaryNeg" (SrcString "- true") TypeError
    , Test "testBinaryOperatorAdd" (SrcString "true + false") TypeError
    , Test "testBinaryOperatorSub" (SrcString "true - 2") TypeError
    , Test "testBinaryOperatorAnd" (SrcString "1 and 2") TypeError
    , Test "testBinaryOperatorAnd2" (SrcString "true and 2") TypeError
    , Test "testBinaryOperatorOr" (SrcString "2 or 4") TypeError
    , Test "testBinaryOperatorEq" (SrcString "2 = true") TypeError
    , Test "testBinaryOperatorDiv" (SrcString "2 div true") TypeError
    , Test "testLet" (SrcString "let n = 1 in true") TypeError
    , Test "testLet2" (SrcString "let n = true in 1 + n") TypeError
    , Test "testLet" (SrcString "let x = 5 in true") TypeError
    , Test "testIf1" (SrcString "if 5 then true else false") TypeError
    , Test "testIf1" (SrcString "if true then 5 else false") TypeError
    , Test "testIf2" (SrcString "if 5 > 10 then true else 10") TypeError
    , Test "testEvalAdd" (SrcString "input a b in a + b + b") (Eval[1,1] (Value 3))
    , Test "testEvalSub" (SrcString "input a b c in a - b - c") (Eval[3,2,1] (Value 0))
    , Test "testEvalMul" (SrcString "input x in x * x") (Eval[1] (Value 1))
    , Test "testEvalDiv" (SrcString "input x in x div x") (Eval[10] (Value 1))
    , Test "tetsEvalMod" (SrcString "input x in x mod 2") (Eval[10] (Value 0))    
    , Test "testEvalEnum" (SrcString "input x in x") (Eval[10] (Value 10))
    , Test "testTwoVars" (SrcString "input x y in x + y") (Eval[1,2] (Value 3))
    , Test "testFromFIleSimpleInput" (SrcFile "fromFile1.pp4") (Eval[1,2,3] (Value 6))
    , Test "testFromFIleSimpleIf" (SrcFile "fromFile2.pp4") (Eval[] (Value 0))
    , Test "testFromFileOperatorsBindings" (SrcFile "fromFile3.pp4") (Eval[] (Value (-12)))
    , Test "testDivByZero" (SrcString "input x in 10 div 0") RuntimeError
    , Test "testNot" (SrcString "not 5") TypeError
    , Test "testsCount" (SrcString "1 * 2 * 3 * 4") (Eval[] (Value 24))

  ]