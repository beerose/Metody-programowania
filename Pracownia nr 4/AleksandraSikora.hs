{-# LANGUAGE Safe #-}

module AleksandraSikora where

import           AST
import           DataTypes

import           Data.List
import qualified Data.Map  as Map
import           Prelude   hiding (Map, lookup)



-- deklaracje typow uzytych w zadaniu

-- zdefiniowanie srodowiska dla funkcji typecheck
type TypeEnviroment = Map.Map Var Type
type Error p = TypeCheckResult p
data Type = Integer | Bool deriving (Show, Eq)

-- zdefiniowanie srodowiska dla funkcji eval
type ValueEnviroment = Map.Map Var Value
data Value = VInteger Integer | VBool Bool deriving (Show, Eq, Ord)


-- listy pomocnicze, przechowujace rozne typy operatorow binarnych
boperatorsBoolean = [BAnd, BOr]
boperatorsComparison = [BEq, BNeq, BLt, BGt, BLe, BGe]
boperatorsArithmetic = [BAdd, BSub, BMul, BDiv, BMod]


-- funkcja sprawdzajaca czy typ podany jako pierwszy argument zgadza sie 
-- z typem wyrazenia podanym jako drugi argument

checkInnerType :: Type -> Either (Error p) Type -> (p, String) -> Either (Error p) Type
checkInnerType expectedType x error = case x of
      Left oldError -> Left oldError
      Right innerType -> if innerType == expectedType 
            then Right expectedType 
            else let (outerP, s) = error in Left $ Error outerP s 


-- funkcja zwracająca typ argumentow jakie przyjmuje operator binarny podany jako argument funkcji
checkBinaryOperatorInput :: BinaryOperator -> Type
checkBinaryOperatorInput operator = 
      if operator `elem` boperatorsBoolean then Bool
      else Integer

-- pomocnicza funkcja do obslugi operatorow binarnych
handleBinary :: (p, TypeEnviroment, BinaryOperator, Expr p, Expr p) -> Either (Error p) Type     
handleBinary (p, env, binaryOperator, left, right) =
      case sideType expectedInput left of
            Left oldError -> Left oldError  
            Right leftType ->
                  case sideType leftType right of
                        Left oldError -> Left oldError
                        Right rightType -> Right outerType
      where expectedInput = checkBinaryOperatorInput binaryOperator
            sideType expt side = checkInnerType expt (inferType env side) (p, "Binary expresions of different types.")
            outerType = if binaryOperator `elem` boperatorsComparison then Bool else expectedInput
            

-- pomocnicza funkcja do obslugi operatorow unarnych
handleUnary :: (p, TypeEnviroment, UnaryOperator, Expr p) -> Either (Error p) Type
handleUnary (outerP, outerEnv, operator, expr)
  | operator == UNot = 
        checkInnerType Bool (inferType outerEnv expr) (outerP, "Unary Bool error.")
  | operator == UNeg = 
        checkInnerType Integer (inferType outerEnv expr) (outerP, "Unary Integer error.")
  | otherwise = Left $ Error outerP "Unary Wrong Operator error."


-- pomocnicza funkcja do obslugi wyrazenia Let
handleLet :: (p, Var, TypeEnviroment, Expr p, Expr p) -> Either (Error p) Type
handleLet (p, var, typeEnviroment, expr1, expr2) =
    case inferType typeEnviroment expr1 of
        Left oldError -> Left oldError
        Right newType -> inferType (Map.insert var newType typeEnviroment) expr2
    where newType = inferType typeEnviroment expr1 


-- funkcja sprawdzajaca czy wyrazenia expr1, expr2 z if _ then expr1 else expr2 
-- sa tego samego typu 
eithersEqual :: p -> Type -> Type -> Either (Error p) Type
eithersEqual p typeThen typeElse =
    if typeElse == typeThen then Right typeThen
    else Left $ Error p  "Else statement type and then statement type not equal."

-- pomocnicza funkcja do obslugi wyrazenia If
handleIf :: (p, TypeEnviroment, Expr p, Expr p, Expr p) -> Either (Error p) Type
handleIf (p, env, exprIf, exprThen, exprElse) =
    case inferType env exprIf of
        Left oldError -> Left oldError
        Right newType -> case newType of
                            Bool -> case inferType env exprThen of
                                        Left oldError -> Left oldError
                                        Right thenType -> case inferType env exprElse of
                                                            Left oldError -> Left oldError
                                                            Right elseType -> eithersEqual p thenType elseType
                            _ -> Left $ Error p  "If statement not bool."
        where newType = inferType env exprIf

-- funckja zwracajaca typ danego wyrazenia
inferType :: TypeEnviroment -> Expr p -> Either (Error p) Type
inferType envt expr = case expr of
  ENum _ _    -> Right Integer
  EBool _ _   -> Right Bool
  EVar p var  -> 
          case envLookup envt var of
                Just t  -> Right t
                Nothing  -> Left $ Error p $ lookupError var
  EUnary p operator innerExpr 
              -> handleUnary (p, envt, operator, innerExpr)
  EBinary p operator leftExpr rightExpr 
              -> handleBinary (p, envt, operator, leftExpr, rightExpr)  
  ELet p var expr1 expr2 -> handleLet (p, var, envt, expr1, expr2)  
  EIf p exprIf exprThen exprElse -> handleIf (p, envt, exprIf, exprThen, exprElse)
  where
        lookupError v = "No variable like " ++ show v ++ " in the TypeEnviroment."

-- sprawdzenie jaki jest typ zmiennej var srodowisku
envLookup :: TypeEnviroment -> Var -> Maybe Type
envLookup envt var = Map.lookup var envt

-- funckja ktora sprawdza czy program jest poprawny
typecheck :: [Var] -> Expr p -> TypeCheckResult p
typecheck vars expr = case inferType env expr of      
            Left error -> error
            Right resultType -> if resultType == Integer 
                                    then Ok
                                    else Error (getData expr) "Program returns bool."
      where env = Map.fromList $ zip vars $ repeat Integer



-- funckja zwracaja wartosc programu
inferEval :: ValueEnviroment -> Expr p -> Either (Error p) Value
inferEval valEnv expr = case expr of
      ENum _ val        -> Right $ VInteger val
      EBool _ bool      -> Right $ VBool bool
      EVar p var        -> case Map.lookup var valEnv of
                              Just t -> Right t
                              Nothing -> undefined
      EUnary p operator expr1 -> case inferEval valEnv expr1 of
                                    Left error -> Left error 
                                    Right innerValue -> case operator of
                                                            UNeg -> Right (let VInteger v = innerValue in VInteger $ negate v)
                                                            UNot -> Right (let VBool v = innerValue in VBool $ not v)
      EBinary p operator expr1 expr2 -> 
                        if operator `elem` boperatorsArithmetic then evalArthimetic p operator ev1 ev2         
                        else if operator `elem` boperatorsBoolean then evalBool p operator ev1 ev2
                        else  evalComparsion p operator ev1 ev2
                                          where ev1 = inferEval valEnv expr1 
                                                ev2 = inferEval valEnv expr2 
      EIf p expr1 expr2 expr3 -> case inferEval valEnv expr1 of
                                    Left error -> Left error 
                                    Right innerValue -> case innerValue of
                                          VBool True -> inferEval valEnv expr2
                                          VBool False -> inferEval valEnv expr3

      ELet p var expr1 expr2 -> case inferEval valEnv expr1 of
                                    Left error -> Left error
                                    Right innerValue -> inferEval (Map.insert var innerValue valEnv) expr2
                                          

-- funckja zwraca wartosc wyrazen z operatorami binarnymi porownania
evalComparsion :: p -> BinaryOperator -> Either (Error p) Value -> Either (Error p) Value -> Either (Error p) Value
evalComparsion p op (Right (VInteger a)) (Right (VInteger b)) = case op of
      BEq  -> Right     $ VBool (a == b)
      BNeq -> Right     $ VBool (not(a == b))
      BLt  -> Right     $ VBool (a < b)
      BGt  -> Right     $ VBool (a > b)
      BLe  -> Right     $ VBool (a <= b)
      BGe  -> Right     $ VBool (a >= b)

-- funckja zwraca wartosc wyrazen z operatorami boolowskimi 
evalBool :: p -> BinaryOperator -> Either (Error p) Value -> Either (Error p) Value -> Either (Error p) Value
evalBool p op (Right (VBool a)) (Right (VBool b)) = case op of
      BAnd -> Right     $ VBool (a && b)
      BOr  -> Right     $ VBool (a || b)

-- funckja zwraca wartosc wyrazen z operatorami arytmetycznymi
evalArthimetic :: p -> BinaryOperator -> Either (Error p) Value -> Either (Error p) Value -> Either (Error p) Value
evalArthimetic p op (Right (VInteger a)) (Right (VInteger b)) = case op of
      BAdd -> Right     $ VInteger (b + a) 
      BSub -> Right     $ VInteger (b - a)
      BMul -> Right     $ VInteger (a * b)
      BMod -> Right     $ VInteger (b `mod` a)
      BDiv -> case b of
                  0           -> Left      $ Error p "Dividing by zero."
                  otherwise   -> Right     $ VInteger (b `div` a)


-- glowna funkcja, ktora zwraca wartosc programu lub RuntimeError
eval :: [(Var,Integer)] -> Expr p -> EvalResult
eval vars expr = case inferEval env expr of
                        Left error -> RuntimeError
                        Right result  -> case result of
                              VInteger int -> Value int
                              VBool _ -> undefined
      where env = Map.fromList $ map (\(v, i) -> (v, VInteger i)) vars  
