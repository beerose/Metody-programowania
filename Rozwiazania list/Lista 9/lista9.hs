-- Zadanie 4
import Data.List
import Data.Char


fib:: Int -> Int
-- !! - bierze dany argument z listy
fib n = fib1 !! (n+1) where
		fib1 = 1:1:zipWith(+)fib1(tail fib1)

-- Zadanie 5

roots:: (Double, Double, Double) -> [Double]

roots (a,b,c) =
	if a == 0 then
		if b == 0 then []
		else [-c/b]
	else
		case compare delta 0 of  -- 0, bo porownywanie delty wzgedem 0 (wynika z compare)
		EQ -> [-b/(2*a)]
		LT -> []
		GT -> [(-b+sqrt(delta))/(2*a),(-b-sqrt(delta))/(2*a)]
		where delta = b*b - 4*a*c	

data Roots = No | One Double | Two (Double, Double) deriving Show
roots2 :: (Double, Double, Double) -> Roots
roots2 (a,b,c) =
	if a == 0 then
		if b == 0 then No
		else One (-c/b)
	else
		case compare delta 0 of  
		EQ -> One (-b/(2*a))
		LT -> No 
		GT -> Two ((-b+sqrt(delta))/(2*a),(-b-sqrt(delta))/(2*a))
		where delta = b*b - 4*a*c	

roots3 :: [Double] -> [Double]
roots3 [a, b, c] =  
	if a == 0 then
		if b == 0 then []
		else [-c/b]
	else
		case compare delta 0 of  
		EQ -> [-b/(2*a)]
		LT -> []
		GT -> [(-b+sqrt(delta))/(2*a),(-b-sqrt(delta))/(2*a)]
		where delta = b*b - 4*a*c


-- Zadanie 6




integerToString :: Integer -> String


integerToString 0 = "0"
integerToString n = (reverse.unfoldr (\n ->
	if n == 0 then Nothing
		else Just ((intToDigit.fromEnum) (mod n 10), div n 10)   )) n


-- Zadanie 7

newtype FSet a = FSet (a -> Bool) 

empty :: FSet a
empty = FSet(\_ -> False)
		
singleton :: Ord a => a -> FSet a
singleton a = FSet(\a -> True)

fromList :: Ord a => [a] -> FSet a
fromList xs = FSet (\x -> elem x xs)

union :: Ord a => FSet a -> FSet a -> FSet a
union (FSet a)  (FSet b) = FSet (\x -> a x || b x)

intersection :: Ord a => FSet a -> FSet a -> FSet a
intersection (FSet a)  (FSet b) = FSet (\x -> a x && b x )

--member :: Ord a => a -> FSet a -> Bool
--member a (FSet b) = a b
