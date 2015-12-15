eval xs = foldl (\x y -> y + (10 * x)) 0 xs
evalRev xs = foldr (\x y -> x + (10 * y)) 0 xs

-- reverse [] = []
-- reverse (x:xs) = reverse xs ++ [x]

-- [] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)

toDigits x 
	| x >= 10 = toDigits(quot x 10) ++ [x `mod` 10]
	| otherwise = [x] 
	
toDigitsRev n = reverse(toDigits n)

doubleSecond :: [Integer] -> [Integer]
doubleSecond [] = []
doubleSecond (x : []) = [x]
doubleSecond (x :(y : xs)) = x : ((2*y) : doubleSecond(xs))

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x : xs)
	| x >= 10 = sumDigits(toDigits(x)) + sumDigits(xs)
	| otherwise = x + sumDigits(xs)
	
isValid :: Integer -> Bool
isValid n = sumDigits(doubleSecond(toDigitsRev(n)))	`mod` 10 == 0