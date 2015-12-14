second xs = head (tail xs)
swap (x, y) = (y, x)
pair x y = (x, y)
double x = x * 2
palindrome xs = reverse xs == xs
twice f x = f (f x)
f xs = take 3 (reverse xs)

replicatef n a = [a | _ <- [1 .. n]]

factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects n = [x | x <- [1 .. n], isPerfect x]
	where isPerfect num = sum(factors num) == num
	
find k t = [v | (k', v) <- t, k == k']
positions x xs = find x (zip xs [0 .. n])
	where n = length xs -1
	
scalarproduct xs ys = sum [x * y | (x, y) <- xs `zip` ys]

riffle xs ys = concat [[x, y] | (x,y) <- xs `zip` ys]

divides x y = y `mod` x == 0
divisors x = [d | d <- [1..x], d `divides` x]