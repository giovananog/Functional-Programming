import Data.Char
import Data.Char (chr)

main :: IO ()    
main = return () 


-- 1. Using a list comprehension, give an expression that calculates the sum 1^2 +
-- 2^2 + ... 100^2 of the first one hundred integer squares.

double2 x = sum(map (^2) x)

doubleL x = sum [a^2 | a <- x]


--2. In a similar way to the function length, show how the library function
-- replicate :: Int -> a -> [a] that produces a list of identical elements can be
-- defined using a list comprehension. For example:
-- > replicate 3 True
-- [True, True, True]

replicate2 n x = [x | _ <- [1..n]]
replicate3 x y = [y | _ <- [1..x]]


-- 3. A triple (x, y, z) of positive integers is pythagorean if x^2 + y^2 = z^2.
-- Using a list comprehension, define a function pyths :: Int -> [(Int, Int, Int)]
-- that returns the list of all pythagorean triples whose components are at most a
-- given limit. For example:
-- > pyths 10
-- [(3,4,5), (4,3,5),(6,8,10),(8,6,10)]

pyths x = [(a,b,c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2 + b^2 == c^2]

pit x = [(w,y,z) | w <- [1..x], y <- [1..x], z <- [1..x], w^2 + y^2 == z^2]



-- 4. A positive integer is perfect if it equals the sum of its factors, excluding
-- the number itself. Using a list comprehension and the function factors, define a
-- function perfects :: Int -> [Int] that returns the list of all perfect numbers up
-- to a given limit. For example:
-- > perfects 500
-- [6,28,496]

divisores x = [a | a <- [1..(x-1)], (mod) x a == 0]
perfects x = [a | a <- [1..x], sum (divisores a) == a]

divi x = [a | a<-[1..(x-1)], (mod) x a == 0]
perfeitos x = [a | a <- [1..x], sum (divi a) == a]

-- 5. Show how the single comprehension [(x,y) | x <- [1,2,3], y <- [4,5,6]] with two
-- generators can be re-expressed using two comprehensions with single generators.
-- Hint: make use of the library function concat and nest one comprehension within
-- the other.
-- I must admit I was totally confused over this one.
-- [[(x,y)| y <- [4,5,6]] | x <- [1,2,3]]
-- >[[(1,4),(1,5),(1,6)],[(2,4),(2,5),(2,6)],[(3,4),(3,5),(3,6)]]

cart1 = [[(x,y)| y <- [4,5,6]] | x <- [1,2,3]]
cart2 = concat [[(x, y) | y <- [4,5,6]] | x <- [1,2,3]]

-- 6. Define the function find used in the function positions.
-- positions :: Eq a => a -> [a] -> [Int]
-- positions x xs = find x (zip xs [0..n])
-- where n = (length xs) - 1

find _ [] = []
find x (a:b)
   | fst a == x = snd a : find x b
   | otherwise  = find x b

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..(length xs - 1)])



-- 7. The scalar product of two lists of integers xs and ys of length n is given by
-- the sum of the products of corresponding integers.
-- n=1
-- --
-- \ (xsi * ysi)
-- /
-- --
-- i=0
-- In a similar manner to the function chisqr, show how a list comprehension can be
-- used to define a function scalarproduct :: [Int] -> [Int] -> Int that returns the
-- scalar product of two lists. For example:
-- > scalarproduct [1,2,3] [4,5,6]
-- 32

scalarproduct x y = sum[x1 * y2 | (x1,y2) <- zip x y]



-- 8. 1. Define the exponentiation operator &! for non-negative integers using the
-- same pattern of recursion as the multiplication operator *, and show how 2 &! 3 is
-- evaluated using your definition.

(&!) :: Int -> Int -> Int
x &! 0 = 1
x &! y = x * (x &! (y-1))



-- 9. 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed
-- using the higher-order functions
-- map and filter. Try to understand and apply the example [(+7) x | x <- [1..10],
-- odd x]

a f p xs = [f x | x <- xs, p x] 

a2 f p xs = map f (filter p xs)


-- 10. 4. Define a function dec2int :: [Int] -> Int that converts a decimal number
-- into an integer. for example:
-- > dec2int [2,3,4,5]
-- 2345
 
dec2int x = []


-- 11. A higher-order function unfold that encapsulates a simple pattern of recursion
-- for producing a list can be defined as follows:
-- unfold p h t x | p x = []
--  |otherwise = h x : unfold p h t (t x)

-- Define new functions so that you can call unfold function passing a list x as
-- parameter and new functions for passing a primitive type element x

unfold p h t x 
 | p x = []
 |otherwise = h x : unfold p h t (t x)
