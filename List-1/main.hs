import Data.Char
main :: IO ()    
main = return () 

somador :: Int -> Int
somador 0 = 0
somador n = n + somador (n-1)

fatorial :: Int -> Int
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

promocao :: (String,Int,Int) -> (String, Int, Int) -> Bool
promocao (x,y,z) (a,b,c)
                 |x == a && y <= b && z <= c = True 
                 |otherwise  = False
-----------------------------------------------------------------------------ex 1 

ex1a :: Double -> Double
ex1a x
       | x >= 0     = (x+4)/(x+2) 
       | otherwise  = 2/x


ex1b :: Double -> Double -> Double
ex1b x y 
       | x >= y     = x+y 
       | otherwise  = x-y


ex1c :: Double -> Double -> Double -> Double 
ex1c x y z 
       | (x+y) > z     = x+y+z
       | (x+y) < z     = x-y-z
       | otherwise  = 0





-----------------------------------------------------------------------------ex 2


fat :: Int -> Int
fat 0 = 1  -- estava faltando uma condicao de parada
fat x = x * fat (x-1)



-----------------------------------------------------------------------------ex 3


-- soma :: Int -> Int -> Int
-- soma x y = x + y

-- mult2 :: Int -> Int -> Int
-- mult2 y x 
--    | y == 0    = 0
--    | otherwise = soma x mult2 x (y-2)


-----------------------------------------------------------------------------ex 4

invertInt :: Int -> Int
invertInt x = ((x `mod` 10)*100) + (x`div`100) + ((x`div`10)`mod`10)*10




-----------------------------------------------------------------------------ex 5


square :: Int -> Int
square x = x * x 

fourPower :: Int -> Int
fourPower x = square x * square x




-----------------------------------------------------------------------------ex 6

raizde6 :: Double -> Double 
raizde6 0 = sqrt 6
raizde6 x = sqrt (6 + raizde6 (x-1));



-----------------------------------------------------------------------------ex 7


escolhaObj :: Int -> Int -> Int
escolhaObj x y 
    | x >= y    = fat x `div` (fat y * fat (x - y))
    | otherwise = 0




-----------------------------------------------------------------------------ex 8

mdc :: Int -> Int -> Int 
mdc m n 
  | m `mod` n /= 0 = mdc n (m `mod` n)
  |otherwise        = n




-----------------------------------------------------------------------------ex 9

multiplos :: Int -> Int -> Int -> Int
multiplos x y z
  | y > z          = 0
  | y `mod` x == 0 = 1 + multiplos x (y+1) z 
  | otherwise      = multiplos x (y+1) z 



-----------------------------------------------------------------------------ex 10


lastDigit :: Int -> Int 
lastDigit x = x `mod` 10



-----------------------------------------------------------------------------ex 11

percorre::Int->String->Int
percorre 0 (a:_) = digitToInt a 
percorre x (a:b) = percorre (x-1) b
percorre _ _ = -1

anyDigit::Int->Int->Int
anyDigit x y = percorre x (show y) 

     
    



-----------------------------------------------------------------------------ex 12

allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p)

-- o erro é que mesmo m sendo diferente de n e n diferente de p, os elementos m e p ainda podem ser iguais

-- maneira correta:

allDifferent2::Int->Int->Int->Bool
allDifferent2 m n p = (m/=n) && (n/=p) && (m/=p)




-----------------------------------------------------------------------------ex 13

howManyEqual::Int->Int->Int->Int
howManyEqual x y z 
  | x == y && y == z = 3
  | x == y && y /= z = 2
  | x == z && z /= y = 2
  | y == z && x /= y = 2
  | otherwise        = 0



----------------------------------------------------------------------------
--ex 14 | prog. funcional
sales :: Int -> Int
sales x
      | x == 0    = 12
      | x == 1    = 20
      | x == 2    = 18
      | x == 3    = 25
      | otherwise = 0


-- letra a
howManyLess :: Int -> Int -> Int -> Int
howManyLess x y z 
            | y > z       = 0
            | x > sales y = 1 + howManyLess x (y+1) z
            | otherwise   = howManyLess x (y+1) z

-- letra b
-- noZeroInPeriod :: Int -> Bool 






-----------------------------------------------------------------------------ex 15

fib :: Int->Int
fib 0 = 0
fib 1 = 1
fib x 
  | x >= 2    = fib(x-1) + fib(x-2)
  | otherwise = -1


antFib2 :: Int->Int->Int
antFib2 x y 
    |fib y == x = y
    |fib y < x = antFib2 x (y+1)
    |otherwise = -1

antFib:: Int->Int
antFib x = antFib2 x 0 




-----------------------------------------------------------------------------ex 16

funny :: Int->Int->Int->Bool 
funny x y z = x > z || y < x 




----------------------------------------------------------------------------
--ex 17 | prog. funcional
intervalo::Int
intervalo = ord 'A' - ord 'a'


converte::Char->Char
converte x 
  | 'a' <= x && x <= 'z' = chr (ord x - intervalo)
  | otherwise = x    





--------------------------------------------------------------------ex 18
bias :: Int
bias = ord '0'

charToNum::Char->Int
charToNum x 
  | '0' <= x && x <= '9' = ord x - bias
  | otherwise = -1





--------------------------------------------------------------------ex 19
duplicate::String->Int->String
duplicate _ 0 = ""
duplicate s n = s ++ duplicate s (n-1)




--------------------------------------------------------------------ex 20

tamanho :: String -> Int
tamanho [] = 0
tamanho (a:b) = 1 + tamanho b

simbolo :: Int->String
simbolo 0 = []
simbolo x = ">" ++ simbolo (x-1)

pushRight::String->Int->String
pushRight s n  = simbolo (n-tamanho s)++s




--------------------------------------------------------------------ex 21
-- x &- y = x-2*y











--------------------------------------------------------------------ex 22
inverte :: [Int] -> [Int]
inverte [] = []
inverte (a:b) = inverte b ++ [a]



--------------------------------------------------------------------ex 23
separa :: [Int] -> ([Int],[Int])
separa [] = ([],[])
separa x = (impares x, pares x)
  

-- pega só impares de uma lista
impares :: [Int] -> [Int]
impares [] = []
impares (a:x)
   | a `mod` 2 == 0 = impares x
   | otherwise      = [a] ++ impares x


-- pega só pares de uma lista
pares :: [Int] -> [Int]
pares [] = []
pares (a:x)
   | a `mod` 2 /= 0 = pares x
   | otherwise      = [a] ++ pares x


--------------------------------------------------------------------ex 26
conta :: Char->String->Int
conta _ [] = 0
conta c (a:b) 
  | c == a = 1 + conta c b
  |otherwise  = conta c b







--------------------------------------------------------------------ex 27
pertence :: Int->[Int]->Bool
pertence _ [] = False
pertence a (b:c) 
  | a == b = True
  | otherwise = pertence a c

purifica::[Int]->[Int]
purifica [] = []
purifica (a:x) 
   | pertence a x = purifica x 
   | otherwise = a : purifica x





--------------------------------------------------------------------ex 28

criaListaInt::Int->Int->[Int]
criaListaInt _ 0 = []
criaListaInt x y = [x] ++ criaListaInt x (y-1)

cria::Int->[Int]
cria x = criaListaInt x x

proliferaInt::[Int]->[Int]
proliferaInt [] = []
proliferaInt (a:x) = cria a ++ proliferaInt x






--------------------------------------------------------------------ex 29

criaListaChar::Char->Int->String
criaListaChar _ 0 = []
criaListaChar x y = [x] ++ criaListaChar x (y-1)

cria2::Char->String
cria2 x = criaListaChar x (ord x - ord 'A')

proliferaChar::String->String
proliferaChar [] = []
proliferaChar (a:x) = cria2 a ++ proliferaChar x





