factorial :: Int -> Int
factorial (0) = 1 
factorial n = n * factorial (n-1)

esPar :: Int -> Bool 
esPar (0) = True
esPar (1) = False
esPar n = esPar(n-2) 

fibonacci :: Integer  -> Integer 
fibonacci(0) = 0
fibonacci(1) = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

parteEntera :: Float -> Integer 
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n - 1) + 1

multi3:: Int -> Bool 
multi3 n | n == 3 || n == 0 = True
         | n < 3 = False 
         | otherwise = multi3(n - 3)


iesimoInpar :: Int -> Int
iesimoInpar n = 2 * n - 1

sumaImpares :: Int -> Int
sumaImpares (1) = 1
sumaImpares n = iesimoInpar n + sumaImpares(n - 1)

--medioFact :: Integer -> Integer

ultimo :: Int -> Int
ultimo n = mod n 10

comienzo :: Int -> Int 
comienzo n = div n 10

sumaDig :: Int -> Int 
sumaDig n | n == 0 = 0
          | otherwise = sumaDig(comienzo n) + ultimo n

--DigIgual :: Int -> Int