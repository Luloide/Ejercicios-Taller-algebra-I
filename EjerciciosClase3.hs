{-ejercicios hechos en clase-}
factorial :: Int -> Int
factorial (0) = 1
factorial n = n * factorial (n-1)

esPar :: Int -> Bool
esPar (0) = True
esPar (1) = False
esPar n = esPar(n-2)
{-ejercicio 1 mirar el ppt-}
fibonacci :: Integer  -> Integer
fibonacci(0) = 0
fibonacci(1) = 1
fibonacci n = fibonacci(n - 1) + fibonacci(n - 2)

{-Implementar una funcion parteEntera :: Float -> Integer que calcule la parte entera de un numero real positivo.-}
parteEntera :: Float -> Integer
parteEntera n | n < 1 = 0
              | otherwise = parteEntera(n - 1) + 1

{-Escribir una funcion para determinar si un numero natural es multiplo de 3. No esta permitido utilizar mod ni div.-}
multi3:: Int -> Bool
multi3 n | n == 3 || n == 0 = True
         | n < 3 = False
         | otherwise = multi3(n - 3)

{-Implementar la funcion sumaImpares :: Int -> Int que dado n ∈ N sume los primeros n numeros impares.-}
iesimoInpar :: Int -> Int
iesimoInpar n = 2 * n - 1

sumaImpares :: Int -> Int
sumaImpares (1) = 1
sumaImpares n = iesimoInpar n + sumaImpares(n - 1)
{-Escribir una funcion medioFact que dado n ∈ N calcula n!! = n(n−2)(n−4)-}
medioFact :: Integer -> Integer
medioFact n | n <= 0 = 1
            | otherwise = n * medioFact(n-2)

{-Escribir una funcion que determine la suma de digitos de un nu ́mero positivo.
Para esta funci ́on pueden utilizar div y mod. -}
ultimo :: Int -> Int
ultimo n = mod n 10

comienzo :: Int -> Int
comienzo n = div n 10

sumaDig :: Int -> Int
sumaDig n | n == 0 = 0
          | otherwise = sumaDig(comienzo n) + ultimo n


{-Implementar una funcion que determine si todos los dıgitos de un numero son iguales.-}
--reutilizo ultimo y comienzo definidos anteriormente
penultimo :: Int -> Int
penultimo n = div (mod n 100) 10

sonIguales :: Int -> Bool
sonIguales n | penultimo n == ultimo n = True
             | otherwise = False


digIgual :: Int -> Bool
digIgual n | sonIguales n == False = False 
           | n == 0 = True
           | otherwise = digIgual(comienzo n)