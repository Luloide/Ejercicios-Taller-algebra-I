--clase 6 
-- sumatoria con listas
sumatoria :: [Int] -> Int 
sumatoria [] = 0
sumatoria xs = head xs + sumatoria (tail xs)

-- funcion para calclar la longitud de una lista
longitud :: [Int] -> Int
longitud [] = 0
longitud xs = 1 + longitud (tail xs)

-- funcion que indica si un elemento aparece en la lista o no
pertenece :: Int -> [Int] -> Bool
pertenece x [] = False 
pertenece x xs | x == head xs = True
               | otherwise = pertenece x (tail xs)

--indica el primer elemento de la lista que es múltiplo de 45345 que encuentre en la lista

primerMultiplode45345 :: [Int] -> Int
primerMultiplode45345 xs | xs == [] = undefined -- en el caso que no haya un numero multiplo de 45345 en la lista 
                         | mod (head xs) 45345  == 0 = head xs
                         | otherwise = primerMultiplode45345 (tail xs)

--sumatoria usando pattern matching
sumatoriaPM [] = 0
sumatoriaPM ( x : xs ) = sumatoriaPM xs + x

--longitud usando pattern matching
longitudPM :: [Int] -> Int
longitudPM [] = 0
longitudPM (x:xs) = 1 + longitudPM xs

-- pertenece usando pattern matching 
pertenecePM :: Int -> [Int] -> Bool
pertenecePM n [] = False 
pertenecePM n (x:xs) |  n == x = True
                     | otherwise = pertenecePM n xs
                     
-- ejercicios 
productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)                      

sumarN :: Int -> [Int] -> [Int]
sumarN n [] = []
sumarN n (x:xs) = (x+n) : (sumarN n xs)

sumarElPrimero :: [Int] -> [Int]
sumarElPrimero xs = sumarN (head xs) xs

--funcion auxiliar para sumar ultimo (esta funcion devuelve el ultimo número de una lista)
ultimo :: [Int] -> Int
ultimo xs | longitud (xs) == 1 = head xs
          | otherwise = ultimo (tail xs)


sumarElUltimo :: [Int] -> [Int]
sumarElUltimo xs = sumarN (ultimo xs) xs

--función auxiliar
esPar :: Int -> Bool
esPar (0) = True
esPar (1) = False
esPar n = esPar(n-2)

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | esPar x == True = x : (pares xs)
             | otherwise = (pares xs)

multiplosDeN :: Int -> [Int] -> [Int]
multiplosDeN n [] = []
multiplosDeN n xs | mod (head xs) n == 0 = (head xs) : multiplosDeN n (tail xs)
                  | mod (head xs) n /= 0 = multiplosDeN n (tail xs)

quitar :: Int -> [Int] -> [Int]
quitar n [] = []
quitar n xs | n == (head xs) = quitar n (tail xs)
            | otherwise = (head xs) : quitar n (tail xs)

hayRepetidos :: [Int] -> Bool
hayRepetidos xs | pertenece (head xs) (tail xs) == True = True
                | otherwise = False

-- no terminado 
{-
eliminarRepetidos :: [Int] -> [Int]
eliminarRepetidos xs | hayRepetidos xs == True = 
-}









