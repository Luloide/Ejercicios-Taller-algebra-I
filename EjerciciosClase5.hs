sumaDivisoresHasta :: Int -> Int -> Int
sumaDivisoresHasta n k | k == 1 = 1
                       | mod n k == 0 = k + sumaDivisoresHasta n (k-1)
                       | otherwise = sumaDivisoresHasta n (k-1)

sumaDivisores :: Int -> Int
sumaDivisores n = sumaDivisoresHasta n n

sumaDivisoresDesde :: Int -> Int -> Int
sumaDivisoresDesde n k | k == n = n
                       | mod n k == 0 = k + sumaDivisoresDesde n (k+1)
                       | otherwise = sumaDivisoresDesde n (k+1)

sumaDivisores' :: Int -> Int
sumaDivisores' n = sumaDivisoresDesde n 1


menorDivisorDesde :: Int -> Int -> Int
menorDivisorDesde n k | mod n k == 0 = k
                      | otherwise = menorDivisorDesde n (k+1) 

menorDivisor :: Int -> Int
menorDivisor n = menorDivisorDesde n 2

esPrimo :: Int -> Bool
esPrimo n | n == 1 = False 
          | menorDivisor n == n = True
          | otherwise = False

minimoPrimoDesde :: Int -> Int
minimoPrimoDesde n | esPrimo n == True = n
                   | otherwise = minimoPrimoDesde (n+1)

nEsimoPrimo :: Int -> Int
nEsimoPrimo n | n == 1 = 2
              | otherwise = minimoPrimoDesde (1 + nEsimoPrimo (n-1)) 

-- Implementar menorFactDesde :: Int -> Int que dado m >= 1
-- encuentra el mínimo n >= m, tal que n = k! para algún k

fact :: Int -> Int
fact n | n == 0 = 1
       | n > 0 = n * fact (n-1)

auxMenorFactDesde :: Int -> Int -> Int
auxMenorFactDesde n l | fact l >= n = fact l
                      | otherwise = auxMenorFactDesde n (l + 1)
                      
menorFactDesde :: Int -> Int                     
menorFactDesde n = auxMenorFactDesde n 1

auxMayorFactDesde :: Int -> Int -> Int
auxMayorFactDesde n l | fact l > n = fact (l - 1)
                      | otherwise = auxMayorFactDesde n (l + 1)

mayorFactDesde :: Int -> Int
mayorFactDesde n = auxMayorFactDesde n 1
           
esFact :: Int -> Bool
esFact n = menorFactDesde n == mayorFactDesde n
                      
