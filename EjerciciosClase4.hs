--Clase 4 
f1 :: Int -> Int
f1 0 = 0
f1 n = 2^n + f1 (n-1)

f2 :: Int -> Float -> Float
f2 0 _ = 0
f2 n q  = q^n + f2 (n - 1) q

f3 :: Int -> Float -> Float
f3 0 _ = q^(2*n) + q(2n-1) - q^(n-1) + (f4 (n-1) q)

factorial :: Int -> Int 
factorial 0 = 1
factorial n = n * factorial (n-1)

eAprox :: Int -> Float
eAprox 0 = 1
eAprox n = 1/(fromIntegral (factorial n)) + eAprox (n-1)

sumatoria :: Int -> Int -> Int
sumatoria 0 _ = 0
sumatoria n q = q^n + sumatoria (n-1) q

sumDoble :: Int -> Int -> Int 
sumDoble 1 m = sumatoria m 1
sumDoble n m = sumatoria m n + sumDoble (n-1) m 

-- no terminado 
sumatoriaDePotencias :: Int -> Int -> Int -> Int
sumatoriaDePotencias 0 _ m = 0
sumatoriaDePotencias q n m = q^(n + m) + sumatoriaDePotencias q (n-1) m

sumaPotencias :: Int -> Int -> Int -> Int
sumaPotencias q n m | q == 1 = 1
                    | n == 1 || m == 1 = q
                    | 1 > n && m = q^(n + m) + sumaPotencias q (n-1) m 
