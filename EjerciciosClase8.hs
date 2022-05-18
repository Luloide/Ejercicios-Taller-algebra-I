--- clase 8
--- funciones auxiliares utiles
type Set a = [a]

vacio :: Set a
vacio = []

agregar :: Eq a => a -> Set a -> Set a
agregar n m | m == vacio = [n]
            | elem n m = m
            | otherwise = n : m

union :: Set(Set Int) -> Set(Set Int) -> Set(Set Int)
union [] m = m
union (n:nx) m | elem n m == True = union nx m
               | otherwise = n : union nx m 

--ejecrcicios de clase
combinatorio :: Int -> Int -> Int
combinatorio n k | n == k = 1
                 | k == 0 = 1
                 | otherwise = combinatorio (n-1) k + combinatorio (n-1) (k-1)              

agregarElemAListas :: Set Int -> Set [Int] -> Set [Int]
agregarElemAListas (x:xs) m = union (agregarElemAListas xs m) (agregarElementoAdelante x m) 

agregarElementoAdelante :: Int -> Set [Int] -> Set[Int]
agregarElementoAdelante x [] = []
agregarElementoAdelante x (ys:yss) = agregar (x:ys) (agregarElementoAdelante x yss)



variaciones :: Set Int -> Int -> Set [Int]
variaciones c (0) = [[]]
variaciones c k = agregarElemAListas c (variaciones c (k-1))

insertarEn :: [Int] -> Int -> Int -> [Int]
insertarEn (x:xs) n 1 = (n:(x:xs))
insertarEn (x:xs) n i = x:(insertarEn xs n (i-1)) 
