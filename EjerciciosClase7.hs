--- clase 7
type Set a = [a]

vacio :: Set Int
vacio = []

agregar :: Int -> Set Int -> Set Int
agregar n m | m == vacio = [n]
            | elem n m = m
            | otherwise = n : m


incluido :: Set Int -> Set Int -> Bool
incluido c m | m == vacio && c == vacio = True
                  | m == vacio = False
                  | c == vacio = True
                  | not (elem (head c) m) = False
                  | otherwise = incluido (tail c) m 
                
iguales :: Set Int -> Set Int -> Bool
iguales c m | incluido c m  && incluido m c = True
            | otherwise = False
               
agregarATodos :: Int -> Set(Set Int) -> Set(Set Int)
agregarATodos n m | m == [] = []
                  | otherwise = agregar n (head m) : agregarATodos (n) (tail m)

agregarC :: Set Int -> Set (Set Int) -> Set (Set Int)
agregarC e [] = [e]
agregarC e c | elem e c = c
             | otherwise = e : c

perteneceC :: Set Int -> Set (Set Int) -> Bool
perteneceC e [] = False
perteneceC e (c:cs) = iguales e c || elem e cs 


union :: Set(Set Int) -> Set(Set Int) -> Set(Set Int)
union [] m = m
union (n:nx) m | elem n m == True = union nx m
               | otherwise = n : union nx m 

partes :: Int -> Set(Set Int)
partes 0 = [[]]
partes n = union (partes (n-1)) (agregarATodos n (partes (n-1)))

--falta producto cartesiano 

