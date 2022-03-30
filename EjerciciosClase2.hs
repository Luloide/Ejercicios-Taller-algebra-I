import Data.Time (picosecondsToDiffTime)
{-
estanRelacionados: dados dos nu ́meros reales, decide si est ́an relacionados considerando la relaci ́on de equivalencia en R cuyas clases de equivalencia son:
(−∞, 3], (3, 7] y (7, ∞).
2 prodInt: calcula el producto interno entre dos vectores de R2.
3 todoMenor: dados dos vectores de R2, decide si es cierto que cada coordenada del primer vector es menor a la coordenada correspondiente del segundo vector.
4 distanciaPuntos: calcula la distancia entre dos puntos de R2.
5 sumaTerna: dada una terna de enteros, calcula la suma de sus tres elementos.
6 posicPrimerPar: dada una terna de enteros, devuelve la posicion del primer numero par si es que hay alguno, y devuelve 4 si son todos impares.
7 crearPar :: a -> b -> (a, b): crea un par a partir de sus dos componentes dadas por separado (debe funcionar para elementos de cualquier tipo).
8 invertir :: (a, b) -> (b, a): invierte los elementos del par pasado como par ́ametro (debe funcionar para elementos de cualquier tipo).
-}


estanRelacionados :: Float -> Float -> Bool
estanRelacionados x y | x <= 3 && y <= 3 = True
                      | (x <=7 && x > 3) && ( y<=7 && y > 3) = True
                      | x > 7 && y > 7 = True 
                      | otherwise = False

prodInt :: (Float, Float) -> (Float, Float) -> Float
prodInt (vx, vy) (wx, wy) = vx * wx + vy * wy

todoMenor :: (Float, Float) -> (Float, Float) -> Bool 
todoMenor (vx, vy) (wx, wy) | (vx < wx) && (vy < wy) = True
                            | otherwise = False

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (vx, vy) (wx, wy) = sqrt ( ((vx - wx) ** 2) + ((vy - wy) ** 2))

sumaTerna :: (Int,Int,Int) -> Int 
sumaTerna (x, y, z) = x + y + z

posicPrimerPar :: (Int,Int,Int) -> Int 
posicPrimerPar (x, y, z) | esPar x = 1
                         | esPar y = 2
                         | esPar z = 3
                         | otherwise = 4
                         where esPar n = mod n 2 == 0

crearPar :: a -> b -> (a, b)
crearPar x y = (x,y)

invertir :: (a, b) -> (b, a)
invertir (x, y) = (y, x)