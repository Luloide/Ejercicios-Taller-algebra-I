{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
import GHC.Base (RuntimeRep(TupleRep))
{- 
1 absoluto: calcula el valor absoluto de un numero entero.
2 maximoabsoluto: devuelve el maximo entre el valor absoluto de dos numeros enteros.
3 maximo3: devuelve el maximo entre tres numeros enteros.
4 algunoEs0: dados dos numeros racionales, decide si alguno de los dos es igual a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).
5 ambosSon0: dados dos numeros racionales, decide si ambos son iguales a 0 (hacerlo dos veces, una sin usar y otra usando pattern matching).
6 esMultiploDe: dados dos numeros naturales, decidir si el primero es multiplo del segundo.
7 digitoUnidades: dado un numero natural, extrae su dıgito de las unidades.
8 digitoDecenas: dado un numero natural, extrae su dıgito de las decenas.
-}

absoluto :: Int -> Int
absoluto x | x >= 0 = x
           | x < 0 = -x

maximoAbsoluto :: Int -> Int -> Int
maximoAbsoluto x y | absoluto x >= absoluto y = absoluto x
                   | otherwise = absoluto y

maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | otherwise = z

{-algunoEs0 sin pattern matching-}
algunoEs0 :: Float -> Float -> Bool 
algunoEs0 x y | x == 0 = True 
              | y == 0 = True
              | otherwise = False

{-algunoEs0 con pattern matching-}
algunoEs0_2 :: Float -> Float -> Bool 
algunoEs0_2 _ 0 = True
algunoEs0_2 0 _ = True 
algunoEs0_2 _ _ = False

{-ambosSon0 sin pattern matching-}
ambosSon0 :: Float -> Float -> Bool 
ambosSon0 x y | x == 0 && y == 0 = True
              | otherwise = False

{-ambosSon0 con pattern matching-}
ambosSon0_2 :: Float -> Float -> Bool 
ambosSon0_2 0 0 = True 
ambosSon0_2 x y = False 

{- en esMultiplo funciona asi: x es multiplo de y -}
esMultiploDe :: Int -> Int -> Bool 
esMultiploDe x y | (mod x y) == 0 = True
                 | otherwise = False
    