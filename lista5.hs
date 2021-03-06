{-Usando compreensão de lista, defina uma função que, dado um inteiro 
positivo n, retorna uma lista com os fatoriais de 1 até n-}
fatarray :: Int -> Int -> [Int] -> [Int]
fatarray goal now list  
    | now < goal = fatarray goal (now+1) (list ++ [last(list) * now])
    | otherwise = list ++ [last list * now]

fatorial :: Int -> [Int]
fatorial a = fatarray a 1 [1]
{-
A função testaLista ::( a −> Bool) −> [a] −> Bool retorna True caso os 
elementos da lista dada como argumento satisfaçam a função passada como
primeiro argumento; caso contrário, retorna False. 
Defina a função testaLista das seguintes maneiras:
-}

--Usando recursão
testaLista :: (a -> Bool) -> [a] -> Bool
testaLista f [] = True
testaLista f (x:xs)
    |f x = testaLista f xs
    |otherwise = False

    --Usando as funções map e and
testaListaMap :: (a -> Bool) -> [a] -> Bool
testaListaMap f list = and (map f list)

--Usando foldr
testaListaFoldr :: (a -> Bool) -> [a]-> Bool
testaListaFoldr f list = foldr (&&) True (map f list)
