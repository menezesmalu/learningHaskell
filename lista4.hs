import Data.List.Split

{-Crie uma função de ordenação sort::(a->a->Bool)->[a]->[a] que recebe uma função de comparação
e uma lista, e ordena a lista de acordo com a função. 
> sort (>) [1,4,2,3,8,15,0]
[15,8,4,3,2,1,0]-}

sort :: (a -> a -> Bool) -> [a] -> [a]
sort func [] = []
sort func (x:xs) = sort func (filter (not . func x) xs)  ++ [x] ++ sort func (filter (func x) xs)

{-
Crie uma função agrupar :: Eq a => [a] -> [[a]] , que recebe uma 
lista e devolve uma lista de lista dos elementos iguais
Exemplo:
> agrupar "arara"
["aaa","rr"]
-}
deleteAll :: Eq a => [a] -> a -> [a]
deleteAll [] x = []
deleteAll (a:as) x = if (x == a)
                   then deleteAll as x
                   else (a:deleteAll as x)
seachAll :: Eq a => [a] -> a -> [a]
seachAll [] x = []
seachAll (a:as) x = if (x == a)
                   then (a:seachAll as x)
                   else seachAll as x
        
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = [[]]
agrupar (x:xs) = (( seachAll (x:xs) x ) : (   agrupar(   deleteAll xs x   )   ) ) 

type Lado = Float
type Triangulo = (Lado, Lado, Lado)

--   Implemente uma função que recebe uma lista de Triangulo e retorna a soma das 
--áreas dos respectivos triângulos. Use obrigatoriamente a:s funções foldr1 e map.
heron :: Triangulo -> Float
heron (a, b, c) = sqrt (( (a+b+c)/2)  * (((a+b+c)/2) - a)* (((a+b+c)/2) - b)*  (((a+b+c)/2) - c))

sumAreas :: [Triangulo] -> Float
sumAreas list = foldr1 (+) (map heron list)

{-
Implemente uma função functions :: [(a -> a -> a)] -> [a] -> [(a -> a)] que recebe 
uma lista de funções binárias e uma lista de valores. O funcionamento de functions é 
tal que aplica cada função a um respectivo elemento da lista de valores e retorna 
uma lista de funções parcialmente aplicadas. Dê alguns exemplos de entrada e respectiva
saída para functions.
-}
functions :: [(a -> a -> a)] -> [a] -> [(a -> a)]
functions [] _ = []
functions _ [] = []
functions (f:fs) (x:xs) = [(f x)] ++ functions fs xs 

{-
Considere uma lista de nomes completos de pessoas, escreva uma função que retorne a lista
 de nomes abreviados da seguinte forma:
> abrev ["Alberto Rodrigues Pontes", "Amanda Azevedo Mendes"]
["A. Mendes", "A. Pontes"]
-}
nome :: String -> String
nome (x:xs) = [x] ++ ". " ++  last( splitOn " " xs)


abrev :: [String] -> [String]
abrev names = map nome names 

{-
Depois de abreviados ordene a lista em ordem alfabética, utilize funções 
como map, fold e filter para resolver a questão.

FALTAAAA
-}