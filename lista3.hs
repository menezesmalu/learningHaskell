-- Defina uma função que, dados dois números x e y, retorne como resultado o m.d.c. de x e y.
mdc :: Int -> Int -> Int
mdc x 0 = x
mdc x y = mdc y (x `mod` y)
-- Defina uma função que, dado um número inteiro positivo x, verifique se x é primo ou não. 
-- Lembre-se de utilizar o crivo de Eratóstenes por razões de otimização.

crivo :: Int ->  [Int] -> [Int]
crivo lim [] = []
crivo lim (x:xs) 
    | x <= floor (sqrt (fromIntegral lim)) = crivo lim [a | a<-xs, a `mod` x /= 0]
    | otherwise = (x:xs)
    

ehPrimo :: Int -> Bool
ehPrimo x = x `elem` crivo x [2..x]

{-
Dados dois pontos num espaço tridimensional, defina uma função distancia e um tipo 
Ponto de tal forma que a função calcule a distância entre dois pontos passados como parâmetros. 
A função tem tipo Ponto -> Ponto -> Double.
> distancia (1.0,2.0,3.0) (2.0,3.0,4.0)
1.7320508075688772
-}
data Ponto = Ponto (Double, Double, Double)

distancia :: Ponto -> Ponto -> Double
distancia (Ponto (x1,y1,z1)) (Ponto (x2,y2,z2)) = sqrt ((x1-x2)*(x1-x2) + (y1-y2)*(y1-y2) + (z1-z2)*(z1-z2))

--Usando compreensão de lista, defina uma expressão que calcule a soma 12+22+...+1002 
--dos quadrados dos cem primeiros inteiros positivos

quadrados = [x*x| x <- [1..100]]
sumList = [2 + (y*10) | y <- [1..100]]
sumsqrt = sum [x+y| x<- quadrados, y <- sumList]

{-
Suponha que uma grade de coordenadas de tamanho m x n is dada por uma lista de todos 
os pares (x,y) tal que 0⩽x⩽m e 0⩽y⩽n. Usando compreensão de lista, defina a função 
grid:: Int -> Int -> [(Int, Int)] que retorna uma grade de um dado tamanho. Por exemplo,
> grid 1 2
[ (0,0) , (0,1), (0,2), (1,0), (1,1), (1,2)]
-}

grid :: Int -> Int -> [(Int, Int)]
grid m n = [(a, b)| a<- [0..m], b <- [0..n] ]
{-
Usando compreensão de lista e a função grid definida acima, defina a função 
square :: Int -> [(Int, Int)] que retorna as coordenadas do quadrado de tamanho n, 
excluindo a diagonal de (0,0) a (n,n). Por exemplo,
> square 2
[(0,1), (0,2), (1,0), (1,2), (2,0), (2,1)]
-}

square :: Int -> [(Int, Int)]
square a = [(x, y) | (x, y)<- grid a a, x /= y]

{-Defina a função recursiva merge :: Ord a => [a] -> [a] -> [a] que mescla duas
listas ordenadas em uma única lista ordenada
> merge [2,5,6] [1,3,4]
-- poderia ter só juntado e dado um quick mas achei assim melhor
[1,2,3,4,5,6]-}
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (a:as) =
    quicksort [y| y<- as, y < a] ++ [a] ++ quicksort [y | y <- as, y >= a]

merge :: Ord a => [a] -> [a] -> [a] 
merge fst scd =quicksort (fst ++ scd) 


{-
Defina e implemente a função aplicaFuncoes :: [Int->Int] -> [Int] -> [[Int]]que recebe uma
lista de funções unárias e uma lista de valores e retorna uma lista de listas na qual cada
lista contém a lista de valores recebidos na entrada aplicada a uma das funções.
aplicaFuncoes [(\x -> x + 2), (\x -> x - 2)] [2,3,4,5,0]
[[4,5,6,7,2],[0,1,2,3,-2]]
-}
aplicaFuncoes :: [Int->Int] -> [Int] -> [[Int]]
aplicaFuncoes func list = [map f list  | f<-func]