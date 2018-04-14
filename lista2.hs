{-double :: [Int] -> [Int] dobra os elementos de uma lista -}
double :: [Int] -> [Int]
double xs = [x*2 | x <- xs]

{- member :: [Int] -> Int -> Bool verifica se um inteiro dado como argumento está na lista -}
member :: [Int] -> Int -> Bool
member [] y = False
member x y
    | head x == y = True
    | head x /= y = member (tail x) y 

{- digits :: String -> String resulta em uma lista que contém apenas os dígitos da lista dada como argumento -}
digits :: String -> String
digits s = [x | x <- s, x `elem` ['0'..'9']] 

{- sumPairs :: [(Int,Int)]->[Int] resulta em uma lista com a soma dos pares da lista dada como argumento -}
sumPairs :: [(Int,Int)] -> [Int]
sumPairs xs = [ x + y | (x,y) <- xs ]

-- Quicksort 
quicksort2 :: [Int] -> [Int]
quicksort2 [] = []
quicksort2 (a:as) = quicksort2 (filter (< a) as) ++ [a] ++ quicksort2 (filter (>= a) as)