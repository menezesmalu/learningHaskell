import Data.Char

{- -1) "o ou-exclusivo entre x e y será True se x é True e y é False or x é False e y é True"-}
exOr :: Bool -> Bool -> Bool 
exOr x y
    | x == True && y == False = True
    | x == False && y == True = True
    | otherwise = False

{- 2) Usando literais no lado esquerdo, podemos escrever uma função para uma tabela-verdade como 
uma definição em Haskell. Complete a seguinte definição de ou-exclusivo neste estilo
 exOr True True = ...
 exOr True False = ...
...
-}

exOr2 :: Bool -> Bool -> Bool
exOr2 True True = False
exOr2 True False = True
exOr2 False True = True
exOr2 False False = False

{- 3)Apresente duas definições distintas da função nAnd :: Bool -> Bool -> Bool que retorna o resultado True exceto 
quando os dois argumentos são True. -}

nAnd :: Bool -> Bool -> Bool
nAnd True True = False
nAnd True False = True
nAnd False True = True
nAnd False False = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 True True = False
nAnd2 True False = True
nAnd2 False _ = True

nAnd3 :: Bool -> Bool -> Bool
nAnd3 True True = False
nAnd3 _ _ = True

{- 4) Defina uma função que converte letras minúsculas em maiúsculas e retorna caracteres não
modificadas no caso de maiúsculos -}

convChar :: Char -> Char
convChar c = toUpper c

convChar2 :: Char -> Char
convChar2 c
    | (fromEnum 'a' <= fromEnum c) && (fromEnum c <= fromEnum 'z') = toEnum ((fromEnum c - fromEnum 'a') + fromEnum 'A')
    | otherwise = c 
    
{- 5)Defina a função charToNum :: Char -> Int que converte um dígito como '8' no inteiro 8, por exemplo -}
charToInt :: Char -> Int
charToInt c = fromEnum c - fromEnum '0'
