{- 1. Escreva uma fun¸c˜ao f :: [Int] −> [Int] que retorna uma lista contendo todos os elementos da lista
dada como argumento que ocorrem duas vezes em sucess˜ao. Caso o elemento ocorra n vezes em
sucess˜ao (n ≥ 2), o elemento surgir´a n − 1 vezes em sucess˜ao na lista dada como resultado. N˜ao ´e
necess´ario definir o valor para lista vazia.
Exemplos: f [1,2,2,3] = [2] f [1,2,2,2,2,1] = [2,2,2] f [3,2] = []-}
{-(a) Apresente uma solu¸c˜ao que pode utilizar fun¸c˜oes b´asicas e recurs˜ao, mas sem compreens˜ao de
lista-}
fa :: [Int] -> [Int]
fa (x:[]) = []
fa (x:y:xs)
    | x == y = [y] ++ fa (y:xs)
    |otherwise = fa(y:xs)
{-(b) Apresente uma solu¸c˜ao com compreens˜ao de lista, mas que n˜ao tenha recurs˜ao-}
fb :: [Int] -> [Int]
fb (a:as) = [x| (x,y) <- zip(a:as) as, x==y]

{-2. Defina uma fun¸c˜ao g ::[ Int] −> Bool que verifica que todo elemento de uma lista que 
est´a entre 10 e 100 (inclusive) ´e par. Utilize as fun¸c oes map, filter e foldr.-}
-- Exemplos: g [1,26,153,72,68,9] = True g [1,12,153,73,9] = False g [] = True g [1,255] = True
g:: [Int] -> Bool
g list = foldr (&&) True (map (\x -> x `mod` 2 == 0) (filter (\x -> x > 10 && x < 100) list))

{-3. Uma lˆampada ´e caracterizada por ser compacta ou incandescente. Al´em, disso, toda lˆampada possui
o nome do seu fabricante e a potˆencia como um valor em Watts.-}
{- (a) Defina o tipo alg´ebrico Lampada, de acordo com as caracter´ısticas descritas -}
type Fabricante = String
type Potencia = Float
data Lampada = Lampada Tipo Fabricante Potencia
data Tipo = Compacta | Incandescente deriving (Show)
{-(b) Estabele¸ca que exibir uma lˆampada resulta em uma string que come¸ca com a palavra “Compacta”,
no caso de lˆampada compacta, ou com a string “Incandescente”. Estas strings s˜ao
seguidas do nome do fabricante e da potˆencia da lˆampada. Ou seja, defina que o tipo Lampada
´e instˆancia da classe Show.-}
instance Show Lampada where
    show (Lampada t f p) = (show t) ++ " " ++ f ++ " " ++ (show p) 
{-(c) Estabele¸ca que o tipo Lampada ´e uma instˆancia da classe Eq, de modo que duas lˆampadas
s˜ao iguais se forem compactas e possu´ırem o mesmo fabricante e potˆencia. O mesmo vale para
lˆampadas incandescentes. -}
instance Eq Lampada where
    (==) (Lampada Compacta f1 p1) (Lampada Compacta f2 p2) = f1 == f2 && p1 == p2
    (==) (Lampada Incandescente f1 p1) (Lampada Incandescente f2 p2) = f1 == f2 && p1 == p2
    (==) (Lampada _ f1 p1) (Lampada _ f2 p2) = False

{-
4. Um lustre ´e constitu´ıdo por pendentes, fios e barras. Em cada extremidade de uma barra ´e preso
um fio, no qual pode estar pendurada uma lˆampada ou uma nova barra. Assim, um lustre pode ter
apenas um pendente com uma lˆampada ou v´arias barras com lustres. A potˆencia de um lustre ´e igua
`a soma das potˆencias das lˆampadas que est˜ao nele.
(a) Defina o tipo alg´ebrico Lustre.
(b) Defina uma fun¸c˜ao para calcular a potˆencia de um lustre (a soma das potˆencias das lˆampadas
do lustre)
(c) Defina uma fun¸c˜ao que determina se um lustre est´a balanceado. Um lustre com um lˆampada
apenas ´e considerado balanceado ou se as pontˆencias das lˆampadas dos lustres nas extremidades
s˜ao iguais e estes lustres est˜ao balanceados.
-}