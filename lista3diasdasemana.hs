-- Implemente um tipo algébrico DiasSemana, começando do Domingo e indo até Sábado, 
--instancie ele para Enum, Show, Ord e Eq e crie 3 funções: 
data DiasSemana = Dom | Seg | Ter | Qua | Qui | Sex | Sab deriving (Show, Ord, Eq)

--Dada uma lista de dias da semana retorna os dias úteis ordenados;
ordenaUteis :: [DiasSemana] -> [DiasSemana]
ordenaUteis [] = []
ordenaUteis (d:ds)
    | Seg <= d && d <= Sex =
         ordenaUteis [y| y<- ds, y < d] ++ [d] ++ ordenaUteis [y | y <- ds, y >= d]
    | otherwise = ordenaUteis ds

--Dada uma lista de tuplas (Dia da semana, data) e um dia da semana, 
--retorna uma lista com somente as datas que sejam do mesmo dia da semana;
datasIguais :: [(DiasSemana,Int)] -> DiasSemana -> [Int]
datasIguais [] x = []
datasIguais ((a,b):ds) x
    | a == x = [b] ++ datasIguais ds x
    | otherwise = datasIguais ds x

--Recebe um dia da semana (referente ao primerio dia do mes) e retorna uma 
--lista de tuplas com todos os dias do mês. Considere o mês como tendo 30 dias.
suc :: DiasSemana -> DiasSemana
suc Dom = Seg
suc Seg = Ter
suc Ter = Qua
suc Qua = Qui 
suc Qui = Sex 
suc Sex = Sab
suc Sab = Dom

imprimeMesSeq:: DiasSemana -> Int -> [(Int, DiasSemana)]
imprimeMesSeq x y 
    | y <= 30 =  [(y, x)] ++ imprimeMesSeq (suc x) (y+1)
    | otherwise = []

imprimeMes :: DiasSemana -> [(Int, DiasSemana)]
imprimeMes x =  imprimeMesSeq x 1
