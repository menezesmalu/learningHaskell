-- Implemente um tipo algébrico DiasSemana, começando do Domingo e indo até Sábado, 
--instancie ele para Enum, Show, Ord e Eq e crie 3 funções: 
data DiasSemana = Dom |Seg | Ter | Qua | Qui | Sex | Sab deriving (Show, Ord, Eq)
diasUteis = [Seg , Ter , Qua , Qui , Sex ]

--Dada uma lista de dias da semana retorna os dias úteis ordenados;
 ordenaUteis :: [DiasSemana] -> [DiasSemana]

--Dada uma lista de tuplas (Dia da semana, data) e um dia da semana, retorna uma lista com somente as datas que sejam do mesmo dia da semana;
datasIguais :: [(DiasSemana,Int)] -> DiasSemana -> [Int]
--Recebe um dia da semana (referente ao primerio dia do mes) e retorna uma lista de tuplas com todos os dias do mês. Considere o mês como tendo 30 dias.
imprimeMes :: DiasSemana -> [(Int, DiasSemana)]

-- Exemplo:
-- > ordenaUteis [Segunda, Segunda, Terca, Domingo, Quarta]
-- [Domingo, Segunda, Segunda, Terca, Quarta]
-- > datasIguais [(Sexta, 2),(Sabado, 3),(Quinta, 10),(Domingo, 25),(Quinta, 29)] Quinta
-- [10, 29]
-- > imprimeMes Terca
-- [(1, Terca), (2, Quarta), (3, Quinta), (4, Sexta), (5, Sabado), (6, Domingo), (7, Segunda), (8, Terca), (9, Quarta) etc...]