type Pessoa = String
type Livro = String
type BancoDados = [(Pessoa, Livro)]
 
livrosCompressao :: BancoDados -> Pessoa -> [Livro]
livrosCompressao banco p = [livro | (pessoa, livro) <- banco, pessoa == p]

livros :: BancoDados -> Pessoa -> [Livro]
livros [] p = []
livros (x:xs) p
    | fst x == p = snd x : livros xs p
    | otherwise = livros xs p 

baseExemplo :: BancoDados
baseExemplo = [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), ("Fernando","Introduction to Programming with Python"), ("Fernando","Programming in Haskell")]

emprestimosCompressao :: BancoDados -> Livro -> [Pessoa]
emprestimosCompressao banco l = [pessoa | (pessoa, livro) <- banco, livro == l]

emprestimos :: BancoDados -> Livro -> [Pessoa]
emprestimos [] l = []
emprestimos (x:xs) l
    | snd x == l = fst x : emprestimos xs l
    | otherwise = emprestimos xs l

emprestado :: BancoDados -> Livro -> Bool
emprestado [] livro = False
emprestado (x:xs) livro
    | snd x == livro = True
    | otherwise = emprestado xs livro

emprestadoLista :: BancoDados -> Livro -> [Bool]
emprestadoLista banco l = [True | (pessoa, livro) <- banco, livro == l]

emprestadoCompressao :: BancoDados -> Livro -> Bool
emprestadoCompressao banco l = head (emprestadoLista banco l)

emprestadoCompressao2 :: BancoDados -> Livro -> Bool
emprestadoCompressao2 banco l = foldr (||) False (emprestadoLista banco l)
-- [("Joao","Software Abstractions"), ("Andre","Programming in Haskell"), ("Fernando","Introduction to Programming with Python"), ("Fernando","Programming in Haskell")] "Programming in Haskell"

qtdeEmprestimos :: BancoDados -> Pessoa -> Int
qtdeEmprestimos [] p = 0
qtdeEmprestimos (b:bs) p
    | fst b == p = 1 + qtdeEmprestimos bs p
    | otherwise = qtdeEmprestimos bs p

qtdeEmprestimosCompressao :: BancoDados -> Pessoa -> Int
qtdeEmprestimosCompressao banco p = length [1 | (pessoa, livro) <- banco, pessoa == p]

emprestar :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar [] p l = [(p, l)]
emprestar (b:bs) p l = b: emprestar bs p l

emprestar2 :: BancoDados -> Pessoa -> Livro -> BancoDados
emprestar2 list p l = [(p,l)] ++ list

devolver :: BancoDados -> Pessoa -> Livro -> BancoDados
devolver [] p l = []
devolver (b:bs) p l
    | fst b == p && snd b == l = devolver bs p l
    | otherwise = b: devolver bs p l

devolverCompressao :: BancoDados -> Pessoa -> Livro -> BancoDados
devolverCompressao b p l = [(pessoa, livro) | (pessoa, livro) <-b, p /= pessoa || l /= livro]
