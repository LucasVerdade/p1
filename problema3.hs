data Arvore = Null | No Int Arvore Arvore
--Questão A
maiorProfundidadeElemento :: Arvore -> Int -> Int
maiorProfundidadeElemento a x = profundidade a x (-1)

maior :: Int -> Int -> Int
maior x y 
    | (x > y) = x
    | otherwise = y 

profundidade :: Arvore -> Int -> Int -> Int
profundidade Null _ _ = -1
profundidade (No n esq dir) x p
    | (n == x) = p+1
    | otherwise = maior (profundidade esq x (p+1)) (profundidade dir x (p+1))
--Fim Questão A

--Questão B
folhasPrimos :: Arvore -> [Int]
folhasPrimos Null = []
folhasPrimos (No n Null Null) = if (primo n) then [n] else []
folhasPrimos (No n esq dir) = folhasPrimos(esq) ++ folhasPrimos(dir)

primo :: Int -> Bool
primo 1 = False
primo 2 = True
primo n | (length [x | x <- [2 .. n-1], mod n x == 0]) > 0 = False
    | otherwise = True
--Fim Questão B

--Questão C
elementosRepetidos :: Arvore -> Int -> Int
elementosRepetidos Null _ = 0
elementosRepetidos (No n esq dir) x
    | (n == x) = 1 + (elementosRepetidos esq x) + (elementosRepetidos dir x)
    | otherwise = (elementosRepetidos esq x) + (elementosRepetidos dir x)

--Fim Questão C

minhaArvore :: Arvore
minhaArvore = No 52 (No 3 (No 12 Null Null) (No 3 Null Null)) (No 11 (No 55 Null Null) (No 19 Null Null))

somaElementos :: Arvore -> Int
somaElementos Null = 0
somaElementos (No n esq dir) = n + (somaElementos esq) + (somaElementos dir)

buscaElemento :: Arvore -> Int -> Bool
buscaElemento Null _ = False
buscaElemento (No n esq dir) x 
    | (n == x) = True                           
    | otherwise = (buscaElemento esq x) || (buscaElemento dir x)

limiteSup :: Int
limiteSup = 1000 --Define um limite superior para o maior número

minimo :: Int -> Int -> Int
minimo x y | (x < y) = x
           | otherwise = y

minimoElemento :: Arvore -> Int
minimoElemento Null = limiteSup 
minimoElemento (No n esq dir) = 
    minimo n (minimo (minimoElemento esq) (minimoElemento dir))
                     
main = do 
    n <- getLine
    let n1 = (read n :: Int)
    --putStrLn (show (maiorProfundidadeElemento minhaArvore n1))
    --putStrLn (show (folhasPrimos minhaArvore))
    putStrLn (show (elementosRepetidos minhaArvore n1))
