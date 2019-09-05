import Data.List
--data Ponto = Int Int
data Ponto = Pont {x :: Int, y :: Int}
--data Retangulo = Pontos Ponto Ponto
data Retangulo = Retangulo{ p1 :: Ponto, p2 :: Ponto}

dentro :: Retangulo -> [Ponto] -> [[Ponto]]
dentro r p = [listaD(r p), p \\ listaD(r p)]

listaD :: Retangulo -> [Ponto] -> [Ponto]
listaD r (p:l)
    | dentroX(r p) && dentroY(r p) = [p] ++ listaD(l)

dentroX :: Retangulo -> Ponto -> Bool
--essa funcao deveria retornar verificar se o ponto esta dentro do retangulo porem nao conseguimos
--acessar o dado dentro da estrutura data
dentroX r  (xp _)   
    | ((x p) >= ((x (p1 r))) = if (x p <= x (p2 r)) then True else False
-- essa é outra maneira tentada porem nao funcionava tambem com error de parse no "="
--dentroX ((x1 _) (x2 _)) (xp _)
--    | (xp >= x1) = if (xp <= x2) then True else False
    | otherwise = if (xp >= x2) then True else False

dentroY :: Retangulo -> Ponto -> Bool
dentroY (_ y1) (_ y2)) (_ yp) 
    | (yp >= y1) = if (yp <= y2) then True else False
    | otherwise = if (yp >= y2) then True else False

main = do 
	putStrLn "Hello World"