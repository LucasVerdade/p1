gerar :: Int -> [Int]
gerar n = [1..n]

mult2 :: [Int] -> [Int]
mult2 [x]   = if (mod x 2) == 0 then [x] else []
mult2 (a:b)
    | (a `mod` 2) == 0 = a:(mult2 b)
    | otherwise        = mult2 b

mult5 :: [Int] -> [Int]
mult5 [x]   = if (mod x 5) == 0 then [] else [x]
mult5 (a:b)
    | (a `mod` 5) == 0 = mult5 b
    | otherwise        = a:(mult5 b)

soma :: [Int] -> Int
soma [x] = x
soma (a:b) = a + soma b

main = do
    n <- getLine
    let n1 = (read n :: Int)
    putStrLn(show (soma(mult5(mult2(gerar n1)))))