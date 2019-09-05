fritz :: Int -> Int
fritz n 
    | (n == 0) = 0
    | (n == 1) = 1
    | otherwise = (quot (fritz(n-1)) 2) + 3 * (fritz (n-2))

main = do
    n <- getLine
    let n1 = (read n :: Int)
    putStrLn (show(fritz n1))