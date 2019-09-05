fritz :: Int -> Int
fritz n | (n == 0) = 0
        | (n == 1) = 1
        | otherwise = (fritz (n-1) ) /2 + 3 * (fritz (n-2)

main = do
    putStrLn (show(fritz 5))