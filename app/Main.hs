module Main (
    main
) where

import Day16 ( dayNo, part1, part2 )

main :: IO ()
main = do
    printOneDay dayNo part1 part2

printOneDay :: Int -> IO () -> IO () -> IO ()
printOneDay n part1 part2 = do
    putStr ("Day " ++ show n)
    putStr ": Part 1 answer = "
    part1
    putStr "; Part 2 answer = "
    part2
    putStrLn ""
