module Main (
    main
) where

import qualified Day05

main :: IO ()
main = do
    printOneDay 5 Day05.part1 Day05.part2

printOneDay :: Int -> IO () -> IO () -> IO ()
printOneDay n part1 part2 = do
    putStr ("Day " ++ show n)
    putStr ": Part 1 answer = "
    part1
    putStr "; Part 2 answer = "
    part2
    putStrLn ""
