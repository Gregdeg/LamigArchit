module Main where

import Puzzle
import Printer
import Solver
import System.IO


main = do
                 putStrLn "Podaj nazwe pliku"
                 --fileName <- getLine
                 putStrLn "Wczytuje..."
                 fileContent <- readFile "puzzle1.txt"
                 let linesOfFile = lines fileContent
                 vCounts <- return (read (linesOfFile !! 0) :: [Int])
                 hCounts <- return (read (linesOfFile !! 1) :: [Int])
                 grid <- return (read (linesOfFile !! 2) :: [(Int,Int)])
                 let puzzle = Puzzle vCounts hCounts (convertInput (length vCounts) (length hCounts) grid)
                 --printUpperTab vCounts 5 stdout
                 --printPuzzle (setGasFields puzzle 5 5) hCounts 5 stdout
                 --print (getColumn (convertInput (length vCounts) (length hCounts) grid) 5 0)
                 --printPuzzle ((setEmptyFields puzzle 5 5)) 5 stdout
                 --printAll (solve (Puzzle vCounts hCounts (convertInput (length vCounts) (length hCounts) grid)) 5 5) 5  stdout
                 printAll (solve ((setEmptyFields (puzzle)  5 5)) 5 5) 5 stdout
                 --printAllToFile (puzzle) 5  "result2.txt"
                 --print (checkRowReady ([Empty,Gas,GasRight,House,Unknown,Empty]) 2)
                 --print (length (filter (\x -> x == Empty || x == Gas) ([Empty,Gas,GasRight,House,Unknown,Empty])))
                 --print (checkRowReady ([Empty,Empty,Gas,House,Unknown,Empty]) 2)