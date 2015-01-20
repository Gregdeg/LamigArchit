module Main where

import Puzzle
import Printer
import Solver
import System.IO


main = do
                 putStrLn "Podaj nazwe pliku"
                 fileName <- getLine
                 putStrLn "Wczytuje..."
                 fileContent <- readFile fileName
                 let linesOfFile = lines fileContent
                 vCounts <- return (read (linesOfFile !! 0) :: [Int])
                 hCounts <- return (read (linesOfFile !! 1) :: [Int])
                 grid <- return (read (linesOfFile !! 2) :: [(Int,Int)])
                 let width = length hCounts
	         let height = length vCounts
	         let puzzle = Puzzle vCounts hCounts (convertInput (height -1) (width -1) grid)
	         let fields = (convertInput (height -1) (width -1) grid)
                 let solvedPuzzle1Fields =     [
                        [Empty,GasHouse,GasLeft,Empty,Empty,Empty],
                        [Empty,Empty,Empty,Empty,Empty,Empty],
                        [Empty,Empty,GasBottom,Empty,GasBottom,Empty],
                        [GasBottom,Empty,GasHouse,Empty,GasHouse,Empty],
                        [GasHouse,Empty,Empty,GasRight,GasHouse,GasBottom],
                        [Empty,GasRight,GasHouse,Empty,Empty,GasHouse]
                        ]
                 let solvedPuzzle1 = Puzzle [1,0,2,1,2,1] [1,1,2,1,1,1] solvedPuzzle1Fields

                 printAll (((solve (setEmptyFields puzzle (width-1) (height-1)) (width-1) (height-1)))) (height-1) stdout




