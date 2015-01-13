module Main where

import           Puzzle
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
                 printPuzzle (setEmptyFields (setGasFields puzzle 5 5) 5 5) 5 stdout