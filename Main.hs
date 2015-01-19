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
	let width=length hCounts
	let height=length vCounts
	grid <- return (read (linesOfFile !! 2) :: [(Int,Int)])
	let puzzle = Puzzle vCounts hCounts (convertInput (height -1) (width -1) grid)
	let fields = (convertInput (height -1) (width -1) grid)
	--printUpperTab vCounts width stdout
	--printPuzzle (setGasFields puzzle width height) hCounts height stdout
	--print (getColumn (convertInput height width grid) (height-1) 0)
	--printPuzzle ((setEmptyFields puzzle 5 5)) 5 stdout
	--printAll (solve (Puzzle vCounts hCounts (convertInput (length vCounts) (length hCounts) grid)) 5 5) 5  stdout
	--printAll (solve (setEmptyFields puzzle 5 5) 5 5) 5 stdout
	--printAllToFile (puzzle) 5  "result2.txt"
	--print (checkRowReady ([Empty,Gas,GasRight,House,Unknown,Empty]) 2)
	--print (length (filter (\x -> x == Empty || x == Gas) ([Empty,Gas,GasRight,House,Unknown,Empty])))
	--print (checkRowReady ([Empty,Empty,Unknown,Empty,Unknown,Empty]) (vCounts!!3))
	--putStrLn "\n"
	--print (hCounts!!0)
	--print (getColumn (fields) 5 0)
	--print (fields!!5)
	printAll (((solve (setEmptyFields puzzle (width-1) (height-1)) (width-1) (height-1)))) (height-1) stdout