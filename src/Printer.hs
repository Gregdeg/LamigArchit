module Printer where

import System.IO
import Puzzle

printRow _ (-1) stream = hPutChar stream '\n'
printRow content column stream = do
    printRow content (column-1) stream
    hPutStr stream (show (content!!column))

printPuzzle _ _ (-1) stream = return ()
printPuzzle (Puzzle _ upperTab fields) leftTab row stream = do
    printPuzzle (Puzzle [] upperTab fields) leftTab (row-1) stream
    printRow (fields!!row) (length upperTab-1) stream
    hPutStr stream (show (leftTab!!(row)))

printUpperTab _ (-1) stream = hPutChar stream '\n'
printUpperTab upperTab index stream = do
    printUpperTab upperTab (index-1) stream
    hPutStr stream (show (upperTab!!(index)))

--printAll