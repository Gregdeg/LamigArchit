module Printer where

import System.IO
import Puzzle

printRow content (-1) stream= hPutChar stream '\n'
printRow content column stream=do
    printRow content (column-1) stream
    hPutStr stream (show (content!!column))

printPuzzle _ (-1) stream=return ()
printPuzzle (Puzzle _ upperTab fields) row stream=do
    printPuzzle (Puzzle [] upperTab fields) (row-1) stream
    printRow (fields!!row) (Prelude.length upperTab-1) stream
