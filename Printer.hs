module Printer where

import System.IO
import Puzzle

--Wypisuje pojedyńczy rząd wraz z krotnością
printRow _ (-1) stream = hPutChar stream '\n'
printRow content column stream = do
    printRow content (column-1) stream
    hPutStr stream (show (content!!column))

--Wypisuje wszystkie rzędy
printPuzzle _  (-1) stream = return ()
printPuzzle (Puzzle leftTab upperTab fields) row stream = do
    printPuzzle (Puzzle leftTab upperTab fields) (row-1) stream
    printRow (fields!!row) (length upperTab-1) stream
    hPutStr stream (show (leftTab!!(row)))

--Wypisuje górne krotności
printUpperTab _ (-1) stream = hPutChar stream '\n'
printUpperTab upperTab index stream = do
    printUpperTab upperTab (index-1) stream
    hPutStr stream (show (upperTab!!(index)))

--Wypisuje do strumienia macierz i krotności
printAll (Puzzle leftTab upperTab fields) row stream = do
    printUpperTab upperTab ((length upperTab) -1) stream
    printPuzzle (Puzzle leftTab upperTab fields) row stream

--Wypisuje do pliku macierz i krotności
printAllToFile (Puzzle leftTab upperTab fields) row fileName = do
    stream <- openFile fileName WriteMode
    printAll (Puzzle leftTab upperTab fields) row stream
    hClose stream