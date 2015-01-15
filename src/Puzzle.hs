module Puzzle where

import System.IO
--import Data.Sequence
--cokolwiek
--Typ reprezentujący pole łamigłówki - nieznane, puste, domek i cztery kierunki zbiorników
data Field = Unknown | Empty | House | GasHouse | GasUp | GasRight | GasBottom | GasLeft deriving Eq

instance Show Field where
    show f=case f of
        Unknown -> "-"
        Empty -> "x"
        House -> "h"
        GasHouse -> "H"
        GasUp -> "u"
        GasRight -> "r"
        GasBottom -> "b"
        GasLeft -> "l"

data Puzzle = Puzzle [Int] [Int] [[Field]]
data Dirs = Up | Right | Down | Left | NotFound deriving Eq

convertInput :: Int -> Int -> [(Int,Int)] -> [[Field]]
convertInput (-1) _ _ = []
convertInput v h tuples =
    convertInput (v-1) h tuples  ++
    [
    convertToRow
        (map 
            snd 
            (
                Prelude.filter
                    (\(x,y) -> x == v)
                    tuples
            )
        )
        h
    ]


convertToRow :: [Int] -> Int -> [Field]
convertToRow _ (-1) = []
convertToRow indices count = if elem count indices  then convertToRow indices (count - 1) ++ [House] else convertToRow indices (count - 1) ++ [Unknown]

-- ustawia sasiednie pole na okreslony typ
setAdjacentField :: Puzzle -> Int -> Int -> Dirs -> Field -> Puzzle
setAdjacentField (Puzzle leftTab upperTab fields) x y dir field =
    case dir of
        Puzzle.Left -> (Puzzle leftTab upperTab (
            (take y fields)++
            [(
                (take (x-1) (fields!!y))++
                [field]++
                (drop (x) (fields!!y))
            )]++
            (drop (y+1) fields))
            )
        Puzzle.Right -> (Puzzle leftTab upperTab (
            (take y fields)++
            [(
                (take (x+1) (fields!!y))++
                [field]++
                (drop (x+2) (fields!!y))
            )]++
            (drop (y+1) fields))
            )
        Up -> (Puzzle leftTab upperTab (
            (take (y-1) fields)++
            [(
                (take (x) (fields!!y))++
                [field]++
                (drop (x+1) (fields!!y))
            )]++
            (drop (y) fields))
            )
        Down -> (Puzzle leftTab upperTab (
            (take (y+1) fields)++
            [(
                (take (x) (fields!!y))++
                [field]++
                (drop (x+1) (fields!!y))
            )]++
            (drop (y+2) fields))
            )

--Sprawdza, czy w polach krzyżowo przyległych do danego jest podany w parametrze typ pola
checkAdjecentQuad :: Puzzle -> Int -> Int -> Field -> Dirs
checkAdjecentQuad (Puzzle leftTab upperTab fields) x y field =
    if
        (y > 0  && fields!!(y-1)!!x == field) then Down else
    if
        (y < (length upperTab) -1 && fields!!(y+1)!!x == field) then Up else
    if
        (x > 0 && fields!!y!!(x-1) == field) then Puzzle.Left else
    if
        (x < (length leftTab) -1 && fields!!y!!(x+1) == field) then Puzzle.Right else NotFound

--Sprawdza, czy we wszystkich polach przyległych do danego jest podany w parametrze typ pola
checkAdjecentOcta :: Puzzle -> Int -> Int -> Field -> Bool
checkAdjecentOcta (Puzzle leftTab upperTab fields) y x field =
    if
        (y > 0  && fields!!(y-1)!!x == field) || --S
        (y < (length upperTab) -1 && fields!!(y+1)!!x == field) || --N
        (x > 0 && fields!!y!!(x-1) == field) || --W
        (x < (length leftTab) -1 && fields!!y!!(x+1) == field) || --E
        (y > 0 && x > 0 && fields!!(y-1)!!(x-1) == field) || --SW
        (y > 0 && x < (length leftTab) &&  fields!!(y-1)!!(x+1) == field) || --SE
        (x > 0 && y < (length upperTab) -1 && fields!!(y+1)!!(x-1) == field) || --NW
        (x < (length leftTab) -1 && x < (length leftTab) && fields!!(y+1)!!(x+1) == field) --NE
    then False
    else True

--cleanPuzzle :: Puzzle -> Int -> Int -> Puzzle
--cleanPuzzle (Puzzle leftTab upperTab fields) x y = cleanPuzzle

