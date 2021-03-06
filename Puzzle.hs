module Puzzle where

import System.IO

--Typ reprezentujący pole łamigłówki - nieznane, puste, domek i cztery kierunki zbiorników
data Field = Unknown | Empty | House | GasHouse | Gas| GasUp | GasRight | GasBottom | GasLeft deriving Eq

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
        Gas -> "g"

--Macierz wraz z krotnościami
data Puzzle = Puzzle [Int] [Int] [[Field]]

--Kierunki wraz z informacją o wielu
data Dirs = Up | Right | Down | Left | NotFound | MultipleFound deriving (Eq, Show)

--Przetwarza format wsadowy na macierz pól
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

-- Wstawia domy do macierzy pól
convertToRow :: [Int] -> Int -> [Field]
convertToRow _ (-1) = []
convertToRow indices count = if elem count indices  then convertToRow indices (count - 1) ++ [House] else convertToRow indices (count - 1) ++ [Unknown]

-- Ustawia sasiednie pole na okreslony typ, w praktyce domy z gazem
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
                (take (x) (fields!!(y-1)))++
                [field]++
                (drop (x+1) (fields!!(y-1)))
            )]++
            (drop (y) fields))
            )
        Down -> (Puzzle leftTab upperTab (
            (take (y+1) fields)++
            [(
                (take (x) (fields!!(y+1)))++
                [field]++
                (drop (x+1) (fields!!(y+1)))
            )]++
            (drop (y+2) fields))
            ) 
        _ -> (Puzzle leftTab upperTab fields)

--Sprawdza, czy w polach krzyżowo przyległych do danego jest podany w parametrze typ pola
checkAdjecentQuad :: Puzzle -> Int -> Int -> Field -> Dirs
checkAdjecentQuad _ (-1) _ _ = NotFound
checkAdjecentQuad _ _ (-1) _ = NotFound
checkAdjecentQuad (Puzzle leftTab upperTab fields) x y field =
    let dirs = [(y > 0  && fields!!(y-1)!!x == field)]
                ++ [((y < ((length leftTab) -1)) && fields!!(y+1)!!x == field)]
                ++ [(x > 0 && fields!!y!!(x-1) == field)]
                ++ [((x < ((length upperTab) -1 )) && fields!!y!!(x+1) == field)]
        trues =  length$filter (==True) dirs
    in
    case trues of
        0 -> NotFound
        1 -> if dirs!!0 then Up else if dirs!!1 then Down else if dirs!!2 then Puzzle.Right else Puzzle.Left
        --_ -> Up
        _ -> MultipleFound

--Sprawdza, czy we wszystkich polach przyległych do danego jest podany w parametrze typ pola
checkAdjecentOcta :: Puzzle -> Int -> Int -> Field -> Bool
checkAdjecentOcta (Puzzle leftTab upperTab fields) x y field =
    if
        (y > 0  && fields!!(y-1)!!x == field) || --S
        (y < ((length leftTab) -1) && fields!!(y+1)!!x == field) || --N
        (x > 0 && fields!!y!!(x-1) == field) || --W
        (x < ((length upperTab) -1) && fields!!y!!(x+1) == field) || --E
        (y > 0 && x > 0 && fields!!(y-1)!!(x-1) == field) || --SW
        (y > 0 && x < ((length upperTab) -1) &&  fields!!(y-1)!!(x+1) == field) || --SE
        (x > 0 && y < ((length leftTab) -1) && fields!!(y+1)!!(x-1) == field) || --NW
        ((x < ((length upperTab) -1)) && ((y < ((length leftTab) -1))) && fields!!(y+1)!!(x+1) == field) --NE
    then False
    else True


houseHasOnlyOnePossiblePlaceForTank :: Int -> Int -> Puzzle -> Dirs-> Bool
houseHasOnlyOnePossiblePlaceForTank x y (Puzzle leftTab upperTab fields) dir = --TODO oprócz unknown jeszcze Gas
        (fields!!y!!x == House &&
	checkAdjecentQuad (Puzzle leftTab upperTab fields) x y Unknown == dir)  ||
        (fields!!y!!x == House &&
	checkAdjecentQuad (Puzzle leftTab upperTab fields) x y Gas == dir)


-- Sprawdza, czy istnieje obok domek, który musi mieć na danym polu swój zbiornik; jeśli tak, zwraca kierunek zbiornika
houseNeedsGasTankHere :: Puzzle -> Int -> Int -> Dirs
houseNeedsGasTankHere (Puzzle leftTab upperTab puzzle) x y=
	--prawo
	if x < ((length upperTab)-1) && houseHasOnlyOnePossiblePlaceForTank (x+1) y (Puzzle leftTab upperTab puzzle) Puzzle.Right then Puzzle.Right else
	--lewo
	if x > 0 && houseHasOnlyOnePossiblePlaceForTank (x-1) y (Puzzle leftTab upperTab puzzle) Puzzle.Left then Puzzle.Left else
	--dol
	if y < ((length leftTab)-1) && houseHasOnlyOnePossiblePlaceForTank x (y+1) (Puzzle leftTab upperTab puzzle) Down then Down else
	--gora
	if y > 0 && houseHasOnlyOnePossiblePlaceForTank x (y-1) (Puzzle leftTab upperTab puzzle) Up then Up else
	NotFound


