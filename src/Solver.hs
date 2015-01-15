module Solver where

import Puzzle

--solvePuzzle :: Puzzle -> Puzzle

--Wstawia znacznik pola, które pozostanie puste
setEmptyFields :: Puzzle -> Int -> Int -> Puzzle
setEmptyFields puzzle (0) (0) = puzzle
setEmptyFields (Puzzle leftTab upperTab fields) x y =
    let
        nx = if x == 0 then length upperTab else x -1
        ny = if x == 0 then y - 1 else y
    in
    if
        (fields!!y!!x /= House) && (
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House) == NotFound  ||
                        checkRowCompletness (fields!!y) (leftTab!!y) ||
                        checkRowCompletness (getColumn fields 5 x) (upperTab!!x)
                        )
    then
        setEmptyFields
            (Puzzle
            leftTab
            upperTab
            (
                (take y fields)++
                [(
                    (take x (fields!!y))++
                    [Empty]++
                    (drop (x+1) (fields!!y))
                )]++
                (drop (y+1) fields)
            )
            )
            nx
            ny
    else
        setEmptyFields
            (Puzzle
            leftTab
            upperTab
            fields)
            nx
            ny

-- Stawia zbiorniki z gazem
setGasFields :: Puzzle -> Int -> Int -> Puzzle
setGasFields puzzle (0) (0) = puzzle
setGasFields (Puzzle leftTab upperTab fields) x y =
    let
        nx = if x == 0 then length upperTab else x -1
        ny = if x == 0 then y - 1 else y
        dir = checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House
    in
    if
        (fields!!y!!x == Unknown) &&
        (dir /= NotFound) &&
        (dir /= MultipleFound) &&    
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasUp) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasRight) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasLeft) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasBottom)
    then
        setGasFields 
            (setAdjacentField 
                (Puzzle
                    ((take y leftTab)++[leftTab!!y-1]++(drop (y+1) leftTab))
                    ((take x upperTab)++[upperTab!!x-1]++(drop (x+1) upperTab))
                    (
                        (take y fields)++
                        [(
                            (take x (fields!!y))++
                            [case dir of Puzzle.Left -> GasLeft;Puzzle.Right -> GasRight;Up -> GasUp;Down -> GasBottom;]++
                            (drop (x+1) (fields!!y))
                        )]++
                        (drop (y+1) fields)
                    )
                )
                x
                y
                dir
                GasHouse
            )
            nx
            ny
                
    else
        setGasFields
            (Puzzle
            leftTab
            upperTab
            fields)
            nx
            ny



--cleanPuzzle :: Puzzle -> Int -> Int -> Puzzle
--cleanPuzzle (Puzzle leftTab upperTab fields) x y = cleanPuzzle

--Sprawdza, czy w danych wierszu ilość zbiorników zgadza się z docelową
checkRowCompletness :: [Field] -> Int -> Bool
checkRowCompletness _ 0 = True
checkRowCompletness [] _ = False
checkRowCompletness (x:xs) c =
    if x == GasUp ||
       x == GasRight ||
       x == GasBottom ||
       x == GasLeft
       then
        checkRowCompletness (xs) (c-1)
       else
        checkRowCompletness (xs) c

--bierze kolumnę indeksując od 0
getColumn :: [[Field]] -> Int -> Int-> [Field]
getColumn _ (-1) _ = []
getColumn fields x y = getColumn fields (x-1) y++[fields!!x!!y]


-- funkcja rozwiazujaca lamiglowke
-- iteracyjnie wykresla pola i ustawia zbiorniki, az tablice gorna i lewa sie nie wyzeruja
solve :: Puzzle -> Puzzle
solve (Puzzle leftTab upperTab fields) = if any (>0) leftTab || any (>0) upperTab then
        solve $ setGasFields (setEmptyFields (Puzzle leftTab upperTab fields) 5 5) 5 5
    else
        (Puzzle leftTab upperTab fields)