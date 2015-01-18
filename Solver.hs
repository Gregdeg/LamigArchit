module Solver where

import Puzzle

--solvePuzzle :: Puzzle -> Puzzle

--Wstawia znacznik pola, które pozostanie puste
setEmptyFields :: Puzzle -> Int -> Int -> Puzzle
setEmptyFields (Puzzle leftTab upperTab fields) 0 0 =
    let
        x = 0
        y = 0
    in
    if
        fields!!0!!0 == Unknown && (
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House) == NotFound  ||
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y GasHouse) /= NotFound ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasUp) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasRight) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasLeft) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasBottom) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y Gas) ||
                        checkRowCompletness (fields!!y) (leftTab!!y) ||
                        checkRowCompletness (getColumn fields 5 x) (upperTab!!x)
                        )
    then

            (Puzzle
            leftTab
            upperTab
            (
                (take 0 fields)++
                [(
                    (take 0 (fields!!0))++
                    [Empty]++
                    (drop (1) (fields!!0))
                )]++
                (drop (1) fields)
            ))
    else
            (Puzzle
            leftTab
            upperTab
            fields)
setEmptyFields (Puzzle leftTab upperTab fields) x y =
    let
        nx = if x == 0 then ((length upperTab) -1) else x -1
        ny = if x == 0 then y - 1 else y
    in
    if
        (fields!!y!!x == Unknown && (
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House) == NotFound  ||
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y GasHouse) /= NotFound ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasUp) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasRight) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasLeft) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasBottom) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y Gas) ||
                        checkRowCompletness (fields!!y) (leftTab!!y) ||
                        checkRowCompletness (getColumn fields 5 x) (upperTab!!x)
                        ))
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
setGasFields puzzle (-1) (-1) = puzzle
setGasFields (Puzzle leftTab upperTab fields) x y =
    let
        dir = checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House
    in
    if
        (fields!!y!!x == Unknown || fields!!y!!x == Gas) &&
        (dir /= NotFound) &&
        --(dir /= MultipleFound) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasUp) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasRight) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasLeft) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasBottom) &&
        (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y Gas)
    then
    --error "setGas"
            (setAdjacentField
                (Puzzle
                    leftTab --((take y leftTab) ++ [leftTab!!y-1] ++ (drop (y+1) leftTab)) --y+1
                    upperTab --((take x upperTab) ++ [upperTab!!x-1] ++ (drop (x+1) upperTab)) -- x+1
                    (
                        (take y fields)++
                        [(
                            (take x (fields!!y))++
                            [case dir of Puzzle.Left -> GasLeft;Puzzle.Right -> GasRight;Up -> GasUp;Down -> GasBottom;MultipleFound -> Gas]++
                            (drop (x+1) (fields!!y)) --x+1
                        )]++
                        (drop (y+1) fields) --y+1
                    )
                )
                x
                y
                dir
                GasHouse
            )
    else
        (Puzzle leftTab upperTab fields)

--cleanPuzzle :: Puzzle -> Int -> Int -> Puzzle
--cleanPuzzle (Puzzle leftTab upperTab fields) x y = cleanPuzzle

--Sprawdza, czy w danych wierszu ilość zbiorników zgadza się z docelową
checkRowCompletness :: [Field] -> Int -> Bool
checkRowCompletness _ 0 = True --TODO
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

--Sprawdza, czy w danym wierszu ilość pól pustych jest równa indeksowi
checkRowReady :: [Field] -> Int -> Bool
checkRowReady [] 0 = True
checkRowReady [] _ = False
checkRowReady (x:xs) c =
    if elem x [Unknown, Gas, GasBottom, GasLeft, GasRight, GasUp]
       then
        checkRowReady (xs) (c-1)
       else
        checkRowReady (xs) c

checkPuzzleSolved :: Puzzle -> Int -> Bool
checkPuzzleSolved _ (-1) = True
checkPuzzleSolved (Puzzle leftTab upperTab puzzle) i = if
        checkRowCompletness (puzzle!!i) (leftTab!!i)
    then checkPuzzleSolved (Puzzle leftTab upperTab puzzle) (i-1)
    else False

--bierze kolumnę indeksując od 0
getColumn :: [[Field]] -> Int -> Int-> [Field]
getColumn _ _ (-1) = []
getColumn fields x y = getColumn fields x (y-1)++[fields!!y!!x]

-- funkcja rozwiazujaca lamiglowke
-- iteracyjnie wykresla pola i ustawia zbiorniki, az tablice gorna i lewa sie nie wyzeruja
--isSolved :: Puzzle -> Puzzle
--isSolved (Puzzle leftTab upperTab fields) = if any (>0) leftTab || any (>0) upperTab then
        --solve $ setGasFields (setEmptyFields (Puzzle leftTab upperTab fields) 5 5) 5 5
    --else
        --(Puzzle leftTab upperTab fields)

solve :: Puzzle -> Int -> Int -> Puzzle
solve (Puzzle leftTab upperTab fields) 0 0 = (Puzzle leftTab upperTab fields)
{-
    let
        nx = (length upperTab) - 1
        ny = (length leftTab) - 1
        y = 0
        x = 0
    in
      if not (checkPuzzleSolved (Puzzle leftTab upperTab fields) 5)
    then
    if
    ((upperTab!!x /= 0) && (leftTab!!y /= 0)) &&
    (((checkRowReady (fields!!y) (leftTab!!y))) ||
    ((checkRowReady (getColumn fields 6 x) (upperTab!!x))))
    then
            solve (setGasFields (Puzzle leftTab upperTab fields) x y) nx ny
    else
            solve (setEmptyFields (Puzzle leftTab upperTab fields) x y) nx ny
    else
        setEmptyFields (Puzzle leftTab upperTab fields) x y
-}
--Głowna funkcja aplikacji
solve (Puzzle leftTab upperTab fields) x y = 

    let
        nx = if x == 0 then ((length upperTab) - 1) else x -1
        ny = if x == 0 then y - 1 else y
    in
    if 
        (not (checkPuzzleSolved (Puzzle leftTab upperTab fields) 5)) &&
        (fields!!y!!x /= House) &&
        (fields!!y!!x /= GasHouse) &&
        (fields!!y!!x /= GasUp) &&
        (fields!!y!!x /= GasBottom) &&
        (fields!!y!!x /= GasLeft) &&
        (fields!!y!!x /= GasRight)
    then
    if
    --error "dawai"
    ((upperTab!!x /= 0) && (leftTab!!y /= 0)) && 
    (((checkRowReady (fields!!y) (leftTab!!y))) ||
    ((checkRowReady (getColumn fields x 5) (upperTab!!x))))
    then
            solve (setGasFields (Puzzle leftTab upperTab fields) x y) nx ny
    else
            solve (setEmptyFields (Puzzle leftTab upperTab fields) (length leftTab -1) (length upperTab -1)) nx ny
    else
            solve (Puzzle leftTab upperTab fields) nx ny
