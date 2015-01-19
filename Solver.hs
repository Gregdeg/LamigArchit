module Solver where

import Puzzle

--solvePuzzle :: Puzzle -> Puzzle

--Wstawia znacznik pola, które pozostanie puste
setEmptyFields :: Puzzle -> Int -> Int -> Puzzle
setEmptyFields (Puzzle leftTab upperTab fields) _ (-1) = Puzzle leftTab upperTab fields
setEmptyFields (Puzzle leftTab upperTab fields) x y =
    let
        nx = if x == 0 then ((length upperTab) -1) else x -1
        ny = if x == 0 then y - 1 else y
    in
    if
        (fields!!y!!x == Unknown) && (((
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House) == NotFound  ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasUp) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasRight) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasLeft) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasBottom) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y Gas) ||
                        checkRowCompletness (fields!!y) (leftTab!!y) ||
                        checkRowCompletness (getColumn fields 5 x) (upperTab!!x)
                        )))
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
        mustDir = houseNeedsGasTankHere (Puzzle leftTab upperTab fields) x y
        dir = if mustDir /= NotFound then mustDir else checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House
        --dir = checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House
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
        --elem True (map (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y) [GasRight, GasUp, GasLeft, GasBottom, Gas])
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
    if elem x [GasUp, GasBottom, GasLeft, GasRight]
       then
        checkRowCompletness (xs) (c-1)
       else
        checkRowCompletness (xs) c
--[Empty,Empty,GasRight,House,Unknown,Empty]
--Sprawdza, czy w danym wierszu ilość pól pustych jest równa indeksowi
{-
checkRowReady :: [Field] -> Int -> Bool
checkRowReady [] _ = False
checkRowReady (x:xs) c =
    if elem x [GasBottom, GasLeft, GasRight, GasUp]
       then
        checkRowReady (xs) (c-1)
       else
       if
            (length(filter (== Unknown) (xs))) == c
            then
            True
            else
            checkRowReady (xs) (c-1)
-}

checkRowReady :: [Field] -> Int -> Bool
checkRowReady [] _ = False
checkRowReady (xs) c =
    let
        finalGases = length (filter (\x -> x == GasLeft || x == GasUp || x == GasUp || x == GasBottom) xs)
        empties =  length (filter (\x -> x == Unknown || x == Gas) xs)
        cg = c - finalGases
    in
    if cg == empties
    then
        True
    else
        False


checkPuzzleSolved :: Puzzle -> Int -> Bool
checkPuzzleSolved _ (-1) = True
checkPuzzleSolved (Puzzle leftTab upperTab puzzle) i = if
        checkRowCompletness (puzzle!!i) (leftTab!!i)
    then checkPuzzleSolved (Puzzle leftTab upperTab puzzle) (i-1)
    else False

--bierze kolumnę indeksując od 0
getColumn :: [[Field]] -> Int -> Int-> [Field]
getColumn _ (-1) _ = []
getColumn fields x y = [fields!!y!!x] ++ getColumn fields (x-1) y

solve :: Puzzle -> Int -> Int -> Puzzle
solve (Puzzle leftTab upperTab fields) _  (-1) =
	if True then --checkPuzzleSolved (Puzzle leftTab upperTab fields) 5 then --TODO
		(Puzzle leftTab upperTab fields)
	else
		solve (Puzzle leftTab upperTab fields) 5 5
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
        gasField = houseNeedsGasTankHere (Puzzle leftTab upperTab fields) x y
    in
    if
        (not (checkPuzzleSolved (Puzzle leftTab upperTab fields) 5)) &&
        elem (fields!!y!!x) [Unknown,Gas]
    then
        if
            ((upperTab!!x /= 0) && (leftTab!!y /= 0)) &&
            (
            ((checkRowReady (fields!!y) (leftTab!!y)))) ||
            ((checkRowReady (getColumn fields 6 x) (upperTab!!x)) ||
            gasField /= NotFound
            )
        then
                solve (setGasFields (setEmptyFields (Puzzle leftTab upperTab fields) (length leftTab -1) (length upperTab -1)) x y) nx ny
        else
                solve (setEmptyFields (Puzzle leftTab upperTab fields) (length leftTab -1) (length upperTab -1)) nx ny
    else
            solve (Puzzle leftTab upperTab fields) nx ny
