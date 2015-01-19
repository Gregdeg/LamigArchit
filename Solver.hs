module Solver where

import Puzzle

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
                        (checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House) == NotFound ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasUp) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasRight) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasLeft) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y GasBottom) ||
                        not (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y Gas) ||
                        checkRowCompletness (fields!!y) (leftTab!!y) ||
                        checkRowCompletness (getColumn fields ((length leftTab)-1) x) (upperTab!!x)
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

-- Stawia zbiorniki z gazem i ustawia domy w gotowe
setGasFields :: Puzzle -> Int -> Int -> Puzzle
setGasFields puzzle (-1) (-1) = puzzle
setGasFields (Puzzle leftTab upperTab fields) x y =
    let
        mustDir = houseNeedsGasTankHere (Puzzle leftTab upperTab fields) x y
        dir = if mustDir /= NotFound then mustDir else checkAdjecentQuad (Puzzle leftTab upperTab fields) x y House
    in
    if
        (fields!!y!!x == Unknown || fields!!y!!x == Gas) &&
        (dir /= NotFound) &&
        elem True (map (checkAdjecentOcta (Puzzle leftTab upperTab fields) x y) [GasRight, GasUp, GasLeft, GasBottom, Gas])
    then
            (setAdjacentField
                (Puzzle
                    leftTab
                    upperTab
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


--Sprawdza, czy w danych wierszu ilość zbiorników zgadza się z docelową
checkRowCompletness :: [Field] -> Int -> Bool
checkRowCompletness _ (-1) = True --TODO
checkRowCompletness [] _ = False
checkRowCompletness (xs) c =
    let
        finalGases = length (filter (\x -> x == GasLeft || x == GasRight || x == GasUp || x == GasBottom) (xs))
    in
    if 
        finalGases == c then True else False

--Sprawdza, czy wiersz/kolumna ma tyle pól pustych co brakujących zbiorników
checkRowReady :: [Field] -> Int -> Bool
checkRowReady [] _ = False
checkRowReady (xs) c =
    let
        finalGases = length (filter (\x -> x == GasLeft || x == GasRight || x == GasUp || x == GasBottom) (xs))
        empties =  length (filter (\x -> x == Unknown || x == Gas) (xs))
        cg = c - finalGases
    in
    if 
        cg == empties then True else False

--Sprawdza wiersz po wierszu czy układanka jest gotowa
checkPuzzleSolved :: Puzzle -> Int -> Bool
checkPuzzleSolved _ 0 = True
checkPuzzleSolved (Puzzle leftTab upperTab puzzle) i = if
        checkRowCompletness (puzzle!!i) (leftTab!!i)
    then checkPuzzleSolved (Puzzle leftTab upperTab puzzle) (i-1)
    else False

--Zwraca całą zadaną drugim parametrem kolumnę
getColumn :: [[Field]] -> Int -> Int-> [Field]
getColumn _ (-1) _ = []
getColumn fields y x = [fields!!y!!x] ++ getColumn fields (y-1) x

-- Ostatni element, sprawdzenie poprawności
solve :: Puzzle -> Int -> Int -> Puzzle
solve (Puzzle leftTab upperTab fields) _  (-1) =
	if checkPuzzleSolved (Puzzle leftTab upperTab fields) ((length leftTab)-1) then
		(Puzzle leftTab upperTab fields)
	else
		solve (Puzzle leftTab upperTab fields) ((length upperTab)-1) ((length leftTab)-1)

--Główna funkcja aplikacji, wykonywana aż do ukończenia puzzli
solve (Puzzle leftTab upperTab fields) x y =
    let
        nx = if x == 0 then ((length upperTab) - 1) else x -1
        ny = if x == 0 then y - 1 else y
        gasField = houseNeedsGasTankHere (Puzzle leftTab upperTab fields) x y
    in
    if
        (not (checkPuzzleSolved (Puzzle leftTab upperTab fields) ((length leftTab)-1))) &&
        elem (fields!!y!!x) [Unknown,Gas]
    then
        if
            ((upperTab!!x /= 0) && (leftTab!!y /= 0)) &&
            (
                (checkRowReady (fields!!y) (leftTab!!y)) ||
                ((checkRowReady (getColumn fields ((length leftTab) -1) x) (upperTab!!x))) ||
                (gasField /= NotFound)
            )
        then
                solve (setGasFields (setEmptyFields (Puzzle leftTab upperTab fields) ((length upperTab) -1) ((length leftTab) -1)) x y) nx ny
        else
                solve (setEmptyFields (Puzzle leftTab upperTab fields) ((length upperTab) -1) ((length leftTab) -1)) nx ny
    else
            solve (Puzzle leftTab upperTab fields) nx ny
