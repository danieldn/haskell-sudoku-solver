module Puzzle where

import Data.List

data Cell = Given Int 
            | Guesses [Int]
            deriving (Show, Eq)
{-
Given a means 'a' is a given number on sudoku puzzle
Guesses [1..9] means there are 1..9 possible choices (represents an 'empty' cell)
Guesses [2..9] means our current guess is 1
Guesses [3..9] means our current guess is 2
Guesses [] means our current guess is 9
-}
type Board = [Row]
type Row = [Cell] 
type Col = [Cell] 
type Grid = [Cell] 


-- Backtrack solver
solve :: Board -> Board
solve b = backtrack b (fst $ nextEmptyCell b 0 (-1)) (snd $ nextEmptyCell b 0 (-1)) -- start at c = -1 in case 0,0 is empty

backtrack :: Board -> Int -> Int -> Board
backtrack b r c 
    -- if calling next empty cell turns out to be (9,0), we're done
    | r == 9 && c == 0  = b
    -- tried all guesses, set cur cell to choices [1..9] and backtrack to
    -- previous empty cell 
    | choices == []     = backtrack b' prevR prevC 
    -- if guess is valid then go to next empty cell, else try again
    | otherwise         = if validGuess guessBoard r c then backtrack guessBoard nextR nextC else backtrack guessBoard r c
    where
          guessBoard    = returnBoardWithGuess b r c guessNum 
          guessNum      = head choices
          choices       = returnChoices curCell
          curCell       = ((b!!r)!!c)
          nextR         = fst $ nextEmptyCell b r c
          nextC         = snd $ nextEmptyCell b r c
          prevR         = fst $ prevEmptyCell b r c
          prevC         = snd $ prevEmptyCell b r c
          b'            = returnBoardWithGuess b r c 0 -- tried all guesses so resets current cell to guesses [1..9] 



-- Support functions on board
returnBoardWithGuess :: Board -> Int -> Int -> Int -> Board
returnBoardWithGuess b r c guess = boardWithGuess b r c guess 0

boardWithGuess :: Board -> Int -> Int -> Int -> Int -> Board
boardWithGuess [] _ _ _ _   = []
boardWithGuess (x:xs) r c guess i
    | i == 9                = []
    | i == r                = [newrow] ++ (boardWithGuess xs r c guess (i+1))
    | otherwise             = [x] ++ (boardWithGuess xs r c guess (i+1))
    where newrow            = returnRowWithGuess x c guess 

returnChoices :: Cell -> [Int]
returnChoices (Guesses a) = a

returnRowWithGuess :: Row -> Int -> Int -> Row
returnRowWithGuess row elem guess = returnRowHelper row elem guess 0

returnRowHelper :: Row -> Int -> Int -> Int -> Row
returnRowHelper [] _ _ _          = []
returnRowHelper (x:xs) elem guess i
    | i == elem             = [Guesses [guess+1..9]] ++ (returnRowHelper xs elem guess (i+1))
    | otherwise             = [x] ++ (returnRowHelper xs elem guess (i+1))

-- Checks guess at cell is valid by looking for collosions
validGuess :: Board -> Int -> Int -> Bool
validGuess b r c = (validRow (getRow b r)) && (validCol (getCol b c)) && (validGrid (getGrid b r c)) 

validRow :: Row -> Bool
validRow r = validRowHelp $ toIntList r

validRowHelp :: [Int] -> Bool
validRowHelp []     = True
validRowHelp (0:xs) = validRowHelp xs
validRowHelp (x:xs) = x `notElem` xs && validRowHelp xs

validCol :: Col -> Bool
validCol c = validRowHelp $ toIntList c

validGrid :: Grid -> Bool
validGrid g = validRowHelp $ toIntList g

-- Check for valid board 

validBoard :: Board -> Bool
validBoard b = (validRows b) && (validCols b) && (validGrids b)

validRows :: Board -> Bool
validRows []     = True
validRows (x:xs) = if validRow x then validRows xs else False

validCols :: Board -> Bool
validCols b = validColsHelp b 0

validColsHelp :: Board -> Int -> Bool
validColsHelp _ 9  = True
validColsHelp b i  = if validCol curCol then validColsHelp b (i+1) else False
    where curCol   = getCol b i

validGrids :: Board -> Bool
validGrids b = validGridsHelp b 0

validGridsHelp :: Board -> Int -> Bool
validGridsHelp _ 9  = True
validGridsHelp b i  = if validGrid curGrid then validGridsHelp b (i+1) else False
    where curGrid   = getGridHelper b i

-- Get, set, and convert functions
toIntList :: Row -> [Int]
toIntList []    = []
toIntList ((Given a):xs)    = [a] ++ (toIntList xs)
toIntList ((Guesses a):xs)
    | a == []     = [9] ++ (toIntList xs)
    | a == [1..9] = [0] ++ (toIntList xs)
    | otherwise   = [head a - 1] ++ (toIntList xs)

getCell :: Board -> Int -> Int -> Cell
getCell b r c = ((getRow b r) !! c)


getRow :: Board -> Int -> Row
getRow b r = (b!!r)

getCol :: Board -> Int -> Col
getCol b c = getColHelper b c 0 

getColHelper :: Board -> Int -> Int -> Col
getColHelper _ _ 9 = []
getColHelper (x:xs) c i = [x !! c] ++ (getColHelper xs c (i+1))


-- Return list representing a grid
-- Grid #s:
--      0   1   2 
--      3   4   5 
--      6   7   8 
getGrid :: Board -> Int -> Int -> Grid
getGrid b i j 
    | i >= 9 || i < 0 || j >= 9 || j < 0  = error "invalid index for grid"
    | i <= 2 && j <= 2                  = getGridHelper b 0  -- grid 0
    | i <= 2 && j > 2 && j < 6          = getGridHelper b 1
    | i <= 2 && j >= 6                  = getGridHelper b 2
    | i > 2 && i <= 5 && j <= 2         = getGridHelper b 3
    | i > 2 && i <= 5 && j > 2 && j < 6 = getGridHelper b 4
    | i > 2 && i <= 5                   = getGridHelper b 5
    | i > 5 && j <= 2                   = getGridHelper b 6
    | i > 5 && j > 2 && j < 6           = getGridHelper b 7
    | i > 5                             = getGridHelper b 8


-- ugly but it works
getGridHelper :: Board -> Int -> Grid 
getGridHelper b grid
    | grid == 0     = take 3 row0 ++ take 3 row1 ++ take 3 row2
    | grid == 1     = [row0 !! 3] ++ [row0 !! 4] ++ [row0 !! 5] ++ [row1 !! 3] ++ [row1 !! 4] ++ [row1 !! 5] ++ [row2 !! 3] ++ [row2 !! 4] ++ [row2 !! 5]
    | grid == 2     = [row0 !! 6] ++ [row0 !! 7] ++ [row0 !! 8] ++ [row1 !! 6] ++ [row1 !! 7] ++ [row1 !! 8] ++ [row2 !! 6] ++ [row2 !! 7] ++ [row2 !! 8]
    | grid == 3     = take 3 row3 ++ take 3 row4 ++ take 3 row5 
    | grid == 4     = [row3 !! 3] ++ [row3 !! 4] ++ [row3 !! 5] ++ [row4 !! 3] ++ [row4 !! 4] ++ [row4 !! 5] ++ [row5 !! 3] ++ [row5 !! 4] ++ [row5 !! 5]
    | grid == 5     = [row3 !! 6] ++ [row3 !! 7] ++ [row3 !! 8] ++ [row4 !! 6] ++ [row4 !! 7] ++ [row4 !! 8] ++ [row5 !! 6] ++ [row5 !! 7] ++ [row5 !! 8]
    | grid == 6     = take 3 row6 ++ take 3 row7 ++ take 3 row8 
    | grid == 7     = [row6 !! 3] ++ [row6 !! 4] ++ [row6 !! 5] ++ [row7 !! 3] ++ [row7 !! 4] ++ [row7 !! 5] ++ [row8 !! 3] ++ [row8 !! 4] ++ [row8 !! 5]
    | grid == 8     = [row6 !! 6] ++ [row6 !! 7] ++ [row6 !! 8] ++ [row7 !! 6] ++ [row7 !! 7] ++ [row7 !! 8] ++ [row8 !! 6] ++ [row8 !! 7] ++ [row8 !! 8]
    | otherwise     = []
    where row0      = b !! 0
          row1      = b !! 1
          row2      = b !! 2 
          row3      = b !! 3
          row4      = b !! 4
          row5      = b !! 5
          row6      = b !! 6
          row7      = b !! 7 
          row8      = b !! 8

getPrevCell :: Int -> Int -> (Int,Int)
getPrevCell 0 0 = (0,0)
getPrevCell r c
    | c == 0          = ((r-1),8)
    | otherwise       = (r,(c-1))

getNextCell :: Int -> Int -> (Int,Int)
getNextCell r c
    | c == 8         = ((r+1),0)
    | otherwise      = (r,(c+1))


nextEmptyCell :: Board -> Int -> Int -> (Int, Int)
nextEmptyCell b r c = nextEmptyCellHelp b row col
                    where row = fst $ getNextCell r c
                          col = snd $ getNextCell r c

nextEmptyCellHelp :: Board -> Int -> Int -> (Int, Int)
nextEmptyCellHelp b r c
    | r == 9                = (9, 0) -- done
    | isGiven ((b!!r)!!c)   = nextEmptyCellHelp b row col
    | otherwise             = (r,c)
    where row = fst $ getNextCell r c
          col = snd $ getNextCell r c


prevEmptyCell :: Board -> Int -> Int -> (Int,Int)
prevEmptyCell b r c = prevEmptyCellHelp b row col
                    where row = fst $ getPrevCell r c
                          col = snd $ getPrevCell r c 
   

prevEmptyCellHelp :: Board -> Int -> Int -> (Int, Int)
prevEmptyCellHelp b r c 
    | r == 0 && c == 0      = (0, 0) -- done
    | isGiven ((b!!r)!!c)   = prevEmptyCellHelp b row col
    | otherwise             = (r,c)
    where row = fst $ getPrevCell r c
          col = snd $ getPrevCell r c

isGiven :: Cell -> Bool
isGiven (Given a) = True
isGiven _ = False



