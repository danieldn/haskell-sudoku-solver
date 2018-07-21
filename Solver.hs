import System.Environment
import System.IO
import Control.Monad
import Puzzle


main :: IO()
main = do    
    files <- getArgs
    if files == []
        then runInteractive
        else solveForEachFile files



-- Interactive mode. User enters sudoku puzzle
runInteractive :: IO()
runInteractive = do putWelcomeMsg 
                    puz <- getPuz  
                    let board = createBoard puz  
                    putStrLn ""
                    putStrLn "Solving..."
                    putStrLn ""
                    -- printBoard board
                    if (not $ validBoard board) then
                      putStrLn "Board entered is invalid."
                      else
                      let board' = solve board in printBoard board'


solveForEachFile :: [String] -> IO()
solveForEachFile [] = putStrLn "Done"
solveForEachFile (x:xs) = do 
                          results <- readLines x
                          let results' = convertLines results
                          let board = createBoard results'
                          runSolver board
                          solveForEachFile xs

runSolver :: Board -> IO()
runSolver b = do putStrLn ""
                 putStrLn "Entered..."
                 putStrLn ""
                 printBoard b
                 putStrLn "Solving..."
                 putStrLn ""
                 if (not $ validBoard b) then
                    putStrLn "Board entered is invalid."
                    else
                    let b' = solve b in printBoard b'

convertLines :: [String] -> [[Int]]
convertLines [] = []
convertLines (x:xs) = [map read $ words x :: [Int]] ++ convertLines xs

readPuz :: String -> [Int]
readPuz c = map readInt (words c)
    where
        readInt = read :: String -> Int

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

createBoard :: [[Int]] -> [Row]
createBoard [[]] = [[]]
createBoard xs = map createBoard_helper xs

createBoard_helper :: [Int] -> Row
createBoard_helper xs = map (\x -> if x == 0 then Guesses [1..9] else Given x) xs


putWelcomeMsg :: IO()
putWelcomeMsg = do putStrLn "Enter a sudoku puzzle row by row"
                   putStrLn "Use whitespace to seperate values"
                   putStrLn "Use 0 to indicate empty cell"
                   putStrLn "----9x9 example----"
                   putStrLn "5 3 0  0 7 0  0 0 0" 
                   putStrLn "6 0 0  1 9 5  0 0 0"
                   putStrLn "0 9 8  0 0 0  0 6 0"
                   putStrLn "8 0 0  0 6 0  0 0 3"
                   putStrLn "4 0 0  8 0 3  0 0 1" 
                   putStrLn "7 0 0  0 2 0  0 0 6"
                   putStrLn "0 6 0  0 0 0  2 8 0"
                   putStrLn "0 0 0  4 1 9  0 0 5"
                   putStrLn "0 0 0  0 8 0  0 7 9"
                   putStrLn "-------------------"
                   putStrLn ""


getPuz :: IO [[Int]]
getPuz = getPuz_helper 0 [[]]

getPuz_helper :: Int -> [[Int]] -> IO [[Int]]
getPuz_helper cur xs = if cur == 9 then
                   return (tail (reverse xs))
                else
                   do row <- readRow
                      (getPuz_helper (cur+1) (row:xs))

readRow :: IO [Int]
readRow = fmap (map read.words) getLine

printBoard :: Board -> IO()
printBoard [] = putStrLn ""
printBoard (x:xs) = do print row 
                       printBoard xs
    where row = toIntList x

-- debugging
printRow :: Row -> IO()
printRow r = printBoard [r]