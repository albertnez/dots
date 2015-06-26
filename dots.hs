import System.Random

-- Our board. Hor. lines, Ver. lines and cells
data Board = Board [[Int]] [[Int]] [[Int]]

-- Aux show functions
showHor :: [Int] -> String
showHor [] = "."
showHor (a:as)
  | a == 0 = ".   " ++ showHor as
  | a == 1 = ".===" ++ showHor as
  | otherwise = ".***" ++ showHor as

showCell :: Int -> String
showCell 0 = "   "
showCell n = " " ++ show n ++ " "

showVerWall :: Int -> String
showVerWall 0 = " "
showVerWall 1 = "="
showVerWall 2 = "*"

showVer :: [Int] -> [Int] -> Int -> String
showVer [] _ _ = ""
showVer (a:as) bs 0 = showVerWall a ++ showVer bs as 1
showVer (a:as) bs 1 = showCell a ++ showVer bs as 0

instance Show Board where
  show b = f b 0 -- 0 is hor, 1 is ver
    where
      f (Board hor ver cells) t
        | t == 0 && hor == [] = ""
        | t == 0 = showHor (head hor) ++ "\n" ++ f (Board (tail hor) ver cells) 1
        | ver == [] = ""
        | otherwise = showVer (head ver) (head cells) 0 
                   ++ "\n" ++ f (Board hor (tail ver) (tail cells)) 0

data Move = Hor (Int,Int) | Ver (Int,Int)

instance Show Move where
  show (Hor pos) = "Horizontal line at " ++ show pos
  show (Ver pos) = "Vertical line at " ++ show pos

-- Function to read move
readMove :: String -> Move
readMove s
  | take 3 s == "hor" = Hor ((\[i,j] -> (i,j)) $ parseArgs $ drop 3 s)
  | take 3 s == "ver" = Ver ((\[i,j] -> (i,j)) $ parseArgs $ drop 3 s)

type Pos = (Int, Int)

-- Aux functions
createEmptyMatrix :: Int -> Int -> [[Int]]
createEmptyMatrix n m = [[0 | j <- [1..m]] | i <- [1..n]]

-- Board stuff
-- Creates an empty board of n by m dots
createEmptyBoard :: Int -> Int -> Board

createEmptyBoard n m = (Board (createEmptyMatrix n (m-1))
                              (createEmptyMatrix (n-1) m)
                              (createEmptyMatrix (n-1) (m-1)))

boardSize :: Board -> Pos
boardSize (Board hor ver cells) = (length hor, length ver)

-- Value at position i j of a matrix
valueAt :: [[Int]] -> Pos -> Int
valueAt mat (i,j) = mat !! i !! j

-- Given a list, position i, and value x, sets the i-th element to x
setValueList :: [Int] -> Int -> Int -> [Int]
setValueList l i x = left ++ [x] ++ tail right
  where
    (left, right) = splitAt i l

-- Given a Matrix, position i j, value x, returns matrix with x set at (i,j)
setValueMat :: [[Int]] -> Pos -> Int -> [[Int]]
setValueMat mat (i,j) x = left 
                       ++ [setValueList (head right) j x]
                       ++ tail right
  where
    (left, right) = splitAt i mat

-- Pairs of (i, j) representing free cells in a matrix
freePositions :: [[Int]] -> [Pos]
freePositions m = [(i,j) | i <- [0..length m -1],
                           j <- [0..length (m!!0) -1],
                           valueAt m (i,j) == 0]

-- Returns a list of all available moves in the board
availableMoves :: Board -> [Move]
availableMoves (Board hor ver cells) = horMoves ++ verMoves
  where
    horMoves = map (\p -> Hor p) $ freePositions hor
    verMoves = map (\p -> Ver p) $ freePositions ver

-- Given a Board, i j p, updates the cell at i,j marked as made by p
-- if the cell is now completed
updateBoardCell :: Board -> Pos -> Int -> Board
updateBoardCell b@(Board hor ver cells) (i,j) p
  | valueAt cells (i,j) /= 0 = b
  | all (/= 0) lines = Board hor ver $ setValueMat cells (i,j) p
  | otherwise = b
  where
    lines = [valueAt hor (i,j), valueAt hor (i+1,j),
             valueAt ver (i,j), valueAt ver (i,j+1)]

-- Aux function to update all cells with player p
updateBoardCells :: Board -> Int -> Board
updateBoardCells b@(Board hor ver cells) p = foldl f b positions
  where
    f b pos = updateBoardCell b pos p
    positions = [(i,j) | i <- [0..length cells -1],
                         j <- [0..length (cells!!0) -1]]

-- Aux function to count number of lines in a cell
numberLinesCell :: Board -> Pos -> Int
numberLinesCell (Board hor ver cells) (i,j) = (f $ valueAt hor (i+1,j)) +
  (f $ valueAt hor (i,j)) + (f $ valueAt ver (i,j)) + (f $ valueAt ver (i,j+1))
  where
    f 0 = 0
    f _ = 1

-- Apply a move to the board by player p. Move should be valid
applyMove :: Board -> Move -> Int -> Board
applyMove (Board hor ver cells) (Hor pos) p
  = Board (setValueMat hor pos p) ver cells
applyMove (Board hor ver cells) (Ver pos) p
  = Board hor (setValueMat ver pos p) cells

-- Check if a board is full
fullBoard :: Board -> Bool
fullBoard (Board hor ver cells) = any (elem 0) cells

makeTurn :: Board -> Strategy -> Int -> IO Board
makeTurn board str p = do
  move <- str board
  putStrLn $ "Move made by player " ++ show p ++ ":  " ++ show move
  return (updateBoardCells (applyMove board move p) p)

-- Number of free cells
countFreeCells :: Board -> Int
countFreeCells (Board hor ver cells) = foldl f 0 cells
  where
    f n list = n + (length $ filter (== 0) list)
-- End Board stuff

-- Strategy Stuff
type Strategy = Board -> IO Move

-- Dumb AI - Just take the first available move
dumbStrategy :: Board  -> IO Move
dumbStrategy board = return (head $ availableMoves board)

-- Random AI - Just take a random available move
randStrategy :: Board -> IO Move
randStrategy board = do
  ind <- randomRIO (0, length moves -1)
  return (moves !! ind)
  where
    moves = availableMoves board

-- 'Intelligent' AI
-- Aux function to given a line, check the actual lines adjacent to the square
adjCellsLines :: Board -> Move -> Int -> Bool
adjCellsLines board@(Board hor ver cells) (Hor (i,j)) desired
  | i == 0 = numberLinesCell board (i,j) == desired
  | i == length hor -1 = numberLinesCell board (i-1,j) == desired
  | otherwise = any (==desired) [numberLinesCell board (x,j) | x <- [i-1..i]]

adjCellsLines board@(Board hor ver cells) (Ver (i,j)) desired
  | j == 0 = numberLinesCell board (i,j) == desired
  | j == length (ver !! 0) -1 = numberLinesCell board (i,j-1) == desired
  | otherwise = any (==desired) [numberLinesCell board (i,x) | x <- [j-1..j]]

-- If there is a scoring moves, it takes it. Else, avoids moves
-- that lead to a square with one remaining edge. Else, random move
intelligentStrategy :: Board -> IO Move
intelligentStrategy board 
  | length goodMoves > 0 = do
    putStrLn "ind: "
    ind <- randomRIO (0, length goodMoves -1)
    return (goodMoves !! ind)
  | length normalMoves > 0 = do
    putStrLn "ind: "
    ind <- randomRIO (0, length normalMoves -1)
    return (normalMoves !! ind)
  | otherwise = do
    putStrLn "ind: "
    ind <- randomRIO (0, length moves -1)
    return (moves !! ind)
  where
    moves = availableMoves board
    normalMoves = filter (\x -> not $ adjCellsLines board x 2) moves
    goodMoves = filter (\x -> adjCellsLines board x 3) moves

-- Human
humanStrategy :: Board -> IO Move
humanStrategy board = do
  putStrLn "please, make a valid move (ver i j | hor i j)"
  line <- getLine
  let move = readMove line
  return (move)
-- End strategy stuff


-- Part of Game Stuff. Turn is either 0 or 1
gameTurn :: Board -> (Int,Int) -> Int -> Strategy -> Strategy -> IO (Int,Int)
gameTurn board (s1,s2) turn str1 str2 = do
  --move <- str1 board p
  let freeCells = countFreeCells board

  if freeCells == 0 
  then do
    if turn == 0 
    then return ((s1,s2))
    else return ((s2,s1))
  else do
    newBoard <- makeTurn board str1 (turn+1)


    let newFreeCells = countFreeCells newBoard
    let scoreMade = freeCells - newFreeCells

    putStrLn $ "score: " ++ (show $ (\(x,y) -> if turn == 0 
                                               then (x,y) 
                                               else (y,x)) (s1+scoreMade,s2))
    putStrLn $ show newBoard
    if scoreMade > 0
      then do gameTurn newBoard (s1+scoreMade,s2) turn str1 str2
      else do gameTurn newBoard (s2,s1) (1-turn) str2 str1


game :: Board -> Strategy -> Strategy -> IO ()
game board str1 str2 = do
  putStrLn (show board)
  score <- gameTurn board (0,0) 0 str1 str2
  if (fst score == snd score)
    then do putStrLn "There is a tie!"
    else do putStrLn $ "Player " ++ ((\(x,y) -> if x > y then "1" else "2") score) ++ " wins!"
  putStrLn $ "Final score:  " ++ (show $ fst score) ++ ", " ++ (show $ snd score)
  return ()
-- End of game

-- Game setter
-- Select an AI
selectAI :: Int -> Strategy
selectAI 1 = dumbStrategy
selectAI 2 = randStrategy
selectAI 3 = intelligentStrategy
selectAI 4 = humanStrategy

initGame :: IO ()
initGame = do
  putStrLn "Dots! Join the dots to close the squares!"
  putStrLn "Player 1 lines marked as *"
  putStrLn "Player 2 lines marked as ="
  putStrLn "Please, select board number of dots 'width height'"
  line <- getLine 
  let size = parseArgs line

  putStrLn "please, select players: 'first second'"
  putStrLn "\t1) easy AI"
  putStrLn "\t2) random AI"
  putStrLn "\t3) Intelligent AI"
  putStrLn "\t4) Human"

  line <- getLine
  let players = parseArgs line

  game (createEmptyBoard (size!!0) (size!!1))
    (selectAI (players!!0)) (selectAI (players!!1))
  return ()


-- IO aux functions
isNum :: String -> Bool
isNum s = all (\c -> c >= '0' && c <= '9') s

splitWords :: String -> [String]
splitWords "" = []
splitWords s
  | l > 0 = ((take l s):(splitWords $ drop (l+1) s))
  | otherwise = splitWords $ drop (l+1) s
  where
    l = length $ takeWhile (/= ' ') s

validArgs :: String -> Bool
validArgs s = all isNum words && length words == 2
  where
    words = splitWords s

parseArgs :: String -> [Int]
parseArgs s = map (read) $ splitWords s

main :: IO ()
main = initGame
