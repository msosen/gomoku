module GomokuData where

data Element = Empty | Black | White deriving (Read, Eq)

instance Show Element where
	show Empty = "_"
	show Black = "X"
	show White = "O"

data Board = Board {board:: [[Element]] }
data Position = Position { x :: Int, y :: Int } deriving (Eq)

-- Show for Board implementation	
instance Show Board where
	show (Board b) = show1Row ++ "  \n" ++ showBoard b	
	
showNum :: Int -> String
showNum num
    | num < 10 = show num ++ "  "
    | otherwise = show num ++ " "
	
show1Row :: String
show1Row = "   " ++(concatMap showNum [1..19])
	
showRow :: [Element] -> String
showRow [] = ""
showRow (e:es) = (show e) ++ "  " ++ (showRow es)

showBoard:: [[Element]] -> String
showBoard [] = ""
showBoard (r:rs) = (showNum(19-(length(rs)))) ++ (showRow r) ++ "\n" ++ (showBoard rs)

-- Board initialization

newBoard :: Board
newBoard = Board [[Empty|_<-[0..18]]|_<-[0..18]]

-------------------


insertB :: Board -> Position -> Element -> Board
insertB (Board tab) pos elem = Board [[ (setBoardValue tab pos elem xx yy) | xx <- [0..18]]| yy <- [0..18]]

setBoardValue :: [[Element]] -> Position -> Element -> Int -> Int -> Element 
setBoardValue tab (Position x y) elem xx yy 
	| (x == xx ) && (y == yy) && ((( tab !! x) !! y) == Empty) = elem
	| otherwise = (( tab !! x) !! y)

	
-- Test -----------
test1 = insertB (newBoard) (Position 4 4) Black
	
	

