module Board where

data Board = Board {board:: [[Element]] }

data Position = Position {x :: Int,
						  y :: Int } deriving (Eq)

data Element = Empty | Black | White deriving (Read, Eq)

instance Show Element where
	show Empty = "_"
	show Black = "X"
	show White = "O"
	
instance Show Position where
	show (Position x y) = show x ++ ", " ++ show y 

instance Eq Board where
	(Board b1) == (Board b2) = b1 == b2


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

newBoardPar :: Int -> Board
newBoardPar dim = Board [[Empty|_<-[0..(dim-1)]]|_<-[0..(dim-1)]]

newFullPar :: Int -> Board
newFullPar dim = Board [[Black|_<-[0..(dim-1)]]|_<-[0..(dim-1)]]
	
-- Inserting Element 

insertElem :: Board -> Position -> Element -> Board
insertElem (Board board) pos elem = Board [[ (setBoardValue board pos elem xx yy) | xx <- [0..18]]| yy <- [0..18]]

setBoardValue :: [[Element]] -> Position -> Element -> Int -> Int -> Element 
setBoardValue board (Position x y) elem xx yy 
	| (x == xx ) && (y == yy) = elem
	| otherwise = (( board !! yy) !! xx)

--((( board !! x) !! y) == Empty)
-- Is Board Full
isFull :: Board -> Bool
isFull (Board b) = isFilled b

isFilled :: [[Element]] -> Bool
isFilled [] = True
isFilled (r:rs) = (isRowFilled r) && (isFilled rs)

isRowFilled :: [Element] -> Bool
isRowFilled [] = True
isRowFilled (e:es) = (e /= Empty) && (isRowFilled es)

-- First empty position
--firstEmpty :: Board -> Position
--firstEmpty (Board board) = (Position x y) where
		

--Is Game Won
--using current position and Element from main
isGameWon :: Board -> Position -> Element -> Bool
isGameWon (Board board) pos elem = ((fiveInRow board pos elem 1 0) || (fiveInRow board pos elem 1 (-1)) ||(fiveInRow board pos elem 0 1) || (fiveInRow board pos elem (-1) 1))

--check---- 6 bo dwa razy bierze ten od ktorego zaczyna chyba
fiveInRow :: [[Element]] -> Position -> Element -> Int -> Int -> Bool
fiveInRow board (Position x y) elem vec1 vec2 = (((length ls1) + (length ls2)) == 6) where
		ls1 = takeWhile (== elem) [((board !! yy) !! xx) | xx <- [x,(x+vec1)..x ], yy <- [y,(y+vec2)..]]
		ls2 = takeWhile (== elem) [((board !! yy) !! xx) | xx <- [x,(x-vec1)..], yy <- [y,(y-vec2)..]]


-- Test -----------
testisFull = isFull (newFullPar 3)
testisFull1 = isFull (newBoardPar 3)
testWon x y board = isGameWon board (Position x y) Black
test1 x y board= insertElem board (Position x y) Black
test2 = insertElem (insertElem (newBoard) (Position 4 4) Black) (Position 5 4) White
test3 = insertElem test2 (Position 5 5) Black


	

