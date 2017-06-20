module Main where

import Board
import System.IO
import Text.Read
import Data.List
import Data.Maybe

data GameState = GameState {cur_user :: Element,
							cur_position :: Position,
							cur_board :: Board} deriving (Eq)
							
							

-- user - Black, comp - White
instance Show GameState where
	show (GameState u p b) = show u ++ "\n" ++ show p ++ "\n" ++ show b ++ "\n"


main :: IO ()
main = do
	let initState = initGame
	playGame initState
	putStr("")	
	
updateGameState :: GameState -> Element -> Position -> Board -> GameState
updateGameState gameState user (Position x y) board = (GameState user (Position x y) board)



playGame :: GameState -> IO ()
playGame gameState = do
	if(won gameState)
		then do
			putStrLn "Przegrałeś..."
		else do
				--printBoard gameState
				print gameState
				putStrLn "Wprowadz pierwsza wspolrzedna: "
				hFlush stdout
				x <- getLine
				let xcord = (read x::Int)
				putStrLn "Wprowadz drugą wspolrzedna: "
				hFlush stdout
				y <- getLine
				let ycord = (read y::Int)
				if(((xcord >= 1) && (xcord <= 19)) && ((ycord >= 1) && (ycord <= 19)))
					then do
						--insertElem :: Board -> Position -> Element -> Board
						--GameState cur_user cur_position cur_board
						--{cur_user = White, cur_position = (Position (xcord - 1) (ycord - 1)), cur_board = insertElem cur_board (Position (xcord - 1) (ycord - 1)) White}
						let userGameState = gameState {cur_user = White, cur_position = (Position (xcord - 1) (ycord - 1)), cur_board = (insertElem (cur_board gameState) (Position (xcord - 1) (ycord - 1)) White)}
						print userGameState
						putStrLn " ^ userGameState ^^"
						if (won userGameState)
							then do
								printBoard userGameState
								putStrLn "Wygrałes "
							else do
								let compGameState = userGameState {cur_user = Black, cur_position = (Position (xcord - 4) (ycord - 3)),cur_board = (insertElem (cur_board userGameState) (Position (xcord - 4) (ycord - 3)) Black)}				
								playGame compGameState
					else do
						putStrLn "Nieprawidlowe wspolrzedne, wprowadz jeszcze raz \n"
						playGame gameState
						putStr ("")
						
	
getBoard :: GameState ->  Board
getBoard (GameState _ _ cur_board) = cur_board
	
printBoard :: GameState -> IO ()
printBoard (GameState _ _ cur_board) = do
	print cur_board

--isGameWon :: Board -> Position -> Element -> Bool

won :: GameState -> Bool
won (GameState cur_user cur_position cur_board) = (isGameWon cur_board cur_position cur_user)

	
--randomMove :: Int
	
initGame = GameState {cur_user = Black, cur_position = (Position {x=0, y=0}), cur_board = newBoard}

isUser :: GameState -> Bool
isUser (GameState cur_user _ _ ) = (cur_user == Black)

isComp :: GameState -> Bool
isComp (GameState cur_user _ _) = (cur_user == White)
