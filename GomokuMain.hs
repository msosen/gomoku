module Main1 where

import Board
import System.IO
import Text.Read
import Data.List
import Data.Maybe

data GameState = GameState {cur_user :: Element,
							cur_position :: Position,
							cur_board :: Board} deriving (Eq)
							
							

-- user - Black, comp - White
main :: IO ()
main = do
	let initState = initGame
	playGame initState
	putStr("")	

	
playGame :: GameState -> IO ()
playGame gameState = do
	if(won gameState)
		then do
			putStrLn "Przegrałeś..."
		else do
				printBoard gameState
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
						let userGameState = gameState {cur_user = White, cur_position = (Position (xcord - 1) (ycord - 1)),( cur_board = (Board (insertElem cur_board (Position (xcord - 1) (ycord - 1)) White))}
						if (won userGameState)
							then do
								printBoard userGameState
								putStrLn "Wygrałes "
							else do
								let compGameState = userGameState {cur_user = Black, cur_position = (Position 2 5),( cur_board = (insertElem cur_board (Position 2 5) Black)}
								playGame compGameState
					else
						putStrLn "Nieprawidlowe wspolrzedne, wprowadz jeszcze raz \n"
						playGame gameState
						putStr ("")
						
	

printBoard :: GameState -> IO ()
printBoard (GameState _ _ cur_board) = do
	print cur_board

--isGameWon :: Board -> Position -> Element -> Bool

won :: GameState -> Bool
won (GameState cur_user cur_position cur_position) = (isGameWon cur_board cur_position cur_user)

	
--randomMove :: Int
	
initGame = GameState {cur_user = Black, cur_position = (Position {x=0, y=0}), cur_board = newBoard}

isUser :: GameState -> Bool
isUser (GameState cur_user _ _ ) = (cur_user == Black)

isComp :: GameState -> Bool
isComp (GameState cur_user _ _) = (cur_user == White)
