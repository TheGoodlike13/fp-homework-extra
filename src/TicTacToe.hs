{-# LANGUAGE FlexibleInstances #-}

module TicTacToe where

import Data.List
import Data.Maybe
import JsonParser
import Util

data Token = Cross | Circle deriving (Eq)
type Move = (Int, Int, Token)
type MoveHistory = [Move]
type Row = [Maybe Token]
type Board = [Row]

instance Show Token where
    show Cross = "X"
    show Circle = "O"

instance {-# OVERLAPPING #-} Show Move where
    show (x, y, token) = "(" ++ show x ++ ", " ++ show y ++ ", " ++ show token ++ ")"

instance {-# OVERLAPPING #-} Show MoveHistory where
    show moves = "[" ++ intercalate ", " [show move | move <- moves] ++ "]"

instance {-# OVERLAPPING #-} Show Board where
    show [] = "\n+-----+"
    show (row : others) = "\n+-----+\n" ++ show row ++ show others

instance {-# OVERLAPPING #-} Show Row where
    show [] = "|"
    show (Nothing : others) = "| " ++ show others
    show ((Just token) : others) = "|" ++ show token ++ show others

instance Json Token where
    toJson token = JsonString (show token)
    fromJson (JsonString v)
        | v == "x" || v == "X" = Just Cross
        | v == "o" || v == "O" || v == "0" = Just Circle
    fromJson _ = Nothing

instance Json Move where
    toJson (x, y, token) = JsonObject [("x", JsonInt x), ("y", JsonInt y), ("v", toJson token)]
    fromJson (JsonObject [("x", JsonInt x), ("y", JsonInt y), ("v", v)])
        = fmap (\token -> (x, y, token)) (fromJson v)
    fromJson _ = Nothing

instance Json MoveHistory where
    toJson moves = JsonArray [toJson move | move <- moves]
    fromJson (JsonArray moves) = accumulateJson moves []

accumulateJson :: [JsonValue] -> [Move] -> Maybe MoveHistory
accumulateJson [] mem = Just (reverse mem)
accumulateJson (jsonMove : others) mem = fromJson jsonMove >>= (\move -> accumulateJson others (move : mem))

emptyBoard :: Board
emptyBoard = replicate 3 (replicate 3 Nothing)

defaultFirstMove :: Move
defaultFirstMove = (1, 1, Cross)

getNextPlayer :: Board -> Token
getNextPlayer board
    | (nothingCount board) `mod` 2 == 1 = Cross
    | otherwise = Circle

nothingCount :: Board -> Int
nothingCount board = sum [nothingCountInRow row | row <- board]

nothingCountInRow :: Row -> Int
nothingCountInRow row = length [token | token <- row, isNothing token]

isValid :: MoveHistory -> Bool
isValid moves = isJust (replay moves)

nextMove :: Board -> Maybe Move
nextMove board
    | isWinner (other nextPlayer) board = Nothing
    | null possibleMoves = Nothing
    | haveAnyWinningMoves = Just (head winningMoves)
    | nextPlayerHasMoreThanOneWinningMove = Nothing
    | nextPlayerHasOneWinningMove = Just (overtake (head nextPlayerWinningMoves))
    | nextPlayerHasWinningMoveInTwoTurns = Just (overtake (head nextPlayerWinningMovesInTwoTurns))
    | otherwise = Just (head possibleMoves)
    where
        nextPlayer = getNextPlayer board
        possibleMoves = findAllPossibleMoves board nextPlayer
        winningMoves = findWinningMoves possibleMoves board
        haveAnyWinningMoves = (not . null) winningMoves
        otherPlayerMoves = map overtake possibleMoves
        nextPlayerWinningMoves = findWinningMoves otherPlayerMoves board
        nextPlayerHasOneWinningMove = length nextPlayerWinningMoves == 1
        nextPlayerHasMoreThanOneWinningMove = length nextPlayerWinningMoves > 1
        nextPlayerWinningMovesInTwoTurns = findWinningMovesInTwoTurns (other nextPlayer) board possibleMoves []
        nextPlayerHasWinningMoveInTwoTurns = length nextPlayerWinningMovesInTwoTurns > 0

gameWinner :: Board -> Maybe Token
gameWinner board
    | isWinner Cross board = Just Cross
    | isWinner Circle board = Just Circle
    | otherwise = Nothing

play :: Move -> Board -> Maybe Board
play (x, y, token) board
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | winnerExists board = Nothing
    | getNextPlayer board /= token = Nothing
    | otherwise = applyMove (x, y, token) board

replay :: MoveHistory -> Maybe Board
replay moves = replayAccumulate moves (Just emptyBoard)

replayAccumulate :: [Move] -> Maybe Board -> Maybe Board
replayAccumulate [] board = board
replayAccumulate (move : others) board = replayAccumulate others (board >>= play move)

applyMove :: Move -> Board -> Maybe Board
applyMove (x, y, token) board
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | otherwise = fmap (\row -> replace x row board) newRow
    where newRow = replaceToken y token (board !! x)

replaceToken :: Int -> Token -> Row -> Maybe Row
replaceToken y token row
    | y < 0 || y > 2 = Nothing
    | isJust (row !! y) = Nothing
    | otherwise = Just (replace y (Just token) row)

winnerExists :: Board -> Bool
winnerExists board = isJust (gameWinner board)

isWinner :: Token -> Board -> Bool
isWinner token board = checkRows token board || checkColumns token board || checkDiagonals token board

checkRows :: Token -> Board -> Bool
checkRows token board = foldl (||) False [checkArray token row | row <- board]

checkColumns :: Token -> Board -> Bool
checkColumns token board = foldl (||) False [checkArray token (getCollumn i board) | i <- indexes]

checkDiagonals :: Token -> Board -> Bool
checkDiagonals token board = checkArray token (getLeftDiagonal board) || checkArray token (getRightDiagonal board)

checkArray :: Token -> [Maybe Token] -> Bool
checkArray _ [] = True
checkArray token (first : others) = isJust first && token == (fromJust first) && checkArray token others

getTokenAt :: Int -> Int -> Board -> Maybe Token
getTokenAt x y board = board !! x !! y

getArray :: [(Int, Int)] -> Board -> [Maybe Token]
getArray coords board = [getTokenAt x y board | (x, y) <- coords]

indexes :: [Int]
indexes = [0..2]

getRow :: Int -> Board -> Row
getRow i board = board !! i

getCollumn :: Int -> Board -> [Maybe Token]
getCollumn i board = getArray coords board
    where coords = zip indexes (replicate 3 i)

getLeftDiagonal :: Board -> [Maybe Token]
getLeftDiagonal board = getArray coords board
    where coords = zip indexes indexes

getRightDiagonal :: Board -> [Maybe Token]
getRightDiagonal board = getArray coords board
    where coords = zip (reverse indexes) indexes

other :: Token -> Token
other Cross = Circle
other Circle = Cross

overtake :: Move -> Move
overtake (x, y, token) = (x, y, other token)

findAllPossibleMoves :: Board -> Token -> [Move]
findAllPossibleMoves board token = accumulatePossibleMoves board token 0 []

accumulatePossibleMoves :: Board -> Token -> Int -> [Move] -> [Move]
accumulatePossibleMoves [] _ _ moves = moves
accumulatePossibleMoves (row : rest) token x moves = accumulatePossibleMoves rest token (x + 1) (movesInRow ++ moves)
    where
        movesInRow = accumulatePossibleMovesInRow row token x 0 []

accumulatePossibleMovesInRow :: Row -> Token -> Int -> Int -> [Move] -> [Move]
accumulatePossibleMovesInRow [] _ _ _ moves = moves
accumulatePossibleMovesInRow (first : rest) token x y moves
    | isJust first = accumulatePossibleMovesInRow rest token x (y + 1) moves
    | otherwise = accumulatePossibleMovesInRow rest token x (y + 1) (move : moves)
    where
        move = (x, y, token)

findWinningMoves :: [Move] -> Board -> [Move]
findWinningMoves [] _ = []
findWinningMoves (move : others) board
    | winnerExists afterMove = move : findWinningMoves others board
    | otherwise = findWinningMoves others board
    where
        afterMove = forceMove move board

forceMove :: Move -> Board -> Board
forceMove move board = fromJust (applyMove move board)

findWinningMovesInTwoTurns :: Token -> Board -> [Move] -> [Move] -> [Move]
findWinningMovesInTwoTurns _ _ [] accumulatedMoves = accumulatedMoves
findWinningMovesInTwoTurns player board (move : others) accumulatedMoves
    = findWinningMovesInTwoTurns player board others (accumulatedMoves ++ findWinningMovesNextTurn player afterMove allMovesAfter [])
    where
        afterMove = forceMove move board
        allMovesAfter = findAllPossibleMoves afterMove player

findWinningMovesNextTurn :: Token -> Board -> [Move] -> [Move] -> [Move]
findWinningMovesNextTurn _ _ [] accumulatedMoves = reverse accumulatedMoves
findWinningMovesNextTurn player board (move : others) accumulatedMoves
    | length winningMoves > 1 = findWinningMovesNextTurn player board others (move : accumulatedMoves)
    | otherwise = findWinningMovesNextTurn player board others accumulatedMoves
    where
        afterMove = forceMove move board
        allMovesAfter = findAllPossibleMoves afterMove player
        winningMoves = findWinningMoves allMovesAfter afterMove
