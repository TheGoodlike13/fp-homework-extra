module Main where

import Control.Monad
import Data.List
import Data.Maybe
import HostProvider
import JsonParser
import Network.HTTP
import System.Environment
import TicTacToe

validate :: String -> Bool
validate json = fromMaybe False (fmap isValid (parsedMoves json))

move :: String -> Maybe (Int, Int, Char)
move json = parsedMoves json >>= replay >>= nextMove >>= convert

winner :: String -> Maybe Char
winner json = fmap convertToken (parsedMoves json >>= replay >>= gameWinner)

parsedMoves :: String -> Maybe MoveHistory
parsedMoves = parseJson

convert :: Move -> Maybe (Int, Int, Char)
convert (x, y, token)
    | x < 0 || x > 2 || y < 0 || y > 2 = Nothing
    | otherwise = Just (x, y, convertToken token)

convertToken :: Token -> Char
convertToken token = head (show token)

gameUrl :: String -> String -> String
gameUrl gameId playerId = "http://" ++ ipAddress ++ "/game/" ++ gameId ++ "/player/" ++ playerId

contentType :: String
contentType = "application/json+list"

getGameBody :: String -> IO String
getGameBody url = do
    response <- simpleHTTP (replaceHeader HdrAccept contentType (getRequest url))
    debug (getResponseBody response) "Sending GET request"

postGame :: String -> MoveHistory -> IO String
postGame url moveHistory = do
    response <- simpleHTTP (postRequestWithBody url contentType (encodeJson moveHistory))
    debug (getResponseBody response) ("Sending POST request with body: " ++ encodeJson moveHistory)

playGame :: String -> IO()
playGame matchUrl = do
    movesJson <- getGameBody matchUrl
    let moves = parseJson movesJson
    let board = moves >>= replay
    let nextGameMove = board >>= nextMove
    let movesAfter = liftM2 appendMove moves nextGameMove
    print board
    if (isNothing movesAfter) then
        print "Game over!" >> return ()
    else
        postGame matchUrl (fromJust movesAfter) >> print (movesAfter >>= replay) >> playGame matchUrl

appendMove :: MoveHistory -> Move -> MoveHistory
appendMove moves lastMove = moves ++ [lastMove]

main :: IO()
main = do
    matchName <- fmap (!! 0) getArgs
    playerId <- fmap (!! 1) getArgs
    let matchUrl = gameUrl matchName playerId
    if (playerId == "1") then
        postGame matchUrl [defaultFirstMove] >> print (fromJust (play defaultFirstMove emptyBoard)) >> playGame matchUrl
    else
        playGame matchUrl

debug :: Show a => IO a -> String -> IO a
debug something message = do
    content <- something
    print message
    print content
    something