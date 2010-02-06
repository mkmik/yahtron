module Tron.Protocol where

import System.IO
import Data.List

testMap = "game.txt"

loadMap name = openFile name ReadMode

game strategy lines = concatMap (newline . renderMove) $ map strategy $ linesToBoards lines

getSize lines = (w, h)
    where [w,h] = map readInt $ words $ head lines


hInteract file f = do 
  s <- hGetContents file
  putStr (f s)

play file strategy = hInteract file $ (game strategy) . lines

fresh strategy= do
  fi <- loadMap testMap
  play fi strategy

setBuffers = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

playStdin strategy = do 
  setBuffers
  play stdin strategy

-- game

canMove move (x,y) tronMap
    | move == North	= if y == 0 then False else (Blank == ((tronMap !! (y-1)) !! x))
    | move == East	= if x+1 == (length (head tronMap)) then False else (Blank == ((tronMap !! y) !! (x+1)))
    | move == South	= if y+1 == (length tronMap) then False else (Blank == ((tronMap !! (y+1)) !! x))
    | move == West	= if x == 0 then False else (Blank == ((tronMap !! y) !! (x-1)))

--- map

type Board = [[Spot]]

data Spot = Wall | Blank | Player | Enemy 
            deriving (Show, Eq)

data Move = North | East | South | West 
            deriving (Show, Eq)

newline s = s ++ "\n"
renderMove North = "1"
renderMove East = "2"
renderMove South = "3"
renderMove West = "4"

readSpot '#' = Wall
readSpot ' ' = Blank
readSpot '1' = Player
readSpot '2' = Enemy

readInt :: String -> Int
readInt a = read a

parseBoard h lines = map (map readSpot) $ take h lines

linesToBoards :: [String] -> [Board]
linesToBoards [] = []
linesToBoards lines = parseBoard h useful : linesToBoards rest
    where (w,h) = getSize lines
          (useful, rest) = splitAt h $ tail lines
