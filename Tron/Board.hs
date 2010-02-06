module Tron.Board where

-- a board is defined as a 2d array of spots
type Board = [[Spot]]

-- a spot can be one of:
data Spot = Wall | Blank | Player | Enemy 
            deriving (Show, Eq)

-- moves can go in these directions:
data Move = North | East | South | West 
            deriving (Show, Eq)

-- game helpers

canMove move (x,y) tronMap
    | move == North	= if y == 0 then False else (Blank == ((tronMap !! (y-1)) !! x))
    | move == East	= if x+1 == (length (head tronMap)) then False else (Blank == ((tronMap !! y) !! (x+1)))
    | move == South	= if y+1 == (length tronMap) then False else (Blank == ((tronMap !! (y+1)) !! x))
    | move == West	= if x == 0 then False else (Blank == ((tronMap !! y) !! (x-1)))
