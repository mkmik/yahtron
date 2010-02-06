module Bot where

import Tron.Protocol
import Tron.Board
import System.IO

-- main strategy
react :: Board -> Move
react b = head $ possibleMoves b

main = do
  playStdin react
