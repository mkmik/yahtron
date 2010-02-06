module Bot where

import Tron.Protocol
import Tron.Board
import System.IO


react :: Board -> Move
react b = West

main = do
  playStdin react
