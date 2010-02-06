module Bot where

import Tron.Protocol
import System.IO


react :: Board -> Move
react b = West

main = do
  playStdin react
