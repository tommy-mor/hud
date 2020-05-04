module Main where

import Lib
import System.IO

main :: IO ()
main
  -- taken from learn you a haskell
  -- http://learnyouahaskell.com/input-and-output
 = do
  handle <-
    openFile
      "/home/tommy/programming/clones/tf2basehud/resource/ui/hudplayerhealth.res"
      ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

