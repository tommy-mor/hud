module Main where

import Lib
import Parse
import System.IO
import Text.Parsec

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
  putStr $ showNice (parse block "hudplayerhealth.res" contents)
  hClose handle

showNice = show
