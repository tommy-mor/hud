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
  putStr $ mergeEither $ first show $ showFormat <$> (parse block "hudplayerhealth.res" contents)
  hClose handle

-- helper function for converting the error type of Eithers
-- we do all this because we want to keep the derived show function
first :: (a -> c) -> Either a b -> Either c b
first fn (Left a) = Left $ fn a
first _ (Right b) = Right b

mergeEither (Left a) = a
mergeEither (Right a) = a
