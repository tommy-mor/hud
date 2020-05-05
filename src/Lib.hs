module Lib
  ( someFunc
  , Node (..)
  ) where

import Text.Parsec
import Text.Parsec.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Node
  = Block String [Node]
  | Field String String (Maybe String)
  deriving (Show, Eq)


-- start with thing the recreates every file
-- then change it so it only recreates the needed files
-- then change it so that it splices in the text lines (with a parser that preserves line numbers)
-- and diffs old/new data structures? idk


-- IDEA maybe it only generates new files, then you like drag the folder manually, see if that works t opatch it.
-- like let windows do the diff. (not that manual alg is that much harder, but easier to start with/test)
