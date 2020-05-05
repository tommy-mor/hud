module Lib
  ( someFunc
  , Node (..)
  , showFormat
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

tabs n = ['\t' | _ <- [1..n]]
quoted s = "\"" ++ s ++ "\""
tagged s = "[$" ++ s ++ "]"
newl = "\n"


showFormat :: Node -> String
showFormat = showFormatHelp 0
  where
    showFormatHelp n (Block name children) =
      (tabs n) ++
      (quoted name) ++
      newl ++
      (tabs n) ++
      "{" ++
      newl ++
      (foldl
         (\build nextchild -> (build ++ (showFormatHelp (n + 1) nextchild)))
         ""
         children) ++
      (tabs n) ++ "}" ++ newl
    showFormatHelp n (Field name val tag) =
      (tabs n) ++
      quoted name ++
      (tabs 1) ++ (quoted val) ++ (tabs 1) ++ (maybe "" tagged tag) ++ "\n"
