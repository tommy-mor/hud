module Lib
  ( someFunc
  , Node (..)
  , Feature (..)
  , DepExpr (..)
  , Quoted (..)
  , Hud (..)
  , getVals
  ) where

import Text.Parsec
import Text.Parsec.String

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Tag = Maybe String

data Quoted = Quoted | Unquoted deriving (Show, Eq)

data Node
  = Block String Quoted [Node] Tag
  | Field String Quoted String Tag
  deriving (Show, Eq)

data Hud =
  Hud
    { dispname :: String
    , path :: String -- change to filepath type later
    , hudlayout :: Node
    , clientscheme :: Node
    } deriving (Show, Eq)
-- TODO finish

data Feature =
  Feature
    { name :: String
    , dependencies :: [DepExpr]
    } deriving (Show, Eq)


data DepExpr
  = BlockCopy String String -- TODO make this list so we can copy nested blocks (maybe not needed)
  | FileCopy String
  | AnimationCopy String
  deriving (Show, Eq)
  
  


-- start with thing the recreates every file
-- then change it so it only recreates the needed files
-- then change it so that it splices in the text lines (with a parser that preserves line numbers)
-- and diffs old/new data structures? idk


-- IDEA maybe it only generates new files, then you like drag the folder manually, see if that works t opatch it.
-- like let windows do the diff. (not that manual alg is that much harder, but easier to start with/test)

getVals :: String -> Node -> [String]
getVals search (Field name quoted val tag) | name == search = [val]
getVals search (Field name quoted val tag) = []
getVals search (Block name quoted nodes tag) = concat (map (getVals search) nodes)

