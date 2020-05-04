module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


data Block = Block String [Field] deriving (Show)
data Field =
  Field String String (Maybe String)
  deriving (Show)
