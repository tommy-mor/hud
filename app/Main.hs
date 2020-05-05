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
      "/home/tommy/programming/clones/tf2basehud/scripts/hudlayout.res"
      ReadMode
  contents <- hGetContents handle
  putStr $ mergeEither $ first show $ showFormat <$> (parse block "hudlayout.res" contents)
  --handle2 <- openFile "/home/tommy/programming/hud/src/features/ammo.hud" ReadMode
  --contents2 <- hGetContents handle2
  --let features = (Feature "name") <$> parse depexprs "ammo feature" contents2
  --putStrLn $ mergeEither $ first show $ (showFeature <$> features)
  hClose handle

-- helper function for converting the error type of Eithers
-- we do all this because we want to keep the derived show function
first :: (a -> c) -> Either a b -> Either c b
first fn (Left a) = Left $ fn a
first _ (Right b) = Right b

mergeEither (Left a) = a
mergeEither (Right a) = a

tabs n = ['\t' | _ <- [1..n]]
quoted s = "\"" ++ s ++ "\""
tagged s = "[$" ++ s ++ "]"
newl = "\n"


-- temp for enforcing type
showFeature :: Feature -> String
showFeature = show


showFormat :: Node -> String
showFormat = showFormatHelp 0
  where
    showFormatHelp n (Block name children tag) =
      (tabs n) ++
      (quoted name) ++
      (tabs 1) ++
      (maybe "" tagged tag) ++
      newl ++
      (tabs n) ++
      "{" ++
      newl ++
      (unwords . map (showFormatHelp (n + 1)) $ children) ++
      (tabs n) ++ "}" ++ newl
    showFormatHelp n (Field name val tag) =
      (tabs n) ++
      quoted name ++
      (tabs 1) ++ (quoted val) ++ (tabs 1) ++ (maybe "" tagged tag) ++ "\n"
