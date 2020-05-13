module Main where

import Lib
import Parse
import System.IO
import Text.Parsec
import System.Directory
import System.FilePath
import Control.Monad.Except



fname = "/home/tommy/programming/clones/tf2basehud"
coolfname = "/home/tommy/programming/clones/rayshud"

main :: IO ()
main
  -- taken from learn you a haskell
  -- http://learnyouahaskell.com/input-and-output
 = do
  --handle <-
    --openFile
      --"/home/tommy/programming/clones/tf2basehud/scripts/hudlayout.res"
      --ReadMode
  --contents <- hGetContents handle
  --putStr $ mergeEither $ first show $ showFormat <$> (parse block "hudlayout.res" contents)
  --handle2 <- openFile "/home/tommy/programming/hud/src/features/ammo.hud" ReadMode
  --contents2 <- hGetContents handle2
  --let features = (Feature "name") <$> parse depexprs "ammo feature" contents2
  --putStrLn $ mergeEither $ first show $ (showFeature <$> features)
  --hClose handle2
  v <- runExceptT mainhelp
  putStrLn (mergeEither v)

mainhelp :: ExceptT String IO String
mainhelp = do
  basehud <- makeHud fname
  coolhud <- makeHud coolfname
  return (show coolhud)


  
  

-- makes hud from filepath,   fname/resource/scripts
-- returns nothing if it doesn't work (change to either for better errors eventually)
makeHud :: FilePath -> ExceptT String IO Hud
makeHud fname = do
  isDir <- lift (doesDirectoryExist fname)
  if not isDir
    then fail "directory does not exist or is filename"
    else do
      let dispname = takeBaseName fname
          hudlayoutFname = fname </> "scripts" </> "hudlayout.res"
          clientschemeFname = fname </> "resource" </> "clientscheme.res"
      hudlayoutHandle <- lift $ openFile hudlayoutFname ReadMode
      hudl <- lift $ hGetContents hudlayoutHandle
      clientschemeHandle <- lift $ openFile clientschemeFname ReadMode
      client <- lift $ hGetContents clientschemeHandle
      (case ( parse block ("hudlayout.res, " ++ dispname) hudl
            , parse block ("clientscheme.res, " ++ dispname) client) of
         (Right hudlnode, Right clientsnode) -> return $ Hud
              { dispname = dispname
              , path = fname
              , hudlayout = hudlnode
              , clientscheme = clientsnode
              }
         (Left error, Right _) -> fail $ "parse failed: " ++ show error
         (Right _, Left error) -> fail $ "parse failed: " ++ show error
         (Left error1, Left error2) ->
           fail $
           "both parses failed: " ++ show error1 ++ "\n------\n" ++ show error2)

test = do
  let fname = "/home/tommy/programming/clones/tf2basehud/resource/clientscheme.res"
  hudlayoutHandle <- openFile fname ReadMode
  hudl <- hGetContents hudlayoutHandle
  return $ parse block "hudl" hudl

  


          
      
    
-- write code outline in main that:
-- reads in base hud clientscheme and hudlayout from directory filepath
-- runs the ammo.hud expressions with guest hud.

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
    showFormatHelp n (Block name isQuoted children tag) =
      (tabs n) ++
      (case isQuoted of
         Quoted -> (quoted name)
         Unquoted -> name) ++
      (tabs 1) ++
      (maybe "" tagged tag) ++
      newl ++
      (tabs n) ++
      "{" ++
      newl ++
      (unwords . map (showFormatHelp (n + 1)) $ children) ++
      (tabs n) ++ "}" ++ newl
    showFormatHelp n (Field name isQuoted val tag) =
      (tabs n) ++
      (case isQuoted of
         Quoted -> (quoted name)
         Unquoted -> name) ++
      (tabs 1) ++ (quoted val) ++ (tabs 1) ++ (maybe "" tagged tag) ++ "\n"
