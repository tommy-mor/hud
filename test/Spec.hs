import Test.HUnit
import Lib
import Text.Parsec

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ parseTests
  return ()

t msg expe res = TestLabel msg $ TestCase (assertEqual msg res expe)
parseTests =
  [ t "test basic"
      (parse
         block
         "test basic"
         "\"Resource/UI/HudPlayerHealth.res\"{\t\n\t// player health data\n\t\"HudPlayerHealth\"\n\t{\n\n\t\t\"ControlName\"\t\"EditablePanel\"\n\t\t\"fieldName\"\t\t\"HudPlayerHealth\"\n\t\t\"xpos\"\t\t\t\"0\"\t\t[$WIN32]\n\t\t}\n\t\t}") $
    Right $ Block "Resource/UI/HudPlayerHealth.res" []
  , t "test field 1"
      (parse field "test field 1" "\t\t\"ControlName\"\t\"ImagePanel\"") $
    Right $ Field "ControlName" "ImagePanel" Nothing
  , t "test field 2"
      (parse field "test field 1" "\t\t\"xpos_minmode\"\t\t\t\"73\"\t[$WIN32]") $
    Right $ Field "xpos_minmode" "73" (Just "WIN32")
  , t "test field 2"
      (parse field "test field 1" "\t\t\"xpos_minmode\"\t\t\t\"73\"\t[$X360]") $
    Right $ Field "xpos_minmode" "73" (Just "X360")

  ]
