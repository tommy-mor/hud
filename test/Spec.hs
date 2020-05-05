import Test.HUnit
import Parse
import Lib
import Text.Parsec

main :: IO ()
main = do
  _ <- runTestTT $ TestList $ parseTests ++ parseFeatureTests
  return ()

t msg expe res = TestLabel msg $ TestCase (assertEqual msg res expe)
parseTests =
  [ t "test basic"
      (parse
         block
         "test basic"
         "\"Resource/UI/HudPlayerHealth.res\"{\t\n\t// player health data\n\t\"HudPlayerHealth\"\n\t{\n\n\t\t\"ControlName\"\t\"EditablePanel\"\n\t\t\"fieldName\"\t\t\"HudPlayerHealth\"\n\t\t\"xpos\"\t\t\t\"0\"\t\t[$WIN32]\n\t\t}\n\t\t}") $
    Right $
    Block
      "Resource/UI/HudPlayerHealth.res"
      [ Block
          "HudPlayerHealth"
          [ Field "ControlName" "EditablePanel" Nothing
          , Field "fieldName" "HudPlayerHealth" Nothing
          , Field "xpos" "0" (Just "WIN32")
          ]
          Nothing
      ]
      Nothing
  , t "test field 1"
      (parse field "test field 1" "\t\t\"ControlName\"\t\"ImagePanel\"") $
    Right $ Field "ControlName" "ImagePanel" Nothing
  , t "test field 2"
      (parse field "test field 1" "\t\t\"xpos_minmode\"\t\t\t\"73\"\t[$WIN32]") $
    Right $ Field "xpos_minmode" "73" (Just "WIN32")
  , t "test field 2"
      (parse field "test field 1" "\t\t\"xpos_minmode\"\t\t\t\"73\"\t[$X360]") $
    Right $ Field "xpos_minmode" "73" (Just "X360")
  , t "test block empty"
      (parse block "test basic" "\tHudDamageIndicator\n\t{\n\n\t}") $
    Right (Block "starst" [] Nothing)
  , t "test block with tag"
      (parse
         block
         "test basic"
         "\tHudDamageIndicator [$WIN32]\n\t{\n\n\"xpos\" \"100\"\n\t}") $
    Right
      (Block "HudDamageIndicator" [Field "xpos" "100" Nothing] (Just "WIN32"))
  ]

parseFeatureTests =
  [ t "test feature parse 1"
      (parse
         depexprs
         "ammo feature"
         "(block-copy \"resource/hudlayout.res\" \"HudAmmoWeapons\")") $
    Right $ [BlockCopy "resource/hudlayout.res" "HudAmmoWeapons"]
  , t "test feature parse 2"
      (parse
         depexprs
         "ammo feature"
         "#test comment \n(file-copy \"resource/ui/hudammoweapons.res\")\n(block-copy \"resource/hudlayout.res\" \"HudAmmoWeapons\")\n#another comment\n\n#another one\n\n\n#neat\n\n(animation-copy \"HudLowAmmoPulse\")\n\n#huh\n\n\n") $
    Right
      [ FileCopy "resource/ui/hudammoweapons.res"
      , BlockCopy "resource/hudlayout.res" "HudAmmoWeapons"
      , AnimationCopy "HudLowAmmoPulse"
      ]
  ]



-- maybe do some tests where it reads a bunch of .res files, and repros them, then makes sure diff is only whitespace/comments
