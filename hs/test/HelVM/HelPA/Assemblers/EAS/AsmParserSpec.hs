module HelVM.HelPA.Assemblers.EAS.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.EAS.AsmParser
import           HelVM.HelPA.Assemblers.EAS.FileUtil
import           HelVM.HelPA.Assemblers.EAS.Instruction
import           HelVM.HelPA.Assemblers.EAS.TestData

import           HelVM.Expectations

import           HelVM.HelPA.Assembler.Value

import           Test.Hspec                             (Spec, describe, it)

spec :: Spec
spec = do
  describe "parseAssemblyFile" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileText fileName
    forM_ [ ("true"     , trueIL)
          , ("hello"    , helloIL)
          , ("pip"      , pipIL)
          , ("pip2"     , pip2IL)
          , ("reverse"  , reverseIL)
          , ("function" , functionIL)
          , ("writestr" , writeStrIL)
          , ("hello2"   , hello2IL <> [D "writestr.eas"])
          , ("hello3"   , hello2IL <> [D "writestr.eas"])
          , ("hello4"   , hello4IL <> [D "writestr.eas"])
          , ("writenum" , writeNumIL)
          , ("multiply" , multiplyIL)
          , ("readnum"  , readNumIL)
          , ("fact"     , factIL   <> [D "readnum.eas",D "writenum.eas",D "multiply.eas",D "writestr.eas"])
          , ("bottles"  , bottlesIL)
          , ("euclid"   , euclidIL)
          ] $ \(fileName , il) -> do
      let parseAssembly = parseAssemblyFile $ buildAbsolutePathToEasFile fileName
      it fileName $ do parseAssembly `shouldSafeIO` il

  describe "empty" $ do
    it "parse ''" $ do parseAssemblyText "" `shouldSafe` []

  describe "Short Commands" $ do
    forM_ [ ("E"              , [E])
          , ("T"              , [T])
          , ("A"              , [A])
          , ("O"              , [O])
          , ("I"              , [I])
          , ("S"              , [S])
          , ("H"              , [H])
          , ("N0"             , [N (Literal 0)])
          , ("N00"            , [N (Literal 0)])
          , ("N1"             , [N (Literal 1)])
          , ("N01"            , [N (Literal 01)])
          , ("N10"            , [N (Literal 10)])
          , ("N' "            , [N (Literal 32)])
          , ("N''"            , [N (Literal 39)])
          , ("N'0"            , [N (Literal 48)])
          , ("N'N"            , [N (Literal 78)])
          , ("N<label"        , [N (Variable "label")])
          , ("*label\n"       , [D "label"])
          , (">label:"        , [L "label"])
          , ("\"label\""      , [U "label"])
          , (">LOOP: Input"   , [L "LOOP",I])
          , (">WRITE: Output" , [L "WRITE",O])
          , ("\n"             , [R])
          ] $ \(line , il) -> do
      describe line $ do
        it "without space"   $ do parseAssemblyText (toText line       ) `shouldSafe` il
        it "with prespace"   $ do parseAssemblyText (" " <> toText line) `shouldSafe` il
        it "with postspace"  $ do parseAssemblyText (toText line <> " ") `shouldSafe` il

  describe "Long Commends" $ do
    forM_ [ ("dividE"             , [E])
          , ("Transfer"           , [T])
          , ("Address"            , [A])
          , ("Output"             , [O])
          , ("Input"              , [I])
          , ("Substract"          , [S])
          , ("Halibut"            , [H])
          , ("Number 0"           , [N (Literal 0)])
          , ("Number 00"          , [N (Literal 0)])
          , ("Number 1"           , [N (Literal 1)])
          , ("Number 01"          , [N (Literal 1)])
          , ("Number 10"          , [N (Literal 10)])
          , ("Number ' "          , [N (Literal 32)])
          , ("Number ''"          , [N (Literal 39)])
          , ("Number '0"          , [N (Literal 48)])
          , ("Number 'N"          , [N (Literal 78)])
          , ("Number <label"      , [N (Variable "label")])
          , ("A N0 T \nA N0 T \n" , [A,N (Literal 0),T,R,A,N (Literal 0),T,R])
          , (" A N0 T \n "        , [A , N (Literal 0), T , R])
          , ( "# A N0 T \n "      , [])
          , (" #A N0 T \n "       , [R])
          , (" A# N0 T \n "       , [A , R])
          , (" A #N0 T \n "       , [A , R])
          ] $ \(line , il) -> do
      describe line $ do
        it "without space"   $ do parseAssemblyText (toText line       ) `shouldSafe` il
--        it "with prespace"   $ do parseAssemblyText (" " <> toText line) `shouldSafe` il
--        it "with postspace"  $ do parseAssemblyText (toText line <> " ") `shouldSafe` il
