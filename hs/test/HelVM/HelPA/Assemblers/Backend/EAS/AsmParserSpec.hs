module HelVM.HelPA.Assemblers.Backend.EAS.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Backend.EAS.AsmParser
import           HelVM.HelPA.Assemblers.Backend.EAS.FileExtra
import           HelVM.HelPA.Assemblers.Backend.EAS.Instruction
import           HelVM.HelPA.Assemblers.Backend.EAS.TestData

import           HelVM.HelIO.Extra

import           HelVM.Expectations

import           HelVM.HelPA.Assembler.Value

import           Test.Hspec                                     (Spec, describe, it)

spec :: Spec
spec = do
  describe "parseAssemblyApp" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
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
      it fileName $ parseAssembly `shouldSafeIO` il

  describe "empty" $
    it "parse ''" $ parseAssemblyText "" `shouldSafe` []

  describe "Short Commands" $
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
          ] $ \(line , il) ->
      describe line $ do
        it "without space"   $ parseAssemblyText (toText line       ) `shouldSafe` il
        it "with prespace"   $ parseAssemblyText (" " <> toText line) `shouldSafe` il
        it "with postspace"  $ parseAssemblyText (toText line <> " ") `shouldSafe` il

  describe "Long Commends" $
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
          , ("# A N0 T \n "       , [])
          , (" #A N0 T \n "       , [R])
          , (" A# N0 T \n "       , [A , R])
          , (" A #N0 T \n "       , [A , R])
          ] $ \(line , il) ->
      describe line $
        it "without space"   $ parseAssemblyText (toText line       ) `shouldSafe` il
