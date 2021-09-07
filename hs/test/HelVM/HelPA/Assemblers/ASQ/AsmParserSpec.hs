module HelVM.HelPA.Assemblers.ASQ.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.ASQ.AsmParser
import           HelVM.HelPA.Assemblers.ASQ.FileUtil
import           HelVM.HelPA.Assemblers.ASQ.Instruction

import           HelVM.Common.Safe

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                             (Spec, describe, it)

spec :: Spec
spec = do

  describe "parseAssemblyFile" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileText fileName
    forM_ [ "esolangs" </> "helloWorld"
          , "mazonka"  </> "echo"
          , "mazonka"  </> "hi"
          , "mazonka"  </> "helloWorld"
          , "mazonka"  </> "siout"
          , "mazonka"  </> "si"
          ] $ \fileName -> do
      let parseAssembly = parseAssemblyFile $ buildAbsolutePathToAsqFile fileName
      it fileName $ do safeIOToPTextIO parseAssembly `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyText" </> fileName)

  describe "parseAssemblyText" $ do
    forM_ [ ("\n"                         , [Instruction Code []])
          , (". \"\\n\";"                 , [Instruction Data [ItemString "\n"]])
          , (". \"Hello, World!\"\n"      , [Instruction Data [ItemString "Hello, World!"]])
          , (". H: \"Hello, World!\n\"\n" , [Instruction Data [ItemLabel "H" , ItemString "Hello, World!\n"]])
          , (". \"Hello\" \"World!\"\n"   , [Instruction Data [ItemString "Hello" , ItemString "World!"]])
          ] $ \(line , il) -> do
      it line $ do parseAssemblyText (toText line) `shouldSafe` il
