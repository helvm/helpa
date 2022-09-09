module HelVM.HelPA.Assemblers.ASQ.EsoLangs.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.AsmParser
import           HelVM.HelPA.Assemblers.ASQ.EsoLangs.Instruction

import           HelVM.HelPA.Assemblers.ASQ.FileUtil

import           HelVM.HelIO.Control.Safe

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                      (Spec, describe, it)

spec :: Spec
spec = do

  describe "parseAssemblyFile" $ do
    let parseAssemblyFile fileName = parseAssemblyText <$> readFileText fileName
    forM_ [ "esolangs" </> "currentAddress" </> "helloWorld"
          , "esolangs" </> "nextAddress"    </> "echo"
          , "esolangs" </> "nextAddress"    </> "hi"
          , "esolangs" </> "nextAddress"    </> "helloWorld"
          , "esolangs" </> "nextAddress"    </> "siout"
          , "esolangs" </> "nextAddress"    </> "si"
          ] $ \fileName -> do
      let parseAssembly = parseAssemblyFile $ buildAbsolutePathToAsqFile fileName
      it fileName $ safeIOToPTextIO parseAssembly `goldenShouldIO` buildAbsolutePathToIlFile ("parseAssemblyFile" </> fileName)

  describe "parseAssemblyText" $
    forM_ [ ("\n"                         , [Instruction Code []])
          , (". \"\\n\";"                 , [Instruction Data [ItemString "\n"]])
          , (". \"Hello, World!\"\n"      , [Instruction Data [ItemString "Hello, World!"]])
          , (". H: \"Hello, World!\n\"\n" , [Instruction Data [ItemLabel "H" , ItemString "Hello, World!\n"]])
          , (". \"Hello\" \"World!\"\n"   , [Instruction Data [ItemString "Hello" , ItemString "World!"]])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
