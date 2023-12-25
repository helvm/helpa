module HelVM.HelPA.Assemblers.Frontend.FBF.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.FBF.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.FBF.FileExtra

import           HelVM.HelIO.Control.Safe
import           HelVM.HelIO.Extra

import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                    (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyApp = "parseAssemblyApp"
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
  let parseAssemblyLang fileName = parseAssemblyFile (langDir </> fileName <.> lang)

  describe parseAssemblyApp $ do
      forM_ [ "Bubblesort"
            , "FindTheNumber"
            , "Groop"
            , "Lightass"
            , "Random"
            , "Stack"
            , "Stones"
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyLang fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fileName)
