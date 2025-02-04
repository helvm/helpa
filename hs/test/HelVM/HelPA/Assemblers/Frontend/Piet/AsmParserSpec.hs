module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser
import           HelVM.HelPA.Assemblers.Frontend.Piet.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

import           HelVM.HelIO.Control.Safe

import           HelVM.HelIO.Extra

import           HelVM.Expectations
import           HelVM.GoldenExpectations

import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
  let parseAssemblyApp = "parseAssemblyApp"
  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
  let parseAssemblyLang fileName = parseAssemblyFile (langDir </> fileName <.> lang)

  describe parseAssemblyApp $ do
      forM_ [ "fizzbuzz"
--            , "quest"
            ] $ \ fileName ->
        it fileName $
          safeIOToPTextIO (parseAssemblyLang fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fileName)

  describe "Commands " $
    forM_ [ ("1"                           , [Directive (PushNatural 1)])
          , ("2:"                          , [Directive (Label (LNatural 2))])
          , ("dup 15 mod bz._track_3"      , [Command Dup , Directive (PushNatural 15) , Command Mod , Directive (Branch (Just BZ) (BLIdentifier "_track_3"))])
          , ("dup 3 mod bz._track_1"       , [Command Dup , Directive (PushNatural 3) , Command Mod , Directive (Branch (Just BZ) (BLIdentifier "_track_1"))])
          , ("dup 5 mod bz._track_2"       , [Command Dup , Directive (PushNatural 5) , Command Mod , Directive (Branch (Just BZ) (BLIdentifier "_track_2"))])
          , ("dup outn 10 out"             , [Command Dup , Command OutN , Directive (PushNatural 10) , Command Out])
          , ("_track_0:"                   , [Directive (Label (LIdentifier "_track_0"))])
          , ("1 add dup 100 brle.2b"       , [Directive (PushNatural 1) , Command Add , Command Dup , Directive (PushNatural 100) , Directive (Branch (Just BLE) (BLNatural (Just Backward) 2))])
          , ("halt"                        , [Directive Halt])
          , (".track #1"                   , [Directive Track])
          , ("@\"Fizz\n\" br._track_0"     , [Directive (Print "Fizz\n") , Directive (Branch Nothing (BLIdentifier "_track_0"))])
          , (".track #2"                   , [Directive Track])
          , ("@\"Buzz\n\" br._track_0"     , [Directive (Print "Buzz\n") , Directive (Branch Nothing (BLIdentifier "_track_0"))])
          , (".track #3"                   , [Directive Track])
          , ("@\"FizzBuzz\n\" br._track_0" , [Directive (Print "FizzBuzz\n") , Directive (Branch Nothing (BLIdentifier "_track_0"))])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
