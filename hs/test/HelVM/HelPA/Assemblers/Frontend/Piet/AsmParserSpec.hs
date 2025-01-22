module HelVM.HelPA.Assemblers.Frontend.Piet.AsmParserSpec (spec) where

import           HelVM.HelPA.Assemblers.Frontend.Piet.AsmParser
--import           HelVM.HelPA.Assemblers.Frontend.Piet.FileExtra
import           HelVM.HelPA.Assemblers.Frontend.Piet.Instruction

--import           HelVM.HelIO.Control.Safe

--import           HelVM.HelIO.Extra

import           HelVM.Expectations
--import           HelVM.GoldenExpectations

--import           System.FilePath.Posix

import           Test.Hspec                                       (Spec, describe, it)

spec :: Spec
spec = do
--  let parseAssemblyApp = "parseAssemblyApp"
--  let parseAssemblyFile fileName = parseAssemblyText <$> readFileTextUtf8 fileName
--  let parseAssemblyLang fileName = parseAssemblyFile (langDir </> fileName <.> lang)
--
--  describe parseAssemblyApp $ do
--      forM_ [ "fizzbuzz"
--            , "quest"
--            ] $ \ fileName ->
--        it fileName $
--          safeIOToPTextIO (parseAssemblyLang fileName) `goldenShouldIO` buildAbsolutePathToIlFile (parseAssemblyApp </> fileName)

  describe "Commands " $
    forM_ [ ("1"  , [Directive (PushNatural 1)])
          , ("2:" , [Directive (Label (LNatural 2))])
          , ("dup 15 mod" , [Command Dup,Directive (PushNatural 15),Command Mod])
          , ("dup 15 mod bz._track_3" , [Command Dup,Directive (PushNatural 15),Command Mod, Directive (Branch (Just BZ)  (BLIdentifier "_track_3"))])
          , ("dup outn 10 out" , [Command Dup, Command OutN, Directive (PushNatural 10),Command Out])
          , ("_track_0:"     , [Directive (Label (LIdentifier "_track_0"))])
          , ("1 add dup 100" , [Directive (PushNatural 1), Command Add, Command Dup, Directive (PushNatural 100)])
--          , ("1 add dup 100 brle.2b" , [Directive (PushNatural 1), Command Add, Command Dup, Directive (PushNatural 100)])
          , ("bz._track_3"   , [Directive (Branch (Just BZ) (BLIdentifier "_track_3"))])
          , ("ble._track_3"  , [Directive (Branch (Just BLE) (BLIdentifier "_track_3"))])
          , ("brz._track_3"  , [Directive (Branch (Just BZ) (BLIdentifier "_track_3"))])
          , ("brle._track_3" , [Directive (Branch (Just BLE) (BLIdentifier "_track_3"))])
          , ("brle.b" , [Directive (Branch (Just BLE) (BLIdentifier "b"))])
          , ("brle.2" , [Directive (Branch (Just BLE) (BLNatural Nothing 2))])
          , ("brle.2b" , [Directive (Branch (Just BLE) (BLNatural (Just Backward) 2))])
          , (".track" , [Directive Track])
          ] $ \(line , il) ->
      it line $ parseAssemblyText (toText line) `shouldSafe` il
