module HelVM.HelPA.Assemblers.Util where

toIO :: Either String a -> IO a
toIO (Right value)  = return value
toIO (Left message) = fail message
