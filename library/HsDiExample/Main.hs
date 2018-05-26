{-# LANGUAGE TypeFamilies, DeriveGeneric, FlexibleContexts, PartialTypeSignatures #-}
module HsDiExample.Main where

import Prelude hiding (readFile)
import GHC.Generics
import Data.Time (UTCTime, diffUTCTime)
import Data.Text (Text, pack)
import Data.Semigroup ((<>))
import Control.Effects

data CurrentTime
instance Effect CurrentTime where
    data EffMethods CurrentTime m = CurrentTimeMethods
        { _getCurrentTime :: m UTCTime }
        deriving (Generic)
CurrentTimeMethods getCurrentTime = effect

data Args
instance Effect Args where
    data EffMethods Args m = ArgsMethods
        { _getArgs :: m [String] }
        deriving (Generic)
ArgsMethods getArgs = effect

data File
instance Effect File where
    data EffMethods File m = FileMethods
        { _readFile :: String -> m Text }
        deriving (Generic)
FileMethods readFile = effect

data Logging
instance Effect Logging where
    data EffMethods Logging m = LoggingMethods
        { _logInfo :: Text -> m () }
        deriving (Generic)
LoggingMethods logInfo = effect

main :: _ => m () -- I usually use partial signatures
-- main :: MonadEffects '[Logging, Args, File, CurrentTime] m => m () -- but you can do this instead
main = do
  startTime <- getCurrentTime
  [fileName] <- getArgs
  target <- readFile fileName
  logInfo $ "Hello, " <> target <> "!"
  endTime <- getCurrentTime
  let duration = endTime `diffUTCTime` startTime
  logInfo $ pack (show (round (duration * 1000) :: Integer)) <> " milliseconds"
