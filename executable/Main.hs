module Main where

import System.Environment as Env
import HsDiExample.Main hiding (main)
import qualified HsDiExample.Main as HsDiExample
import Control.Monad.IO.Class
import Control.Effects
import Data.Function ((&))
import qualified Data.Text.IO as T
import Data.Time as Time

main :: IO ()
main = HsDiExample.main
    & implement (FileMethods (liftIO . T.readFile))
    & implement (LoggingMethods (liftIO . T.putStrLn))
    & implement (ArgsMethods (liftIO Env.getArgs))
    & implement (CurrentTimeMethods (liftIO Time.getCurrentTime))
