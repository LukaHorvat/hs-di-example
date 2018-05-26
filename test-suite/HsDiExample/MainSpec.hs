module HsDiExample.MainSpec where

import Data.Time (UTCTime, utctDay, addUTCTime)
import Test.Hspec
import Data.IORef (newIORef, modifyIORef, readIORef, IORef)
import Data.Function ((&))
import HsDiExample.Main
import Data.Functor.Identity (runIdentity)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Control.Effects
import Control.Monad.IO.Class

spec :: Spec
spec = describe "main" $ do

  logMessages <- runIO $ do
    logs <- newIORef []
    clockStart <- newIORef $ posixSecondsToUTCTime 0

    main
      & implement (LoggingMethods (\a -> liftIO $ modifyIORef logs (++ [a])))
      & implement (ArgsMethods (return ["sample.txt"]))
      & implement (FileMethods (\"sample.txt" -> return "Alyssa"))
      & implement (CurrentTimeMethods (readModifyIORef clockStart (addUTCTime 1)))

    readIORef logs


  it "prints two log messages" $
    length logMessages `shouldBe` 2

  it "prints a greeting as the first message" $
    (logMessages !! 0) `shouldBe` "Hello, Alyssa!"

  it "prints the elapsed time in milliseconds as the second message" $
    (logMessages !! 1) `shouldBe` "1000 milliseconds"


readModifyIORef :: IORef a -> (a -> a) -> IO a
readModifyIORef ref phi = do
  val <- readIORef ref
  modifyIORef ref phi
  return val

noop :: Monad m => m ()
noop = return ()
