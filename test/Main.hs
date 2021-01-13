{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}

module Main (main) where

import           Test.Hspec

import           Control.Concurrent     (forkIO, threadDelay)
import           Control.Exception      (Exception (fromException))
import           Control.Exception.Base (IOException, SomeException)
import           Data.Maybe             (isJust)
import           Data.Pool
import           Debug.Trace            (traceShow)

main :: IO ()
main = hspec $ do
  describe "acquiring resources" $ do
    it "acquires a single resource when available" $
      canAcquireSingleItem `shouldReturn` Just ()
    it "timeout exception is thrown if acquiring takes too long" $
      acquiringThrowsWhenTimeoutExpired `shouldReturn` Nothing
    it "correctly releases and re-acquires resources" $
      acquireReleaseAndReAcquire `shouldReturn` Just ()
    it "waits for timeout before giving up" $
      acquireWaitAndRelease `shouldReturn` Just ()
    it "waits for timeout and gives up" $
      acquireAndWaitTooLong `shouldReturn` Nothing

mkPool :: IO (Pool ())
mkPool =
  createPool
    (pure ())
    (const (pure ()))
    stripes
    idleTime
    maxResources
    timeout
  where
    stripes = 1
    idleTime = 1000
    maxResources = 1
    timeout = Just 1

canAcquireSingleItem :: IO (Maybe ())
canAcquireSingleItem =
  mkPool >>= fmap (fmap fst) . takeResource

acquiringThrowsWhenTimeoutExpired :: IO (Maybe ())
acquiringThrowsWhenTimeoutExpired = do
  pool <- mkPool
  _ <- takeResource pool
  fmap fst <$> takeResource pool

acquireReleaseAndReAcquire :: IO (Maybe ())
acquireReleaseAndReAcquire = do
  pool <- mkPool
  takeResource pool >>= \case
    Nothing -> pure Nothing
    Just (resource, lp) -> do
      putResource lp resource
      fmap fst <$> takeResource pool

acquireWaitAndRelease :: IO (Maybe ())
acquireWaitAndRelease = do
  pool <- mkPool
  Just (res, lp) <- takeResource pool
  _ <- forkIO do
    threadDelay 300_000
    putResource lp res
  fmap fst <$> takeResource pool

acquireAndWaitTooLong :: IO (Maybe ())
acquireAndWaitTooLong = do
  pool <- mkPool
  Just (res, lp) <- takeResource pool
  _ <- forkIO do
    threadDelay 2_000_000
    putResource lp res
  fmap fst <$> takeResource pool
