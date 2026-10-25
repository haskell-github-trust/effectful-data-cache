{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications #-}

module Effectful.Cache.MemcacheSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import qualified Data.ByteString.Char8 as BC
import qualified Database.Memcache.Client as M
import Effectful
import Effectful.Cache
import Effectful.Cache.Memcache
import System.Clock (TimeSpec (..))
import Test.Hspec
import Text.Read (readMaybe)
import Prelude hiding (lookup)

codec :: Codec String Int
codec = Codec BC.pack (BC.pack . show) (readMaybe . BC.unpack)

connect :: IO (Maybe M.Client)
connect = do
  c <- M.newClient [M.def] M.def
  r <- try @SomeException (M.version c)
  pure (c <$ either (const Nothing) Just r)

withClient :: (M.Client -> IO ()) -> Maybe M.Client -> IO ()
withClient _ Nothing = pendingWith "memcached not reachable on 127.0.0.1:11211"
withClient f (Just c) = M.flush c Nothing >> f c

run :: M.Client -> Maybe TimeSpec -> Eff '[Cache String Int, IOE] a -> IO a
run c ts = runEff . runCacheMemcache codec c ts

spec :: Spec
spec = beforeAll connect $ do
  it "looks up what it inserted" . withClient $ \c -> do
    r <- run c Nothing $ insert "a" (1 :: Int) >> lookup @String @Int "a"
    r `shouldBe` Just 1

  it "misses on an absent key" . withClient $ \c -> do
    r <- run c Nothing $ lookup @String @Int "nope"
    r `shouldBe` Nothing

  it "deletes" . withClient $ \c -> do
    r <- run c Nothing $ do
      insert "a" (1 :: Int)
      delete @String @Int "a"
      lookup @String @Int "a"
    r `shouldBe` Nothing

  it "purges" . withClient $ \c -> do
    r <- run c Nothing $ do
      insert "a" (1 :: Int)
      purge @String @Int
      lookup @String @Int "a"
    r `shouldBe` Nothing

  it "treats a non-positive expiration as already expired" . withClient $ \c -> do
    r <- run c Nothing $ do
      insert "a" (1 :: Int)
      insert' (Just (TimeSpec (-1) 0)) "a" (2 :: Int)
      lookup @String @Int "a"
    r `shouldBe` Nothing

  it "expires after the default expiration" . withClient $ \c -> do
    (d, r) <- run c (Just (TimeSpec 0 1)) $ do
      insert "a" (1 :: Int)
      liftIO (threadDelay 2_100_000)
      (,) <$> defaultExpiration @String @Int <*> lookup @String @Int "a"
    d `shouldBe` Just (TimeSpec 0 1)
    r `shouldBe` Nothing

  it "sets the default expiration" . withClient $ \c -> do
    d <- run c Nothing $ do
      setDefaultExpiration @String @Int (Just (TimeSpec 5 0))
      defaultExpiration @String @Int
    d `shouldBe` Just (TimeSpec 5 0)

  it "misses on an undecodable value" . withClient $ \c -> do
    _ <- M.set c (BC.pack "a") (BC.pack "junk") 0 0
    r <- run c Nothing $ lookup @String @Int "a"
    r `shouldBe` Nothing

  it "rejects keys" . withClient $ \c ->
    run c Nothing (keys @String @Int) `shouldThrow` \(MemcacheUnsupported _) -> True
