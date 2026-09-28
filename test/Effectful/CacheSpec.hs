{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Effectful.CacheSpec (spec) where

import Data.List (sort)
import Effectful
import Effectful.Cache
import System.Clock (TimeSpec (..))
import Test.Hspec
import Prelude hiding (lookup)

run :: Maybe TimeSpec -> Eff '[Cache String Int, IOE] a -> IO a
run ts = runEff . runCache ts

expired :: Maybe TimeSpec
expired = Just (TimeSpec (-1) 0)

spec :: Spec
spec = do
  it "looks up what it inserted" $ do
    r <- run Nothing $ do
      insert "a" (1 :: Int)
      lookup @String @Int "a"
    r `shouldBe` Just 1

  it "misses on an absent key" $ do
    r <- run Nothing $ lookup @String @Int "nope"
    r `shouldBe` Nothing

  it "deletes" $ do
    r <- run Nothing $ do
      insert "a" (1 :: Int)
      delete @String @Int "a"
      lookup @String @Int "a"
    r `shouldBe` Nothing

  it "lists keys and size" $ do
    (ks, n) <- run Nothing $ do
      insert "a" (1 :: Int)
      insert "b" (2 :: Int)
      (,) <$> keys @String @Int <*> size @String @Int
    sort ks `shouldBe` ["a", "b"]
    n `shouldBe` 2

  it "honours the default expiration" $ do
    r <- run expired $ do
      insert "a" (1 :: Int)
      lookup @String @Int "a"
    r `shouldBe` Nothing

  it "honours a per-entry expiration" $ do
    r <- run Nothing $ do
      insert' expired "a" (1 :: Int)
      lookup @String @Int "a"
    r `shouldBe` Nothing

  it "evicts on lookup but not on lookup'" $ do
    (afterLookup', afterLookup) <- run expired $ do
      insert "a" (1 :: Int)
      _ <- lookup' @String @Int "a"
      n1 <- size @String @Int
      _ <- lookup @String @Int "a"
      n2 <- size @String @Int
      pure (n1, n2)
    afterLookup' `shouldBe` 1
    afterLookup `shouldBe` 0

  it "filters with key" $ do
    ks <- run Nothing $ do
      insert "keep" (1 :: Int)
      insert "drop" (2 :: Int)
      filterWithKey @String @Int (\k _ -> k == "keep")
      keys @String @Int
    ks `shouldBe` ["keep"]

  it "purges everything" $ do
    n <- run Nothing $ do
      insert "a" (1 :: Int)
      insert "b" (2 :: Int)
      purge @String @Int
      size @String @Int
    n `shouldBe` 0

  it "purges only expired entries" $ do
    n <- run Nothing $ do
      insert' expired "old" (1 :: Int)
      insert "new" (2 :: Int)
      purgeExpired @String @Int
      size @String @Int
    n `shouldBe` 1

  it "round-trips the default expiration" $ do
    (old, new) <- run Nothing $ do
      b <- defaultExpiration @String @Int
      setDefaultExpiration @String @Int (Just (TimeSpec 5 0))
      a <- defaultExpiration @String @Int
      pure (b, a)
    old `shouldBe` Nothing
    new `shouldBe` Just (TimeSpec 5 0)
