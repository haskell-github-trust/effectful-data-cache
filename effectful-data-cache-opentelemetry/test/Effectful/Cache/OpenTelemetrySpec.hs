{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Effectful.Cache.OpenTelemetrySpec (spec) where

import Data.IORef (readIORef)
import Data.Text (Text)
import Effectful
import Effectful.Cache
import Effectful.Cache.OpenTelemetry (traceCache)
import OpenTelemetry.Attributes (Attribute (..), PrimitiveAttribute (..), lookupAttribute)
import OpenTelemetry.Exporter.InMemory.Span (inMemoryListExporter)
import OpenTelemetry.Trace (
  ImmutableSpan (..),
  createTracerProvider,
  emptyTracerProviderOptions,
  makeTracer,
  shutdownTracerProvider,
  tracerOptions,
 )
import OpenTelemetry.Trace.Core (SpanHot (..))
import Test.Hspec
import Prelude hiding (lookup)

-- | Run the given cache program under a traced interpreter, returning its
-- result plus every span that was exported.
traced
  :: Eff '[Cache String Int, IOE] a
  -> IO (a, [SpanHot])
traced prog = do
  (processor, ref) <- inMemoryListExporter
  provider <- createTracerProvider [processor] emptyTracerProviderOptions
  let tracer = makeTracer provider "effectful-data-cache-test" tracerOptions
  r <- runEff . runCache @String @Int Nothing $ traceCache @String @Int tracer prog
  _ <- shutdownTracerProvider provider Nothing
  spans <- traverse (readIORef . spanHot) . reverse =<< readIORef ref
  pure (r, spans)

attr
  :: SpanHot
  -> Text
  -> Maybe Attribute
attr s = lookupAttribute (hotAttributes s)

spec :: Spec
spec = do
  it "emits a span per operation" $ do
    (_, spans) <- traced $ do
      insert @String @Int "a" (1 :: Int)
      _ <- lookup @String @Int "a"
      purge @String @Int
    fmap hotName spans `shouldBe` ["cache.insert", "cache.lookup", "cache.purge"]

  it "records hits and misses" $ do
    (_, spans) <- traced $ do
      insert @String @Int "a" (1 :: Int)
      _ <- lookup @String @Int "a"
      _ <- lookup @String @Int "nope"
      pure ()
    let hits = [attr s "cache.hit" | s <- spans, hotName s == "cache.lookup"]
    hits
      `shouldBe` [ Just (AttributeValue (BoolAttribute True))
                 , Just (AttributeValue (BoolAttribute False))
                 ]

  it "records the size" $ do
    (_, spans) <- traced $ do
      insert @String @Int "a" (1 :: Int)
      insert @String @Int "b" (2 :: Int)
      _ <- size @String @Int
      pure ()
    [attr s "cache.size" | s <- spans, hotName s == "cache.size"]
      `shouldBe` [Just (AttributeValue (IntAttribute 2))]

  it "still returns the underlying result" $ do
    (r, _) <- traced $ do
      insert @String @Int "a" (1 :: Int)
      lookup @String @Int "a"
    r `shouldBe` Just 1
