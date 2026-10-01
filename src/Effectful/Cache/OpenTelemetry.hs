{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | OpenTelemetry instrumentation for 'Cache', layered over any interpreter.
--
-- > runEff . runCache Nothing . traceCache tracer $ do ...
module Effectful.Cache.OpenTelemetry (
  traceCache,
) where

import Data.Maybe (isJust)
import Data.Text (Text)
import Effectful
import Effectful.Cache (Cache (..))
import Effectful.Dispatch.Dynamic (interpose, passthrough)
import OpenTelemetry.Trace.Core (
  Tracer,
  addAttribute,
  defaultSpanArguments,
  inSpan',
 )

-- | Wrap every cache operation in a span. Keys and values are never recorded:
-- unbounded cardinality, and likely user data.
--
-- @since 0.1.0.1
traceCache
  :: forall k v es a
   . Cache k v :> es
  => IOE :> es
  => Tracer
  -> Eff es a
  -> Eff es a
traceCache tracer = interpose @(Cache k v) $ \env op ->
  inSpan' tracer (opName op) defaultSpanArguments $ \sp -> do
    r <- passthrough env op
    case op of
      Lookup _ -> addAttribute sp "cache.hit" (isJust r)
      Lookup' _ -> addAttribute sp "cache.hit" (isJust r)
      Size -> addAttribute sp "cache.size" r
      Keys -> addAttribute sp "cache.size" (length r)
      _ -> pure ()
    pure r

opName
  :: Cache k v m a
  -> Text
opName = \case
  Insert{} -> "cache.insert"
  Insert'{} -> "cache.insert"
  Lookup{} -> "cache.lookup"
  Lookup'{} -> "cache.lookup"
  Keys{} -> "cache.keys"
  Delete{} -> "cache.delete"
  FilterWithKey{} -> "cache.filterWithKey"
  Purge{} -> "cache.purge"
  PurgeExpired{} -> "cache.purgeExpired"
  Size{} -> "cache.size"
  DefaultExpiration{} -> "cache.defaultExpiration"
  SetDefaultExpiration{} -> "cache.setDefaultExpiration"
