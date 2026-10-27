{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | 'Cache' interpreter over a 'HashMap' in a 'TVar', without "Data.Cache".
module Effectful.Cache.STM (
  Store,
  newStore,
  runCacheSTM,
  runCacheSTMWith,
) where

import Control.Concurrent.STM (
  STM,
  TVar,
  atomically,
  modifyTVar',
  newTVarIO,
  readTVar,
  writeTVar,
 )
import qualified Data.HashMap.Strict as HM
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Cache (Cache (..))
import Effectful.Dispatch.Dynamic (interpret)
import System.Clock (Clock (Monotonic), TimeSpec, getTime)

-- | Entries with their absolute expiration, and the default expiration.
--
-- @since 0.2.2.0
data Store k v = Store (TVar (HM.HashMap k (v, Maybe TimeSpec))) (TVar (Maybe TimeSpec))

-- | Fresh store with the given default expiration.
--
-- @since 0.2.2.0
newStore :: Maybe TimeSpec -> IO (Store k v)
newStore ts = Store <$> newTVarIO HM.empty <*> newTVarIO ts

-- | Run against a fresh store with the given default expiration.
--
-- @since 0.2.2.0
runCacheSTM
  :: forall k v es a
   . IOE :> es
  => Maybe TimeSpec
  -> Eff (Cache k v : es) a
  -> Eff es a
runCacheSTM ts eff = do
  s <- liftIO (newStore ts)
  runCacheSTMWith s eff

-- | Run against an existing 'Store', e.g. one shared between threads.
--
-- @since 0.2.2.0
runCacheSTMWith
  :: forall k v es a
   . IOE :> es
  => Store k v
  -> Eff (Cache k v : es) a
  -> Eff es a
runCacheSTMWith s = interpret $ \_ op -> liftIO $ do
  now <- getTime Monotonic
  atomically (withStore s now op)

withStore :: Store k v -> TimeSpec -> Cache k v m a -> STM a
withStore (Store ref def) now op = case op of
  Insert k v -> readTVar def >>= \ts -> modifyTVar' ref (HM.insert k (v, (now +) <$> ts))
  Insert' ts k v -> modifyTVar' ref (HM.insert k (v, (now +) <$> ts))
  Lookup k -> do
    m <- HM.lookup k <$> readTVar ref
    case m of
      Just (v, e) | live e -> pure (Just v)
      Just _ -> Nothing <$ modifyTVar' ref (HM.delete k)
      Nothing -> pure Nothing
  Lookup' k -> do
    m <- HM.lookup k <$> readTVar ref
    pure $ case m of
      Just (v, e) | live e -> Just v
      _ -> Nothing
  Keys -> HM.keys <$> readTVar ref
  Delete k -> modifyTVar' ref (HM.delete k)
  FilterWithKey p -> modifyTVar' ref (HM.filterWithKey (\k (v, _) -> p k v))
  Purge -> writeTVar ref HM.empty
  PurgeExpired -> modifyTVar' ref (HM.filter (live . snd))
  Size -> HM.size <$> readTVar ref
  DefaultExpiration -> readTVar def
  SetDefaultExpiration ts -> writeTVar def ts
 where
  live = maybe True (> now)
