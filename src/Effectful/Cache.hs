{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Cache (
  Cache (..),
  insert,
  insert',
  lookup,
  lookup',
  keys,
  delete,
  filterWithKey,
  purge,
  purgeExpired,
  size,
  defaultExpiration,
  setDefaultExpiration,
  runCache,
  runCacheWith,
) where

import qualified Data.Cache as C
import Data.Hashable (Hashable)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Effectful (
  Dispatch (Dynamic),
  DispatchOf,
  Eff,
  Effect,
  IOE,
  MonadIO (liftIO),
  type (:>),
 )
import Effectful.Dispatch.Dynamic (interpret, send)
import System.Clock (TimeSpec)
import Prelude hiding (lookup)

data Cache k v :: Effect where
  Insert
    :: Hashable k
    => k
    -> v
    -> Cache k v m ()
  Insert'
    :: Hashable k
    => Maybe TimeSpec
    -> k
    -> v
    -> Cache k v m ()
  Lookup
    :: Hashable k
    => k
    -> Cache k v m (Maybe v)
  Lookup'
    :: Hashable k
    => k
    -> Cache k v m (Maybe v)
  Keys
    :: Hashable k => Cache k v m [k]
  Delete
    :: Hashable k
    => k
    -> Cache k v m ()
  FilterWithKey
    :: Hashable k
    => (k -> v -> Bool)
    -> Cache k v m ()
  Purge
    :: Hashable k
    => Cache k v m ()
  PurgeExpired
    :: Hashable k
    => Cache k v m ()
  Size
    :: Hashable k
    => Cache k v m Int
  DefaultExpiration
    :: Hashable k
    => Cache k v m (Maybe TimeSpec)
  SetDefaultExpiration
    :: Hashable k
    => Maybe TimeSpec
    -> Cache k v m ()

type instance DispatchOf (Cache k v) = Dynamic

insert
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> v
  -> Eff es ()
insert k v = send (Insert k v :: Cache k v (Eff es) ())

insert'
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Maybe TimeSpec
  -> k
  -> v
  -> Eff es ()
insert' ts k v = send (Insert' ts k v :: Cache k v (Eff es) ())

lookup
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> Eff es (Maybe v)
lookup k = send (Lookup k :: Cache k v (Eff es) (Maybe v))

-- | Like 'lookup' but never evicts the expired entry it read.
lookup'
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> Eff es (Maybe v)
lookup' k = send (Lookup' k :: Cache k v (Eff es) (Maybe v))

keys
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es [k]
keys = send (Keys :: Cache k v (Eff es) [k])

delete
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> Eff es ()
delete k = send (Delete k :: Cache k v (Eff es) ())

filterWithKey
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => (k -> v -> Bool)
  -> Eff es ()
filterWithKey p = send (FilterWithKey p :: Cache k v (Eff es) ())

purge
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es ()
purge = send (Purge :: Cache k v (Eff es) ())

purgeExpired
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es ()
purgeExpired = send (PurgeExpired :: Cache k v (Eff es) ())

size
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es Int
size = send (Size :: Cache k v (Eff es) Int)

defaultExpiration
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es (Maybe TimeSpec)
defaultExpiration = send (DefaultExpiration :: Cache k v (Eff es) (Maybe TimeSpec))

setDefaultExpiration
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Maybe TimeSpec
  -> Eff es ()
setDefaultExpiration ts = send (SetDefaultExpiration ts :: Cache k v (Eff es) ())

-- | Run against a fresh store with the given default expiration.
runCache
  :: forall k v es a
   . IOE :> es
  => Maybe TimeSpec
  -> Eff (Cache k v : es) a
  -> Eff es a
runCache ts eff = do
  c <- liftIO (C.newCache ts)
  runCacheWith c eff

-- | Run against an existing 'C.Cache', e.g. one shared with non-effectful code.
runCacheWith
  :: forall k v es a
   . IOE :> es
  => C.Cache k v
  -> Eff (Cache k v : es) a
  -> Eff es a
runCacheWith c0 eff = do
  -- the ref only exists so SetDefaultExpiration can swap the record; the
  -- underlying store is shared by every copy.
  ref <- liftIO (newIORef c0)
  interpret (\_ op -> withCache ref op) eff

withCache
  :: IOE :> es
  => IORef (C.Cache k v)
  -> Cache k v m a
  -> Eff es a
withCache ref op = do
  c <- liftIO (readIORef ref)
  liftIO $ case op of
    Insert k v -> C.insert c k v
    Insert' ts k v -> C.insert' c ts k v
    Lookup k -> C.lookup c k
    Lookup' k -> C.lookup' c k
    Keys -> C.keys c
    Delete k -> C.delete c k
    FilterWithKey p -> C.filterWithKey p c
    Purge -> C.purge c
    PurgeExpired -> C.purgeExpired c
    Size -> C.size c
    DefaultExpiration -> pure (C.defaultExpiration c)
    SetDefaultExpiration ts -> writeIORef ref (C.setDefaultExpiration c ts)
