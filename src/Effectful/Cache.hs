{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Dynamic effect over "Data.Cache", an in-memory key/value store with
-- optional per-item expiration.
--
-- > runEff . runCache @Text @Int Nothing $ do
-- >   insert "a" 1
-- >   lookup @Text @Int "a"
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

-- | Cache of values @v@ indexed by keys @k@.
--
-- @since 0.1.0.0
data Cache k v :: Effect where
  -- | See 'insert'.
  Insert
    :: Hashable k
    => k
    -> v
    -> Cache k v m ()
  -- | See 'insert''.
  Insert'
    :: Hashable k
    => Maybe TimeSpec
    -> k
    -> v
    -> Cache k v m ()
  -- | See 'lookup'.
  Lookup
    :: Hashable k
    => k
    -> Cache k v m (Maybe v)
  -- | See 'lookup''.
  Lookup'
    :: Hashable k
    => k
    -> Cache k v m (Maybe v)
  -- | See 'keys'.
  Keys
    :: Hashable k => Cache k v m [k]
  -- | See 'delete'.
  Delete
    :: Hashable k
    => k
    -> Cache k v m ()
  -- | See 'filterWithKey'.
  FilterWithKey
    :: Hashable k
    => (k -> v -> Bool)
    -> Cache k v m ()
  -- | See 'purge'.
  Purge
    :: Hashable k
    => Cache k v m ()
  -- | See 'purgeExpired'.
  PurgeExpired
    :: Hashable k
    => Cache k v m ()
  -- | See 'size'.
  Size
    :: Hashable k
    => Cache k v m Int
  -- | See 'defaultExpiration'.
  DefaultExpiration
    :: Hashable k
    => Cache k v m (Maybe TimeSpec)
  -- | See 'setDefaultExpiration'.
  SetDefaultExpiration
    :: Hashable k
    => Maybe TimeSpec
    -> Cache k v m ()

type instance DispatchOf (Cache k v) = Dynamic

-- | Insert an item with the cache's default expiration.
--
-- @since 0.1.0.0
insert
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> v
  -> Eff es ()
insert k v = send (Insert k v :: Cache k v (Eff es) ())

-- | Insert an item with an explicit expiration, relative to now;
-- 'Nothing' never expires. The default expiration is ignored.
--
-- @since 0.1.0.0
insert'
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Maybe TimeSpec
  -> k
  -> v
  -> Eff es ()
insert' ts k v = send (Insert' ts k v :: Cache k v (Eff es) ())

-- | Look up a non-expired item, evicting it if expired.
--
-- @since 0.1.0.0
lookup
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> Eff es (Maybe v)
lookup k = send (Lookup k :: Cache k v (Eff es) (Maybe v))

-- | Like 'lookup' but never evicts the expired entry it read.
--
-- @since 0.1.0.0
lookup'
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> Eff es (Maybe v)
lookup' k = send (Lookup' k :: Cache k v (Eff es) (Maybe v))

-- | All keys in the store, including expired ones.
--
-- @since 0.1.0.0
keys
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es [k]
keys = send (Keys :: Cache k v (Eff es) [k])

-- | Delete an item; no-op if absent.
--
-- @since 0.1.0.0
delete
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => k
  -> Eff es ()
delete k = send (Delete k :: Cache k v (Eff es) ())

-- | Keep only items satisfying the predicate, which may be called
-- on expired items.
--
-- @since 0.1.0.0
filterWithKey
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => (k -> v -> Bool)
  -> Eff es ()
filterWithKey p = send (FilterWithKey p :: Cache k v (Eff es) ())

-- | Delete all items.
--
-- @since 0.1.0.0
purge
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es ()
purge = send (Purge :: Cache k v (Eff es) ())

-- | Delete all expired items, atomically.
--
-- @since 0.1.0.0
purgeExpired
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es ()
purgeExpired = send (PurgeExpired :: Cache k v (Eff es) ())

-- | Number of items, including expired ones.
--
-- @since 0.1.0.0
size
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es Int
size = send (Size :: Cache k v (Eff es) Int)

-- | Expiration applied by 'insert'.
--
-- @since 0.1.0.0
defaultExpiration
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Eff es (Maybe TimeSpec)
defaultExpiration = send (DefaultExpiration :: Cache k v (Eff es) (Maybe TimeSpec))

-- | Change the expiration applied by subsequent 'insert's.
--
-- @since 0.1.0.0
setDefaultExpiration
  :: forall k v es
   . Cache k v :> es
  => Hashable k
  => Maybe TimeSpec
  -> Eff es ()
setDefaultExpiration ts = send (SetDefaultExpiration ts :: Cache k v (Eff es) ())

-- | Run against a fresh store with the given default expiration.
--
-- @since 0.1.0.0
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
--
-- @since 0.1.0.0
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
