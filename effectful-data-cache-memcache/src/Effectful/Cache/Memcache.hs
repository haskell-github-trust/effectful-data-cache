{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Interpret 'Cache' against a memcached server via "Database.Memcache.Client".
--
-- > c <- newClient [def] def
-- > runEff . runCacheMemcache codec c Nothing $ insert "a" (1 :: Int)
--
-- memcached cannot enumerate its items, so 'Keys', 'FilterWithKey' and 'Size'
-- throw 'MemcacheUnsupported'. Expired items are never returned, so 'Lookup''
-- behaves like 'Lookup', and 'PurgeExpired' is a no-op.
module Effectful.Cache.Memcache (
  Codec (..),
  MemcacheUnsupported (..),
  runCacheMemcache,
) where

import Control.Exception (Exception, throwIO)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import qualified Database.Memcache.Client as M
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.Cache (Cache (..))
import Effectful.Dispatch.Dynamic (interpret)
import System.Clock (Clock (Realtime), TimeSpec (..), getTime, toNanoSecs)

-- | How keys and values are stored. A value that fails to decode is a miss.
--
-- @since 0.1.0.0
data Codec k v = Codec
  { encodeKey :: k -> ByteString
  , encodeValue :: v -> ByteString
  , decodeValue :: ByteString -> Maybe v
  }

-- | Thrown by operations memcached has no equivalent for.
--
-- @since 0.1.0.0
newtype MemcacheUnsupported = MemcacheUnsupported String
  deriving Show

instance Exception MemcacheUnsupported

-- | Run against a memcached client with the given default expiration.
--
-- @since 0.1.0.0
runCacheMemcache
  :: forall k v es a
   . IOE :> es
  => Codec k v
  -> M.Client
  -> Maybe TimeSpec
  -> Eff (Cache k v : es) a
  -> Eff es a
runCacheMemcache codec c ts eff = do
  ref <- liftIO (newIORef ts)
  interpret (\_ op -> liftIO (withMemcache codec c ref op)) eff

withMemcache
  :: Codec k v
  -> M.Client
  -> IORef (Maybe TimeSpec)
  -> Cache k v m a
  -> IO a
withMemcache codec c ref = \case
  Insert k v -> readIORef ref >>= \ts -> set ts k v
  Insert' ts k v -> set ts k v
  Lookup k -> get k
  Lookup' k -> get k
  Keys -> unsupported "keys"
  Delete k -> del k
  FilterWithKey _ -> unsupported "filterWithKey"
  Purge -> M.flush c Nothing
  PurgeExpired -> pure ()
  Size -> unsupported "size"
  DefaultExpiration -> readIORef ref
  SetDefaultExpiration ts -> writeIORef ref ts
 where
  key = encodeKey codec
  get k = (>>= \(v, _, _) -> decodeValue codec v) <$> M.get c (key k)
  del k = void (M.delete c (key k) 0)
  set ts k v =
    expiration ts >>= \case
      Nothing -> del k
      Just e -> void (M.set c (key k) (encodeValue codec v) 0 e)
  unsupported op = throwIO (MemcacheUnsupported op)

-- | memcached expirations are whole seconds, 0 never expires, and values over
-- 30 days are unix timestamps. 'Nothing' means already expired.
expiration :: Maybe TimeSpec -> IO (Maybe Word32)
expiration Nothing = pure (Just 0)
expiration (Just ts)
  | secs <= 0 = pure Nothing
  | secs <= thirtyDays = pure (Just (fromInteger secs))
  | otherwise = Just . fromInteger . (+ secs) . toInteger . sec <$> getTime Realtime
 where
  secs = negate (toNanoSecs ts `div` negate 1_000_000_000)
  thirtyDays = 30 * 24 * 3600
