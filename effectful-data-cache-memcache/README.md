# effectful-data-cache-memcache

`runCacheMemcache` interprets `Cache` against memcached using the
[memcache](https://hackage.haskell.org/package/memcache) client.

```haskell
import Database.Memcache.Client (def, newClient)
import Effectful.Cache.Memcache

codec :: Codec String Int
codec = Codec BC.pack (BC.pack . show) (readMaybe . BC.unpack)

main = do
  c <- newClient [def] def
  runEff . runCacheMemcache codec c Nothing $ insert "key" (42 :: Int)
```

memcached cannot enumerate items: `keys`, `filterWithKey` and `size` throw
`MemcacheUnsupported`. `lookup'` behaves like `lookup` and `purgeExpired` is
a no-op. Expirations are rounded up to whole seconds.
