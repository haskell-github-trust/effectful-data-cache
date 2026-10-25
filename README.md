# effectful-data-cache

```haskell
import Effectful
import Effectful.Cache
import Prelude hiding (lookup)

main :: IO ()
main = runEff . runCache Nothing $ do
  insert "key" (42 :: Int)
  v <- lookup @String @Int "key"
  liftIO $ print v
```

`runCache` creates a fresh store; `runCacheWith` wraps an existing
`Data.Cache.Cache` so it can be shared with non-effectful code.

## Tracing

See [effectful-data-cache-opentelemetry](effectful-data-cache-opentelemetry)
for OpenTelemetry instrumentation.

## memcached

See [effectful-data-cache-memcache](effectful-data-cache-memcache) for a
memcached backend.
