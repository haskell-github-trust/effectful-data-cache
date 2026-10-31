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

## Development

Requires [just](https://github.com/casey/just), stack, fourmolu 0.21.0.0 and hlint.

```sh
just build   # build and test; memcache tests need: docker run -d -p 11211:11211 memcached:1.6
just fmt     # format with fourmolu
just lint    # lint with hlint
just docs    # build Haddock tarballs
```

Releasing needs `HACKAGE_KEY` set:

```sh
just candidate [pkgs...]  # upload Hackage candidates
just release [pkgs...]    # publish to Hackage
```
