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
