# effectful-data-cache-opentelemetry

`traceCache` wraps every operation in an OpenTelemetry span
(`cache.insert`, `cache.lookup`, …), with `cache.hit` on lookups and
`cache.size` on `size`/`keys`. Keys and values are never recorded.

```haskell
import Effectful.Cache.OpenTelemetry (traceCache)

main = runEff . runCache Nothing . traceCache tracer $ do
  insert "key" (42 :: Int)
```

It is an interposer, so it layers over any interpreter and can be omitted
without touching call sites.
