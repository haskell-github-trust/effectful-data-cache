# AGENTS

Haskell library (stack + hpack). Edit `package.yaml`, never generated `.cabal`.

## Commands

- build and test (GHC 9.10): `stack build --test --ghc-options=-Werror`
- format: `fourmolu -i src test effectful-data-cache-opentelemetry/{src,test}` (fourmolu 0.21.0.0, config in `fourmolu.yaml`)
- lint: `hlint src test effectful-data-cache-opentelemetry/{src,test}` (rules in `.hlint.yaml`; HLS has no hlint plugin on GHC 9.10)

CI (`.github/workflows/stack.yml`) runs all of the above; keep it green.

## Style

- `fmap` over `map`, `<>` over `++` (enforced by hlint)
- curried constraints: `Read a => Ord a => a`, not `(Read a, Ord a) => a`;
  not enforced by tooling
- `-Wredundant-constraints` on: only add `Show e` for `throwError` where needed
