#!/usr/bin/env -S just --justfile

packages := "effectful-data-cache effectful-data-cache-opentelemetry effectful-data-cache-memcache"
dirs := "src test effectful-data-cache-opentelemetry/src effectful-data-cache-opentelemetry/test effectful-data-cache-memcache/src effectful-data-cache-memcache/test"

default:
    @just --list

# Build and run tests (memcache tests need memcached on localhost:11211)
test:
    stack build --test --ghc-options=-Werror

# Format sources with fourmolu
fmt:
    fourmolu -i {{dirs}}

# Lint sources with hlint
lint:
    hlint {{dirs}}

# Build Haddock tarballs for Hackage
docs:
    stack haddock --haddock-for-hackage

[private]
check-env:
    @test -n "${HACKAGE_KEY:-}" || { echo "HACKAGE_KEY is not set" >&2; exit 1; }

# Upload packages and docs as Hackage candidates
candidate *pkgs=packages: check-env docs
    for p in {{pkgs}}; do stack upload --candidate --test-tarball "$p" && stack upload --candidate --documentation "$p" || exit 1; done

# Publish packages and docs to Hackage
release *pkgs=packages: check-env docs
    for p in {{pkgs}}; do stack upload --test-tarball "$p" && stack upload --documentation "$p" || exit 1; done
