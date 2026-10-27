module Effectful.Cache.STMSpec (spec) where

import Effectful (runEff)
import Effectful.Cache.STM (runCacheSTM)
import Effectful.CacheSpec (specWith)
import Test.Hspec (Spec)

spec :: Spec
spec = specWith (\ts -> runEff . runCacheSTM ts)
