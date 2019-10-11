{-# LANGUAGE DataKinds #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.GCD.Solver #-}

module GHC.TypeLits.GCD.Test where

import Data.Proxy
import GHC.TypeLits.GCD

test1 :: Proxy (GCD 6 8) -> Proxy 2
test1 = id
