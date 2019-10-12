{-# LANGUAGE ScopedTypeVariables, DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KnownNat.Sample where

import GHC.TypeLits
import Data.Proxy

-- f :: forall n . KnownNat n => Proxy n -> Integer
-- f _ = natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy (n + 2))
