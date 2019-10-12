{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KnownNat.EvLevelAdd where

import Data.Proxy
import GHC.TypeLits

newtype SNatKn (n :: Nat) = SNatKn Integer

class KnownNatAdd (a :: Nat) (b :: Nat) where
	natSingAdd :: SNatKn (a + b)

instance (KnownNat a, KnownNat b) => KnownNatAdd a b where
	natSingAdd = SNatKn (natVal (Proxy @a) + natVal (Proxy @b))

{-
newtype SNat (n :: Nat) = SNat Integer

class KnownNat (n :: Nat) where natSing :: SNat n
-}
