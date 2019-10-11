{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GCDExample where

import Data.Proxy
import GHC.TypeLits

type family Cmp123 (o :: Ordering) (x :: Nat) (y :: Nat) (z :: Nat) :: Nat where
	Cmp123 'LT x y z = x
	Cmp123 'EQ x y z = y
	Cmp123 'GT x y z = z

type family GCD (x :: Nat) (y :: Nat) :: Nat where
	GCD 0 x = x
	GCD x 0 = x
	GCD x y = Cmp123 (CmpNat x y) (GCD x (y - x)) x (GCD (x - y) y)

test :: Proxy (GCD 372 48) -> Proxy 12
test = id
