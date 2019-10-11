{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module GHC.TypeLits.GCD (GCD) where

import GHC.TypeLits (Nat)

type family GCD (x :: Nat) (y :: Nat) :: Nat where
	GCD 0 x = x
