{-# LANGUAGE GADTs, DataKinds, KindSignatures, TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=PlusOne.Plugin #-}

module PlusOne.Test where

import GHC.TypeLits

infixr 5 :::

data List :: Nat -> * -> * where
	Nil :: List 0 a
	(:::) :: a -> List length a -> List (length + 1) a
--	(:::) :: a -> List (length - 1) a -> List length a

deriving instance Show a => Show (List l a)

-- tail_ :: List (n + 1) a -> List n a
-- tail_ (_ ::: as) = as
