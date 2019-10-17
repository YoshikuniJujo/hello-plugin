{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=HelloGadtsWithInt.Plugin #-}

module HelloGadtsWithInt.Test where

data Foo a where
	FooZero :: Foo a

foo :: Foo a -> a
foo FooZero = (0 :: Int)

-- some :: Int -> Char
-- some = id
