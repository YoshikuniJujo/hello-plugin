{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=HelloGadts.Plugin #-}

module HelloGadts.Test where

data Exp a where
	Zero :: Exp Int

eval :: Exp a -> a
eval Zero = (0 :: Int)

run :: IO ()
run = print $ eval Zero

data Foo a where
	FooZero :: Foo a

foo :: Foo a -> a
foo FooZero = (0 :: Int)
