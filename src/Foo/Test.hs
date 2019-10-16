{-# LANGUAGE DataKinds, KindSignatures #-}
{-# OPTIONS_GhC -Wall -fno-warn-tabs -fplugin=Foo.Plugin #-}

module Foo.Test where

import GHC.TypeLits
import Data.Proxy

class Foo a

foo :: Foo a => a -> a
foo = id

-- three :: Int
-- three = foo 3

class Bar (n :: Nat)

bar :: Bar n => Proxy n -> Proxy n
bar = id

-- eight :: Proxy 8
-- eight = bar Proxy

class Baz (s :: Symbol)

baz :: Baz s => Proxy s -> Proxy s
baz = id

-- hello :: Proxy "hello"
-- hello = baz Proxy

-- world :: Proxy "world"
-- world = baz Proxy
