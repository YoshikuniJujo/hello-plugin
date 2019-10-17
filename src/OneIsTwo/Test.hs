{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs -fplugin=OneIsTwo.Plugin #-}

module OneIsTwo.Test where

import Data.Proxy

oneIsTwo :: Proxy 1 -> Proxy 2
oneIsTwo = id

{-
useOneIsTwo :: Proxy 1 -> Proxy 1
useOneIsTwo p = case oneIsTwo p of
	p' -> p'
	-}

-- twoIsOne :: Proxy 2 -> Proxy 1
-- twoIsOne = id
