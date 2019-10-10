{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HelloTypecheck.Plugin where

import GhcPlugins

-- import Plugins
import TcRnTypes
import TcPluginM

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install,
	tcPlugin = const $ Just myTcPlugin
	}

defaultTcPlugin :: [CommandLineOption] -> Maybe TcPlugin
defaultTcPlugin = const Nothing

myTcPlugin :: TcPlugin
myTcPlugin = TcPlugin {
	tcPluginInit = do
		tcPluginTrace "foobar" (ppr "baz")
		return (),
	tcPluginSolve = const $ myTypechecker,
	tcPluginStop = const $ return ()
	}

myTypechecker :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
myTypechecker _ _ [] = do
	tcPluginTrace "foobar" (ppr "baz")
	return $ TcPluginOk [] []
myTypechecker g _ w = do
	tcPluginTrace "given" (ppr g)
	tcPluginTrace "wanted" (ppr w)
	return $ TcPluginOk [] []


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opt todo = do
	putMsgS `mapM_` opt
	putMsgS "Hello, Typechecker!"
	return todo
