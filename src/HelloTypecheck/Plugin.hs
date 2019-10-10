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
		tcPluginTrace "HelloTypecheck.Plugin: init" (ppr "baz")
		return (),
	tcPluginSolve = const $ myTypechecker,
	tcPluginStop = const $ do
		tcPluginTrace "HelloTypecheck.Plugin: stop" (ppr "foobar")
		return ()
	}

myTypechecker :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
myTypechecker _ _ [] = do
	tcPluginTrace "HelloTypeCheck.Plugin: empty wanted" (ppr "baz")
	return $ TcPluginOk [] []
myTypechecker g _ w = do
	tcPluginTrace "HelloTypecheck.Plugin: given" (ppr g)
	tcPluginTrace "HelloTypecheck.Plugin: wanted" (ppr w)
	return $ TcPluginOk [] []


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opt todo = do
	putMsgS `mapM_` opt
	putMsgS "Hello, Typechecker!"
	return todo
