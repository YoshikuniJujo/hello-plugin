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
	tcPluginTrace "HelloTypecheck.Plugin: empty wanted" (ppr "baz")
	return $ TcPluginOk [] []
myTypechecker g _ w = do
	tcPluginTrace "HelloTypecheck.Plugin: given" (ppr g)
	tcPluginTrace "GIVEN: " . text . unlines $ getNatEquality <$> g
	tcPluginTrace "HelloTypecheck.Plugin: wanted" (ppr w)
	tcPluginTrace "WANTED: " . text . unlines $ getNatEquality <$> w
	return $ TcPluginOk [] []


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opt todo = do
	putMsgS `mapM_` opt
	putMsgS "Hello, Typechecker!"
	return todo

getNatEquality :: Ct -> String
getNatEquality ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 ->
		"EqPred NomEq (" ++ showSDocUnsafe (ppr t1) ++ ") (" ++ showSDocUnsafe (ppr t2) ++ ")"
	_ -> "NO EqPred"
