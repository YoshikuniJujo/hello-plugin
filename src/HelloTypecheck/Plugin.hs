{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HelloTypecheck.Plugin where

import Prelude hiding ((<>))

import GhcPlugins

-- import Plugins
import TcRnTypes
import TcPluginM

import TyCoRep

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install,
	tcPlugin = const $ Just myTcPlugin
--	tcPlugin = myTcPlugin
	}

defaultTcPlugin :: [CommandLineOption] -> Maybe GhcPlugins.TcPlugin
defaultTcPlugin = const Nothing

myTcPlugin :: TcRnTypes.TcPlugin
myTcPlugin = TcPlugin {
	tcPluginInit = do
		tcPluginTrace "HelloTypecheck.Plugin: init" "baz"
		return (),
	tcPluginSolve = const $ myTypechecker,
	tcPluginStop = const $ do
		tcPluginTrace "HelloTypecheck.Plugin: stop" "foobar"
		return ()
	}

myTypechecker :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
myTypechecker _ _ [] = do
	tcPluginTrace "HelloTypecheck.Plugin: empty wanted" "baz"
	return $ TcPluginOk [] []
myTypechecker g _ w = do
	tcPluginTrace "HelloTypecheck.Plugin: given" (ppr g)
	tcPluginTrace "GIVEN: " . vcat $ getNatEquality <$> g
	tcPluginTrace "HelloTypecheck.Plugin: wanted" (ppr w)
	tcPluginTrace "WANTED: " . vcat $ getNatEquality <$> w
	return $ TcPluginOk [] []


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opt todo = do
	putMsgS `mapM_` opt
	putMsgS "Hello, Typechecker!"
	return todo

getNatEquality :: Ct -> SDoc
getNatEquality ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 ->
		"EqPred NomEq (" <> showType t1 <> ") (" <> showType t2 <> ")"
	_ -> "NO EqPred"

showType :: Type -> SDoc
showType (TyVarTy v) = text "TyVarTy" <+> ppr v
showType t = text "Other Type" <+> ppr t
