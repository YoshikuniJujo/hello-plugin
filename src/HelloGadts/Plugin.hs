{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HelloGadts.Plugin where

import GhcPlugins

import TcPluginM
import TcRnTypes
import TyCoRep
import TcEvidence

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = showCore,
	tcPlugin = const . Just $ TcPlugin {
		tcPluginInit = return (),
		tcPluginSolve = const solveFoo,
		tcPluginStop = const $ return () } }

showCore :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
showCore _ todo = do
	return $ corePlugin : todo

corePlugin :: CoreToDo
corePlugin = CoreDoPluginPass "hogehogehoge" $ \mg -> do
	putMsgS "Woops!"
	putMsgS "--------------------"
	putMsg . ppr $ mg_binds mg
	putMsgS "--------------------"
	return mg

solveFoo :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveFoo _ _ [] = return $ TcPluginOk [] []
solveFoo gs _ ws = do
	tcPluginTrace "!HelloGadts.Plugin:" ""
	tcPluginTrace "gs: " $ ppr gs
	tcPluginTrace "ws: " $ ppr ws
--	return $ TcPluginOk [] []
	return $ TcPluginOk ((myEvTerm ,) <$> ws) []

myEvTerm :: EvTerm
myEvTerm = evByFiat "hogehoge" (litTyStr "foobarbaz") (litTyStr "hogehogehoge")

litTyStr :: FastString -> Type
litTyStr = LitTy . StrTyLit

evByFiat :: String -> Type -> Type -> EvTerm
evByFiat name t1 t2 = EvExpr . Coercion $ mkUnivCo (PluginProv name) Nominal t1 t2
