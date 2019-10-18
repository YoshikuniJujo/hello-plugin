{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module HelloGadtsWithInt.Plugin where

import Data.Maybe

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
showCore _ = return . (corePlugin :)

corePlugin :: CoreToDo
corePlugin = CoreDoPluginPass "higehigehige" $ \mg -> do
	putMsgS "HELLO"
	putMsgS "-------------------------------------------------"
	putMsg . ppr $ mg_binds mg
	putMsgS "-------------------------------------------------"
	return mg

badEvTerm :: EvTerm
badEvTerm = evByFiat "!HelloGadtsWithInt.Plugin" (litTyStr "foobarbaz") (litTyStr "foofoofoo")

litTyStr :: FastString -> Type
litTyStr = LitTy . StrTyLit

evByFiat :: String -> Type -> Type -> EvTerm
evByFiat name t1 t2 = EvExpr . Coercion $ mkUnivCo (PluginProv name) Nominal t1 t2

evByFiatHere :: Type -> Type -> EvTerm
evByFiatHere = evByFiat "!HelloGadtsWithInt.Plugin"

solveFoo :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveFoo _ _ [] = return $ TcPluginOk [] []
solveFoo gs _ ws = do
	tcPluginTrace "!HelloGadtsWithInt.Plugin:" ""
	tcPluginTrace "gs: " $ ppr gs
	tcPluginTrace "ws: " $ ppr ws
	tcPluginTrace "lookWanted: " . cat . catMaybes $ lookWanted <$> ws
	return $ TcPluginOk (catMaybes $ mkEvTerm <$> ws) []
	{-
	case ws of
		[w] -> case getAAndInt w of
			Just (a, i) -> return $ TcPluginOk [(evByFiatHere a i, w)] []
--			Just (a, i) -> return $ TcPluginOk [(evByFiatHere i a, w)] []
--			Just (a, i) -> return $ TcPluginOk [(badEvTerm, w)] []
			Nothing -> return $ TcPluginOk [] []
		_ -> return $ TcPluginOk [] []
--	return $ TcPluginOk [] []
--	return $ TcPluginOk ((badEvTerm ,) <$> ws) []
	-}

lookWanted :: Ct -> Maybe SDoc
lookWanted (CIrredCan (CtWanted pr dst _ns _lc) ins) = Just
	$ "CIrredCan (CtWanted " <+> ppr pr <+> ppr dst <+> "<foo>" <+> "<bar>" <+> ")" <+> ppr ins
lookWanted (CNonCanonical (CtWanted pr dst _nsh _loc))
	| Just t <- lookType pr = Just $
		"CNonCanonical (CtWanted" <+> t <+> ppr dst <+> "<ctev_nosh>" <+> "<ctev_loc>" <+> ")"
lookWanted _ = Nothing

getAAndInt :: Ct -> Maybe (Type, Type)
getAAndInt (CNonCanonical (CtWanted pr _ _ _)) | Just t <- aAndInt pr = Just t
getAAndInt _ = Nothing

lookType :: Type -> Maybe SDoc
lookType (TyConApp tc as) = Just $ "TyConApp" <+> ppr tc <+> ppr as
lookType _ = Nothing

aAndInt :: Type -> Maybe (Type, Type)
aAndInt (TyConApp _ [_, _, a, i]) = Just (a, i)
aAndInt _ = Nothing

mkEvTerm :: Ct -> Maybe (EvTerm, Ct)
mkEvTerm ct@(CNonCanonical (CtWanted (TyConApp _ [_, _, a, i]) _ _ _)) = Just (
	EvExpr . Coercion
		$ mkUnivCo (PluginProv "!HelloGadtsWithInt.Plugin") Nominal a i,
	ct)
mkEvTerm _ = Nothing

{-
evByFiat :: String -> Type -> Type -> EvTerm
evByFiat name t1 t2 = EvExpr . Coercion $ mkUnivCo (PluginProv name) Nominal t1 t2

evByFiatHere :: Type -> Type -> EvTerm
evByFiatHere = evByFiat "!HelloGadtsWithInt.Plugin"
-}
