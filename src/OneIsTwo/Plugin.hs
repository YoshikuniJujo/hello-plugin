{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module OneIsTwo.Plugin where

import Control.Arrow ((&&&))
import Data.Maybe

import GhcPlugins
import TcPluginM
import TcRnTypes
import TyCoRep
import TcEvidence

import GHC.TcPluginM.Extra

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = showCore,
	tcPlugin = const . Just $ TcPlugin {
		tcPluginInit = return (),
		tcPluginSolve = const $ solveOneIsTwo,
		tcPluginStop = const $ return () } }

showCore :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
showCore _ todo = do
	putMsgS "Hello!"
	putMsg . foldr1 ($$) $ ppr <$> todo
	return $ corePlugin : todo ++ [corePlugin]
	where
	corePlugin = CoreDoPluginPass "hogehogehoge" myCoreDoPlugin

myCoreDoPlugin :: CorePluginPass
myCoreDoPlugin mg = do
	putMsgS "Woops!"
	putMsgS "------------------------"
	putMsg . ppr $ mg_module mg
	putMsgS . show $ mg_hsc_src mg
	putMsg . ppr $ mg_tcs mg
	putMsg . ppr $ mg_binds mg
	putMsgS "------------------------"
	return mg

solveOneIsTwo :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveOneIsTwo _ _ [] = return $ TcPluginOk [] []
solveOneIsTwo gs _ ws = do
	tcPluginTrace "!OneIsTwo.Plugin:" ""
	tcPluginTrace "Given: " $ ppr gs
	tcPluginTrace "Wanted: " $ ppr ws
	tcPluginTrace "look wanted: " . cat . catMaybes $ lookOneIsTwo <$> ws
	Just s <- case ws of
		[src] -> Just <$> someCt src
		_ -> return Nothing
	tcPluginTrace "someCt: " $ ppr s
	let	ws' = filter checkOneIsTwo ws
--	return $ TcPluginContradiction ws
--	return $ TcPluginOk ((fromJust . evMagic &&& id) <$> ws') []
--	return $ TcPluginOk ((evByFiat "foo" one two ,) <$> ws') []
--	return $ TcPluginOk ((evByFiat "foo" two one ,) <$> ws') [s]
--	return $ TcPluginOk ((evByFiat "foo" three four ,) <$> ws') [s]
--	return $ TcPluginOk ((evByFiat' "foo" (litTyStr "foofoofoo") (litTyStr "foobarbaz") ,) <$> ws') [s]
	return $ TcPluginOk ((evByFiat' "foobooboo" (litTyStr "foofoofoobooboo") (litTyStr "foobarbazbooboo") ,) <$> ws') []
--	return $ TcPluginOk [] []

litTyNum :: Integer -> Type
litTyNum = LitTy . NumTyLit

litTyStr :: FastString -> Type
litTyStr = LitTy . StrTyLit

evByFiat' :: String -> Type -> Type -> EvTerm
evByFiat' name t1 t2 = EvExpr $ Coercion $ mkUnivCo (PluginProv name) Nominal t1 t2

one, two, three, four :: Type
[one, two] = litTyNum <$> [1, 2]
three = LitTy (NumTyLit 3)
four = LitTy (NumTyLit 4)

someCt :: Ct -> TcPluginM Ct
someCt src = mkIrredCt <$> GHC.TcPluginM.Extra.newWanted (ctLoc src) (TyConApp c [n1, n2, three, four])
	where
	CIrredCan (CtWanted (TyConApp c [n1, n2, _, _]) _dst _ns _lc) _ = src

lookOneIsTwo :: Ct -> Maybe SDoc
lookOneIsTwo (CIrredCan (CtWanted (LitTy pr) dst _ns _lc) ins) =
	Just $ "CIrredCan (CtWanted " <+> "(LitTy" <+>
		ppr pr <+> ")" <+> ppr dst <+> "<ShadowInfo>" <+> "<CtLoc>" <+> ")" <+> ppr ins
lookOneIsTwo (CIrredCan (CtWanted (TyVarTy pr) dst _ns _lc) ins) =
	Just $ "CIrredCan (CtWanted " <+> "(TyVarTy" <+>
		ppr pr <+> ")" <+> ppr dst <+> "<ShadowInfo>" <+> "<CtLoc>" <+> ")" <+> ppr ins
lookOneIsTwo (CIrredCan (CtWanted pr@(TyConApp c as) dst _ns _lc) ins) = Just $
	"CIrredCan (CtWanted " <+> "(TyConApp" <+>
		ppr c <+> ppr as <+> ")" <+> ppr dst <+> "<ShadowInfo>" <+> "<CtLoc>" <+> ")" <+> ppr ins $$
	"classifyPredType pr: " <+> getEqPred pr
	where getEqPred p = case classifyPredType p of
		EqPred NomEq t1 t2 -> ppr t1 <+> ppr t2
		_ -> "not EqPred NomEq _ _"
lookOneIsTwo (CIrredCan (CtWanted pr dst _ns _lc) ins) =
	Just $ "CIrredCan (CtWanted " <+>
		ppr pr <+> ppr dst <+> "<ShadowInfo>" <+> "<CtLoc>" <+> ")" <+> ppr ins
lookOneIsTwo _ = Nothing

checkOneIsTwo :: Ct -> Bool
checkOneIsTwo (CIrredCan (CtWanted pr _ _ _) _) = case classifyPredType pr of
	EqPred NomEq (LitTy (NumTyLit 1)) (LitTy (NumTyLit 2)) -> True
	_ -> False
checkOneIsTwo _ = False

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just $ evByFiat "hello-plugin" t1 t2
	_ -> Nothing


