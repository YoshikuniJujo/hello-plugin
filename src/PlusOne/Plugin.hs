{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PlusOne.Plugin where

import Control.Arrow ((&&&))
import Data.Maybe

import GhcPlugins

import TcPluginM (TcPluginM, tcPluginTrace, newCoercionHole)
import TcRnTypes (
	Ct, TcPlugin(..), TcPluginResult(..), ctEvidence, ctEvPred, ShadowInfo(WDeriv),
	TcEvDest(..), CtEvidence(..), mkNonCanonical, ctLoc
	)

import TcEvidence (EvTerm)
import GHC.TcPluginM.Extra (evByFiat)
import TyCoRep (Type(..), TyLit(..))
import TcTypeNats (typeNatAddTyCon)

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just plusOnePlugin }

plusOnePlugin :: TcPlugin
plusOnePlugin = TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const $ solvePlusOne,
	tcPluginStop = const $ return () }

solvePlusOne :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solvePlusOne _ _ [] = do
--	tcPluginTrace "PlusOne.Plugin: " "wanted: empty"
	return $ TcPluginOk [] []
solvePlusOne given _ wanted = do
	tcPluginTrace "!PlusOne.Plugin: given" $ ppr given
	tcPluginTrace "!PlusOne.Plugin: wanted" $ ppr wanted
	tcPluginTrace "lookCt <$> given: " . cat $ lookCt <$> given
	tcPluginTrace "lookCt <$> given: " . cat $ lookEqAddition <$> given
	tcPluginTrace "lookCt <$> wanted: " . cat $ lookCt <$> wanted
	tcPluginTrace "Plus Ones: " . cat $ ppr <$> filterByTypes (isPlusOneOfTypesMulti wanted) given
	newWanted <- catMaybes <$> checkPlusOne given `mapM` wanted
	let	(ok, new) = unzip newWanted
	return $ TcPluginOk ((fromJust . evMagic &&& id) <$> ok) new

checkPlusOne :: [Ct] -> Ct -> TcPluginM (Maybe (Ct, Ct))
checkPlusOne given wanted = do
	let	b = doesExistPlusOne wanted given
	if b then ((wanted ,) <$>) <$> toPlusOneCt wanted else return Nothing

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just $ evByFiat "hello-plugin" t1 t2
	_ -> Nothing

lookCt :: Ct -> SDoc
lookCt ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> "EqPred NomEq " <+> ppr t1 <+> ppr t2
	_ -> "Other than (EqPred NomEq _ _)" <+> ppr ct

lookEqAddition :: Ct -> SDoc
lookEqAddition ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> "EqPred NomEq" <+> lookAddition t1 <+> lookAddition t2
	_ -> "Other than (EqPred NomEq _ _)" <+> ppr ct

lookAddition :: Type -> SDoc
lookAddition (TyConApp tc xs) | tc == typeNatAddTyCon = "(+)" <+> ppr xs
lookAddition ct = "Not Addition" <+> ppr ct

filterByTypes :: (Type -> Bool) -> [Ct] -> [Ct]
filterByTypes f = filter (maybe False (uncurry (||)) . applyTypes f)

applyTypes :: (Type -> a) -> Ct -> Maybe (a, a)
applyTypes f ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just (f t1, f t2)
	_ -> Nothing

isPlusOneOfTypesMulti :: [Ct] -> Type -> Bool
isPlusOneOfTypesMulti cts t = any (flip isPlusOneOfTypes t) cts

isPlusOneOfTypes :: Ct -> Type -> Bool
isPlusOneOfTypes ct t = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> t `isPlusOneOf` t1 || t `isPlusOneOf` t2
	_ -> False

isPlusOneOf :: Type -> Type -> Bool
TyConApp tc xs `isPlusOneOf` (TyVarTy t) | tc == typeNatAddTyCon = case xs of
	[TyVarTy t1, LitTy (NumTyLit 1)] -> t1 == t
	[LitTy (NumTyLit 1), TyVarTy t2] -> t2 == t
	_ -> False
_ `isPlusOneOf` _ = False

hasPlusOne :: Type -> Ct -> Bool
hasPlusOne t ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> t1 `isPlusOneOf` t || t2 `isPlusOneOf` t
	_ -> False

doesExistPlusOne :: Ct -> [Ct] -> Bool
doesExistPlusOne ct given = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> any (t1 `hasPlusOne`) given && any (t2 `hasPlusOne`) given
	_ -> False

toPlusOne :: Type -> Type
toPlusOne t = TyConApp typeNatAddTyCon [t, LitTy $ NumTyLit 1]

-- toPlusOneCt :: [Ct] -> Ct -> Maybe Ct
-- toPlusOneCt given ct = 

-- mkNonCanonical :: CtEvidence -> Ct
-- CtWanted pred_type (HoleDest hole) WDeriv
--
-- newCoercionHole :: PredType -> TcPluginM CoercionHole
-- mkPrimEqPred

mkCtFromPredType :: Ct -> PredType -> TcPluginM Ct
mkCtFromPredType ct pt = do
	hole <- newCoercionHole pt
	return . mkNonCanonical $ CtWanted pt (HoleDest hole) WDeriv (ctLoc ct)

myMkPrimEqPred :: Type -> Type -> PredType
myMkPrimEqPred = mkPrimEqPred

toPlusOneCt :: Ct -> TcPluginM (Maybe Ct)
toPlusOneCt ct = case classifyPredType .ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 ->do
		let	pt = mkPrimEqPred (toPlusOne t1) (toPlusOne t2)
		Just <$> mkCtFromPredType ct pt
	_ -> return Nothing
