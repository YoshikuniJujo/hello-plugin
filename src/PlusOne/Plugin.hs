{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PlusOne.Plugin (plugin) where

import GHC.TcPluginM.Extra (evByFiat)
import Control.Arrow ((&&&))
import Data.Maybe (fromJust, catMaybes)

import GhcPlugins (
	Plugin(..), defaultPlugin,
	PredTree(..), EqRel(..), classifyPredType, PredType, mkPrimEqPred,
	ppr, ($$) )
import TcPluginM (TcPluginM, tcPluginTrace, newCoercionHole)
import TcRnTypes (
	TcPlugin(..), TcPluginResult(..),
	Ct, ctEvPred, ctEvidence, ctLoc,
	mkNonCanonical, CtEvidence(..), TcEvDest(..), ShadowInfo(WDeriv) )
import TcEvidence (EvTerm)
import TyCoRep (Type(..), TyLit(..))
import TcTypeNats (typeNatAddTyCon)

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const $ solvePlusOne,
	tcPluginStop = const $ return () } }

solvePlusOne :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solvePlusOne _ _ [] = return $ TcPluginOk [] []
solvePlusOne gs _ ws = do
	tcPluginTrace "!PlusOne.Plugin: plugin" $ ppr gs $$ ppr ws
	(ok, new) <- unzip . catMaybes <$> checkPlusOne gs `mapM` ws
	return $ TcPluginOk ((fromJust . evMagic &&& id) <$> ok) new

checkPlusOne :: [Ct] -> Ct -> TcPluginM (Maybe (Ct, Ct))
checkPlusOne gs w =
	if elemPlusOne w gs then ((w ,) <$>) <$> plusOne w else return Nothing

elemPlusOne :: Ct -> [Ct] -> Bool
elemPlusOne ct gs = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 ->
		any (t1 `hasPlusOne`) gs && any (t2 `hasPlusOne`) gs
	_ -> False

hasPlusOne :: Type -> Ct -> Bool
hasPlusOne t ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> t1 `isPlusOneOf` t || t2 `isPlusOneOf` t
	_ -> False

isPlusOneOf :: Type -> Type -> Bool
TyConApp tc xs `isPlusOneOf` (TyVarTy t) | tc == typeNatAddTyCon = case xs of
	[TyVarTy t1, LitTy (NumTyLit 1)] -> t1 == t
--	[LitTy (NumTyLit 1), TyVarTy t2] -> t2 == t
	_ -> False
_ `isPlusOneOf` _ = False

plusOne :: Ct -> TcPluginM (Maybe Ct)
plusOne ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 ->
		Just <$> mkCtFromPredType ct (mkPrimEqPred (po t1) (po t2))
	_ -> return Nothing
	where po t = TyConApp typeNatAddTyCon [t, LitTy $ NumTyLit 1]

mkCtFromPredType :: Ct -> PredType -> TcPluginM Ct
mkCtFromPredType ct pt = do
	hole <- newCoercionHole pt
	return . mkNonCanonical $ CtWanted pt (HoleDest hole) WDeriv (ctLoc ct)

evMagic :: Ct -> Maybe EvTerm
evMagic ct = case classifyPredType . ctEvPred $ ctEvidence ct of
	EqPred NomEq t1 t2 -> Just $ evByFiat "hello-plugin" t1 t2
	_ -> Nothing
