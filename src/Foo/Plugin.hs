{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo.Plugin where

import Data.Maybe

import GhcPlugins
import TcPluginM
import TcRnTypes
import TcEvidence
import TyCoRep

import Literal

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const $ solveFoo,
	tcPluginStop = const $ return () } }

solveFoo :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveFoo _ _ [] = return $ TcPluginOk [] []
solveFoo gs _ ws = do
	tcPluginTrace "!Foo.Plugin: " $ ppr gs $$ ppr ws
	tcPluginTrace "lookBar: " . cat . catMaybes $ lookBar <$> ws
	tcPluginTrace "lookBaz: " . cat . catMaybes $ lookBaz <$> ws
	let	ok = catMaybes $ bazOk <$> ws
	return $ TcPluginOk ok []

lookBar :: Ct -> Maybe SDoc
lookBar (CDictCan ev cl [LitTy (NumTyLit cta)] cpsc) = Just
	$ "CDictCan" $$
		"cc_ev =" <+> ppr ev $$
		"cc_class =" <+> ppr cl $$
		"cc_tyargs =" <+> ppr cta $$
		"cc_pend_sc =" <+> ppr cpsc
lookBar _ = Nothing

barOk :: Ct -> Maybe (EvTerm, Ct)
-- barOk ct@(CDictCan _ev _cl [LitTy (NumTyLit cta)] _cpsc) = Just (EvExpr $ Lit $ LitNumber LitNumNatural cta $ TyConApp [],  ct)
barOk _ = Nothing

-- natType :: Type
-- natType = mkPrimTyCon 'Nat [] []


lookBaz :: Ct -> Maybe SDoc
lookBaz (CDictCan ev cl [LitTy (StrTyLit x)] cpsc) = Just
	$ "CDictCan" $$
		"cc_ev =" <+> ppr ev $$
		"cc_class =" <+> ppr cl $$
		"cc_tyargs =" <+> ppr x $$
		"cc_pend_sc =" <+> ppr cpsc
lookBaz _ = Nothing

bazOk :: Ct -> Maybe (EvTerm, Ct)
bazOk ct@(CDictCan ev cl [LitTy (StrTyLit x)] cpsc)
	| x == "hello" = Just (EvExpr $ Lit $ LitLabel x Nothing IsData, ct)
bazOk _ = Nothing
