{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Foo.Plugin where

import Control.Monad
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
	let	ok = catMaybes $ barBazOk <$> ws
	return $ TcPluginOk ok []

barBazOk :: Ct -> Maybe (EvTerm, Ct)
barBazOk ct = barOk ct `mplus` bazOk ct

lookBar :: Ct -> Maybe SDoc
lookBar (CDictCan ev cl [LitTy (NumTyLit cta)] cpsc) = Just
	$ "CDictCan" $$
		"cc_ev =" <+> ppr ev $$
		"cc_class =" <+> ppr cl $$
		"cc_tyargs =" <+> ppr cta $$
		"cc_pend_sc =" <+> ppr cpsc
lookBar _ = Nothing

barOk :: Ct -> Maybe (EvTerm, Ct)
barOk ct@(CDictCan _ev _cl [LitTy (NumTyLit cta)] _cpsc) = Just (EvExpr $ Type $ LitTy $ NumTyLit cta, ct)
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
--	| x == "hello" = Just (EvExpr $ Lit $ LitLabel x Nothing IsData, ct)
--	| x == "hello" = Just (EvExpr $ Lit $ LitLabel "booboo" Nothing IsData, ct)
--	| x == "hello" = Just (EvExpr $ Lit $ LitLabel undefined Nothing IsData, ct)
--	| x == "hello" = Just (EvExpr $ Lit undefined, ct)
--	| x == "hello" = Just (EvExpr undefined, ct)
	| x == "hello" = Just (EvExpr . Type . LitTy $ StrTyLit "hello", ct)
bazOk _ = Nothing
