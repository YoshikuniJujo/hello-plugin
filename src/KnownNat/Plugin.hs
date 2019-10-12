{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module KnownNat.Plugin where

import GhcPlugins

import TcPluginM
import TcRnTypes
import TcEvidence

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const . Just $ TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const $ solveKnownNat,
	tcPluginStop = const $ return () } }

solveKnownNat :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveKnownNat _ _ [] = do
	tcPluginTrace "!KnownNat.Plugin: empty [W]anted" ""
	return $ TcPluginOk [] []
solveKnownNat gs _ ws = do
	tcPluginTrace "!KnownNat.Plugin:" ""
	tcPluginTrace "[G]iven: " $ ppr gs
	tcPluginTrace "[W]nted: " $ ppr ws
--	return $ TcPluginContradiction gs
	return $ TcPluginOk [] []

-- constraintToEvTerm :: KnownNatDefs -> [(CType, EvExpr)] -> KnConstraint -> TcPluginM (Maybe ((EvTerm, Ct), [Ct]))
-- constraintToEvTerm = undefined
