{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module PlusOne.Plugin where

import GhcPlugins

import TcPluginM (TcPluginM, tcPluginTrace)
import TcRnTypes (Ct, TcPlugin(..), TcPluginResult(..))

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const $ Just plusOnePlugin }

plusOnePlugin :: TcPlugin
plusOnePlugin = TcPlugin {
	tcPluginInit = return (),
	tcPluginSolve = const $ solvePlusOne,
	tcPluginStop = const $ return () }

solvePlusOne :: [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solvePlusOne _ _ [] = do
	tcPluginTrace "PlusOne.Plugin: " "wanted: empty"
	return $ TcPluginOk [] []
solvePlusOne given _ wanted = do
	tcPluginTrace "PlusOne.Plugin: " "foo"
	return $ TcPluginOk [] []
