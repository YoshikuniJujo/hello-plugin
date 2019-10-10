{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module DoNothing.Plugin (plugin) where

import GhcPlugins

plugin :: Plugin
plugin = defaultPlugin {
	installCoreToDos = install
	}

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install opt todo = do
	putMsgS `mapM_` opt
	putMsgS "Hello!"
	return todo
